#	Tie Layer Test Harness
#	Jonathan H. Morgan
#	1 September 2026
#	Settles the tie construction before any ranking or labeling work.
#	Runs every configured era pair through the same funnel and reports:
#	  1. where candidate ties are lost (intersection filter vs community lookup)
#	  2. how much of each era's community set participates in the graph
#	  3. what the dedup costs in arcs and in weight
#	  4. how thin the evidence behind each arc is

##############
#   CONFIG   #
##############

#	Paths
	PROJECT_DIR   <- "/workspace/caffeine_citation"
	NODE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_node_list_3Oct2024.Rda")
	EDGE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_edge_list_21Mar2024.Rda")
	PAJEK_DIR     <- file.path(PROJECT_DIR, "pajek_files")
	OUT_DIR       <- file.path(PROJECT_DIR, "data/tie_tests")

#	Era pairs to test, as list(prior = t-1, curr = t)
	ERA_PAIRS <- list(list(prior = 21, curr = 22),
					  list(prior = 22, curr = 23))

#	Options
	options(stringsAsFactors = FALSE)
	options(scipen = 999)

#	Validation
	if (utils::packageVersion("dplyr") < "1.1.0") {
		stop("dplyr >= 1.1.0 required for left_join(relationship = ); found ",
			 utils::packageVersion("dplyr"))
	}

#################
#   FUNCTIONS   #
#################

#	Resolve Era File Paths
	resolve_era_paths <- function(pajek_dir, era) {
		#	"""
		#	Args:
		#		pajek_dir: root pajek_files directory
		#		era: integer era id
		#	Returns:
		#		list(net, clu) of resolved absolute paths
		#	Notes:
		#		Era folders are inconsistent across the project. Eras 1-17 keep
		#		.net inside Era<N>/ but .clu at the root; 18-21 keep both at the
		#		root; 22-23 keep both inside Era<N>/. Partition files are named
		#		era<N>_testCommunity.clu or era<N>_Community.clu depending on era.
		#		This is the normalization layer the full sweep will need.
		#	"""

		#	Candidate network locations
			net_candidates <- c(file.path(pajek_dir, paste0("Era", era), paste0("era", era, ".net")),
								file.path(pajek_dir, paste0("era", era, ".net")))

		#	Candidate partition locations and names
			clu_names <- c(paste0("era", era, "_testCommunity.clu"),
						   paste0("era", era, "_Community.clu"))
			clu_candidates <- c(file.path(pajek_dir, paste0("Era", era), clu_names),
								file.path(pajek_dir, clu_names))

		#	Select the first that exists
			net <- net_candidates[file.exists(net_candidates)]
			clu <- clu_candidates[file.exists(clu_candidates)]

		#	Validation
			if (length(net) == 0L) stop("No .net found for era ", era, "; tried:\n  ",
										paste(net_candidates, collapse = "\n  "))
			if (length(clu) == 0L) stop("No .clu found for era ", era, "; tried:\n  ",
										paste(clu_candidates, collapse = "\n  "))

		#	Assembling result
			return(list(net = net[[1L]], clu = clu[[1L]]))
	}

#	Parse Pajek Vertices
	read_pajek_vertices <- function(net_path) {
		#	"""
		#	Args:
		#		net_path: path to a Pajek .net file
		#	Returns:
		#		data.frame(pajek_id, node_id) parsed from the *Vertices block
		#	Notes:
		#		Reads only the vertex block. Vertex labels here are node_ids.
		#	"""

		#	Validation
			if (!file.exists(net_path)) stop("Missing .net file: ", net_path)

		#	Read the header
			con <- file(net_path, "r")
			on.exit(close(con))
			header <- readLines(con, n = 1L)
			n_vertices <- as.integer(sub("^\\*Vertices\\s+", "", header, ignore.case = TRUE))
			if (is.na(n_vertices)) stop("Could not parse *Vertices header in ", net_path)

		#	Read the vertex lines
			raw <- readLines(con, n = n_vertices)

		#	Extract id and quoted label
			pajek_id <- as.integer(sub("^\\s*(\\d+)\\s.*$", "\\1", raw))
			node_id  <- as.numeric(sub('^\\s*\\d+\\s+"([^"]*)".*$', "\\1", raw))

		#	Validation
			if (anyNA(pajek_id) || anyNA(node_id)) stop("Vertex parse produced NAs in ", net_path)

		#	Assembling result
			return(data.frame(pajek_id = pajek_id, node_id = node_id))
	}

#	Build Era Community Map
	build_era_map <- function(node_list, net_path, clu_path, era) {
		#	"""
		#	Args:
		#		node_list: full citation node list
		#		net_path: era .net file
		#		clu_path: era .clu partition file
		#		era: integer era id
		#	Returns:
		#		data.frame(node_label, community), one row per label
		#	Notes:
		#		Pure equivalent of id_map(), with the vertex/partition alignment
		#		asserted rather than assumed.
		#	"""

		#	Read network and partition
			vertices  <- read_pajek_vertices(net_path)
			partition <- as.integer(readLines(clu_path)[-1L])

		#	Validation
			if (nrow(vertices) != length(partition)) {
				stop("Vertex/partition length mismatch for era ", era, ": ",
					 nrow(vertices), " vertices vs ", length(partition), " partition entries")
			}

		#	Attach community
			vertices$community <- partition

		#	Join the era slice of the node list to the partition
			era_nodes <- node_list[node_list$era == era, c("node_id", "node_label")]
			era_map <- dplyr::left_join(era_nodes, vertices[c("node_id", "community")],
										by = "node_id", relationship = "many-to-one")

		#	Report coverage before dropping
			n_unmatched <- sum(is.na(era_map$community))
			if (n_unmatched > 0L) {
				message("  NOTE era ", era, ": ", n_unmatched,
						" node_list rows had no vertex in the network.")
			}
			era_map <- era_map[!is.na(era_map$community), c("node_label", "community")]

		#	Report duplicate labels rather than letting a later join fan out
			n_dupe <- sum(duplicated(era_map$node_label))
			if (n_dupe > 0L) {
				message("  NOTE era ", era, ": ", n_dupe,
						" duplicate node_labels; keeping first occurrence of each.")
				era_map <- era_map[!duplicated(era_map$node_label), ]
			}

		#	Assembling result
			return(era_map)
	}

#	Normalize Arc Weights
	normalize_arcs <- function(arcs, value_col) {
		#	"""
		#	Args:
		#		arcs: data.frame with sender_community, target_community, value_col
		#		value_col: column to row-normalize
		#	Returns:
		#		arcs with a `proportion` column summing to 1 within sender_community
		#	Notes:
		#		Normalized out of the citer (era t) community, referencing backwards.
		#	"""

		#	Validation
			if (!value_col %in% colnames(arcs)) stop("Missing column: ", value_col)

		#	Row-normalize within sender
			arcs$proportion <- ave(arcs[[value_col]], arcs$sender_community,
								   FUN = function(x) x / sum(x))

		#	Assembling result
			return(arcs)
	}

#	Analyze One Era Pair
	analyze_pair <- function(node_list, edges, pajek_dir, era_prior, era_curr) {
		#	"""
		#	Args:
		#		node_list: full citation node list
		#		edges: full citation edge list
		#		pajek_dir: root pajek_files directory
		#		era_prior: integer era t-1 (cited)
		#		era_curr: integer era t (citer)
		#	Returns:
		#		list(arcs, per_target, summary) for this pair
		#	Notes:
		#		Reports the full attrition funnel so the cost of the intersection
		#		filter can be separated from the cost of the community lookup.
		#	"""

		cat("\n############################################\n")
		cat("###   ERA PAIR ", era_curr, " -> ", era_prior, "\n", sep = "")
		cat("############################################\n")

		#	Resolve paths
			p_prior <- resolve_era_paths(pajek_dir, era_prior)
			p_curr  <- resolve_era_paths(pajek_dir, era_curr)
			cat("  net(t-1): ", p_prior$net, "\n  clu(t-1): ", p_prior$clu, "\n", sep = "")
			cat("  net(t)  : ", p_curr$net,  "\n  clu(t)  : ", p_curr$clu,  "\n", sep = "")

		#	Build era maps
			map_prior <- build_era_map(node_list, p_prior$net, p_prior$clu, era_prior)
			map_curr  <- build_era_map(node_list, p_curr$net,  p_curr$clu,  era_curr)
			k_prior <- length(unique(map_prior$community))
			k_curr  <- length(unique(map_curr$community))
			cat("  era ", era_prior, ": ", nrow(map_prior), " nodes, ", k_prior, " communities\n", sep = "")
			cat("  era ", era_curr,  ": ", nrow(map_curr),  " nodes, ", k_curr,  " communities\n", sep = "")

		#	Attach sender era (left_join preserves edge order, which strategy A needs)
			sender_era <- data.frame(sender_id = node_list$node_id, sender_era = node_list$era)
			sender_era <- sender_era[!duplicated(sender_era$sender_id), ]
			cit <- dplyr::left_join(edges, sender_era, by = "sender_id",
									relationship = "many-to-one")

		#	Split by sender era
			cit_prior <- cit[!is.na(cit$sender_era) & cit$sender_era == era_prior, ]
			cit_curr  <- cit[!is.na(cit$sender_era) & cit$sender_era == era_curr,  ]

		#	THE FUNNEL
			cat("\n  --- attrition funnel ---\n")
			cat("  citation edges sent by era ", era_curr,  ": ", nrow(cit_curr),  "\n", sep = "")
			cat("  citation edges sent by era ", era_prior, ": ", nrow(cit_prior), "\n", sep = "")

			shared_targets <- unique(cit_prior$target_id)
			work <- cit_curr[cit_curr$target_id %in% shared_targets, ]
			cat("  surviving intersection filter    : ", nrow(work),
				"  (", round(100 * nrow(work) / max(nrow(cit_curr), 1), 1), "% of era ",
				era_curr, " edges )\n", sep = "")

		#	Attach communities by label
			sender_map <- data.frame(sender_label = map_curr$node_label,
									 sender_community = map_curr$community)
			target_map <- data.frame(target_label = map_prior$node_label,
									 target_community = map_prior$community)
			work <- dplyr::left_join(work, sender_map, by = "sender_label",
									 relationship = "many-to-one")
			work <- dplyr::left_join(work, target_map, by = "target_label",
									 relationship = "many-to-one")

			cat("    lost on sender community lookup: ", sum(is.na(work$sender_community)), "\n", sep = "")
			cat("    lost on target community lookup: ", sum(is.na(work$target_community)), "\n", sep = "")

			work <- work[!is.na(work$sender_community) & !is.na(work$target_community), ]
			cat("  final working edge set           : ", nrow(work), "\n", sep = "")

		#	Early return when the pair yields nothing
			if (nrow(work) == 0L) {
				cat("  !! no surviving edges; skipping pair\n")
				return(NULL)
			}

		#	Per-target diagnostics
			n_articles <- tapply(work$sender_id,        work$target_id, function(x) length(unique(x)))
			n_comms    <- tapply(work$sender_community, work$target_id, function(x) length(unique(x)))
			per_target <- data.frame(target_id            = names(n_articles),
									 n_citing_articles    = as.integer(n_articles),
									 n_citing_communities = as.integer(n_comms))

			cat("\n  --- dedup cost ---\n")
			cat("  distinct cited targets            : ", nrow(per_target), "\n", sep = "")
			cat("  cited by exactly 1 community      : ", sum(per_target$n_citing_communities == 1),
				" (", round(100 * mean(per_target$n_citing_communities == 1), 1), "% )\n", sep = "")
			cat("  community-tie observations lost   : ",
				sum(per_target$n_citing_communities) - nrow(per_target), "\n", sep = "")

		#	One pass serves every measure
			article_cell <- aggregate(list(n_cites = work$target_id),
									  by = list(sender_community = work$sender_community,
												target_community = work$target_community,
												target_id        = work$target_id),
									  FUN = length)

			arcs_b <- aggregate(list(n_unique = article_cell$target_id),
								by = list(sender_community = article_cell$sender_community,
										  target_community = article_cell$target_community),
								FUN = length)
			arcs_b <- normalize_arcs(arcs_b, "n_unique")

			arcs_c <- aggregate(list(n_citations = article_cell$n_cites),
								by = list(sender_community = article_cell$sender_community,
										  target_community = article_cell$target_community),
								FUN = sum)
			arcs_c <- normalize_arcs(arcs_c, "n_citations")

			article_cell$share_sq <- ave(article_cell$n_cites,
										 article_cell$sender_community,
										 article_cell$target_community,
										 FUN = function(x) x / sum(x))^2
			arcs_d <- aggregate(list(hhi = article_cell$share_sq),
								by = list(sender_community = article_cell$sender_community,
										  target_community = article_cell$target_community),
								FUN = sum)

			a_src  <- work[!duplicated(work$target_id), ]
			arcs_a <- aggregate(list(count = a_src$target_id),
								by = list(sender_community = a_src$sender_community,
										  target_community = a_src$target_community),
								FUN = length)
			arcs_a <- normalize_arcs(arcs_a, "count")

		#	Assemble the arc table
			arcs <- data.frame(sender_community   = arcs_b$sender_community,
							   target_community   = arcs_b$target_community,
							   n_unique           = arcs_b$n_unique,
							   proportion_breadth = arcs_b$proportion)
			arcs <- dplyr::left_join(arcs,
									 data.frame(sender_community     = arcs_c$sender_community,
												target_community     = arcs_c$target_community,
												n_citations          = arcs_c$n_citations,
												proportion_intensity = arcs_c$proportion),
									 by = c("sender_community", "target_community"),
									 relationship = "one-to-one")
			arcs <- dplyr::left_join(arcs, arcs_d,
									 by = c("sender_community", "target_community"),
									 relationship = "one-to-one")
			arcs <- dplyr::left_join(arcs,
									 data.frame(sender_community   = arcs_a$sender_community,
												target_community   = arcs_a$target_community,
												proportion_current = arcs_a$proportion),
									 by = c("sender_community", "target_community"),
									 relationship = "one-to-one")

		#	Coverage: how much of each era reaches the graph
			n_send <- length(unique(arcs$sender_community))
			n_targ <- length(unique(arcs$target_community))
			cat("\n  --- coverage (drives the LLM queue size) ---\n")
			cat("  era ", era_curr,  " communities in graph: ", n_send, " of ", k_curr,
				" (", round(100 * n_send / k_curr, 1), "% )\n", sep = "")
			cat("  era ", era_prior, " communities in graph: ", n_targ, " of ", k_prior,
				" (", round(100 * n_targ / k_prior, 1), "% )\n", sep = "")
			cat("  labels needed ", n_send + n_targ, " of ", k_curr + k_prior,
				" -> ", round(100 * (1 - (n_send + n_targ) / (k_curr + k_prior)), 1),
				"% of LLM work is currently discarded\n", sep = "")

		#	Structure and evidence depth
			tps <- table(arcs$sender_community)
			cat("\n  --- structure ---\n")
			cat("  arcs (breadth): ", nrow(arcs), "   arcs (current): ", nrow(arcs_a),
				"   dropped by dedup: ", nrow(arcs) - nrow(arcs_a), "\n", sep = "")
			cat("  ties per sender -- median ", median(as.integer(tps)),
				"  max ", max(as.integer(tps)), "\n", sep = "")
			cat("  senders with exactly 1 arc (proportion forced to 1.0): ", sum(tps == 1),
				" (", round(100 * mean(tps == 1), 1), "% )\n", sep = "")
			cat("  arcs resting on a single article: ", sum(arcs$n_unique == 1),
				" (", round(100 * mean(arcs$n_unique == 1), 1), "% )\n", sep = "")
			cat("  arcs with >= 5 articles         : ", sum(arcs$n_unique >= 5),
				" (", round(100 * mean(arcs$n_unique >= 5), 1), "% )\n", sep = "")

			shared <- arcs[!is.na(arcs$proportion_current), ]
			cat("  max |current - breadth| on shared arcs: ",
				round(max(abs(shared$proportion_current - shared$proportion_breadth)), 3), "\n", sep = "")

		#	Assembling result
			return(list(arcs = arcs, per_target = per_target,
						era_prior = era_prior, era_curr = era_curr))
	}

##############
#   IMPORT   #
##############

cat("Loading node list...\n")
nl_env <- new.env(); load(NODE_LIST_RDA, envir = nl_env)
node_list <- get(ls(nl_env)[1], envir = nl_env)

cat("Loading edge list...\n")
el_env <- new.env(); load(EDGE_LIST_RDA, envir = el_env)
edges <- get(ls(el_env)[1], envir = el_env)

cat("  node_list: ", nrow(node_list), " rows;  edges: ", nrow(edges), " rows\n", sep = "")
cat("  edge columns: ", paste(colnames(edges), collapse = ", "), "\n", sep = "")

#######################################
#   Doubled Edge List Investigation   #
#######################################

	cat("\n============================================\n")
	cat("===   IS THE EDGE LIST DUPLICATED?\n")
	cat("============================================\n")

#	Exact duplicate rows
	cat("  fully duplicated rows          : ", sum(duplicated(edges)), " of ", nrow(edges), "\n", sep = "")

#	Duplicate sender/target pairs
	pair_key <- paste(edges$sender_id, edges$target_id, sep = "_")
	mult <- table(table(pair_key))
	cat("  distinct sender/target pairs   : ", length(unique(pair_key)), "\n", sep = "")
	cat("  multiplicity distribution (how many times each pair appears):\n")
	print(mult)

#	If pairs repeat, do the repeated rows differ anywhere?
	dupe_rows <- edges[pair_key %in% names(which(table(pair_key) > 1)), ]
	if (nrow(dupe_rows) > 0L) {
		dupe_rows <- dupe_rows[order(dupe_rows$sender_id, dupe_rows$target_id), ]
		cat("\n  first 6 rows of a repeated pair group (do any columns differ?):\n")
		print(head(dupe_rows, 6))
	}

#################
#   RUN PAIRS   #
#################

#	Create & Specify a Directory
	dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

#	Iterate through the Pairs
	for (pair in ERA_PAIRS) {
		res <- analyze_pair(node_list, edges, PAJEK_DIR, pair$prior, pair$curr)

		#	Write outputs when the pair produced a graph
			if (!is.null(res)) {
				readr::write_csv(res$arcs,
					file.path(OUT_DIR, sprintf("arc_strategies_%d_%d.csv", res$era_prior, res$era_curr)))
				readr::write_csv(res$per_target,
					file.path(OUT_DIR, sprintf("target_diagnostics_%d_%d.csv", res$era_prior, res$era_curr)))
			}
	}

	cat("\nDone. Outputs in ", OUT_DIR, "\n", sep = "")
