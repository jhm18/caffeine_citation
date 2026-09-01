#	Tie Strategy Test Harness
#	Jonathan H. Morgan
#   1 September 2026

##########################
#####     Config      ####
##########################

#	Paths (edit these)
	PROJECT_DIR   <- "/workspace/caffeine_citation"
	NODE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_node_list_3Oct2024.Rda")
	EDGE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_edge_list_21Mar2024.Rda")
	OUT_DIR       <- file.path(PROJECT_DIR, "data/tie_tests")

#	Era pair (t-1 = cited, t = citer)
	ERA_PRIOR <- 22
	ERA_CURR  <- 23

#	Era network and partition files
	NET_PRIOR <- file.path(PROJECT_DIR, "pajek_files/Era22/era22.net")
	CLU_PRIOR <- file.path(PROJECT_DIR, "pajek_files/Era22/era22_testCommunity.clu")
	NET_CURR  <- file.path(PROJECT_DIR, "pajek_files/Era23/era23.net")
	CLU_CURR  <- file.path(PROJECT_DIR, "pajek_files/Era23/era23_testCommunity.clu")

#	Options
	options(stringsAsFactors = FALSE)
	options(scipen = 999)

#	Packages
	suppressPackageStartupMessages(library(dplyr))

#	Validation
	if (utils::packageVersion("dplyr") < "1.1.0") {
		stop("dplyr >= 1.1.0 required for the `relationship` and `.by` arguments; found ",
			 utils::packageVersion("dplyr"))
	}

####################
####  Functions  ####
####################

#	Parse Pajek Vertices
	read_pajek_vertices <- function(net_path) {
		#	"""
		#	Args:
		#		net_path: path to a Pajek .net file
		#	Returns:
		#		data.frame(pajek_id, node_id) parsed from the *Vertices block
		#	Notes:
		#		Reads only the vertex block. Vertex labels in this project are node_ids.
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

#	Read Pajek Partition
	read_pajek_partition <- function(clu_path) {
		#	"""
		#	Args:
		#		clu_path: path to a Pajek .clu partition file
		#	Returns:
		#		integer vector of community assignments, one per vertex
		#	"""

		#	Validation
			if (!file.exists(clu_path)) stop("Missing .clu file: ", clu_path)

		#	Read and drop the header
			raw <- readLines(clu_path)
			return(as.integer(raw[-1L]))
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
		#		tibble(node_label, community) for this era, one row per label
		#	Notes:
		#		Pure equivalent of id_map(). Asserts the vertex/partition alignment
		#		the original assumes silently. Keyed on node_label because
		#		community_membership() joins on labels, not ids.
		#	"""

		#	Read network and partition
			vertices  <- read_pajek_vertices(net_path)
			partition <- read_pajek_partition(clu_path)

		#	Validation
			if (nrow(vertices) != length(partition)) {
				stop("Vertex/partition length mismatch for era ", era, ": ",
					 nrow(vertices), " vs ", length(partition))
			}

		#	Attach community
			vertices$community <- partition

		#	Join to the era slice of the node list
			era_map <- node_list |>
				filter(era == !!era) |>
				select(node_id, node_label) |>
				left_join(select(vertices, node_id, community),
						  by = "node_id", relationship = "many-to-one") |>
				filter(!is.na(community)) |>
				select(node_label, community)

		#	Report duplicate labels rather than letting a join fan out
			n_dupe <- sum(duplicated(era_map$node_label))
			if (n_dupe > 0L) {
				message("  NOTE era ", era, ": ", n_dupe,
						" duplicate node_labels; keeping first occurrence of each.")
				era_map <- distinct(era_map, node_label, .keep_all = TRUE)
			}

		#	Assembling result
			return(era_map)
	}

#	Normalize Arc Weights
	normalize_arcs <- function(arcs, value_col) {
		#	"""
		#	Args:
		#		arcs: data.frame with sender_community, target_community, value_col
		#		value_col: name of the column to row-normalize
		#	Returns:
		#		arcs with a `proportion` column summing to 1 within sender_community
		#	Notes:
		#		Matches community_membership(): normalized out of the citer (era t)
		#		community, referencing backwards.
		#	"""

		#	Assembling result
			return(mutate(arcs, proportion = .data[[value_col]] / sum(.data[[value_col]]),
						  .by = sender_community))
	}

##########################
#####     Import      ####
##########################

	cat("Loading node list...\n")
	nl_env <- new.env(); load(NODE_LIST_RDA, envir = nl_env)
	node_list <- get(ls(nl_env)[1], envir = nl_env)

	cat("Loading edge list...\n")
	el_env <- new.env(); load(EDGE_LIST_RDA, envir = el_env)
	edges <- get(ls(el_env)[1], envir = el_env)

	cat("  node_list:", nrow(node_list), "rows;  edges:", nrow(edges), "rows\n")
	cat("  edge columns:", paste(colnames(edges), collapse = ", "), "\n")

#	Sender-era lookup must be one row per node_id or every count inflates
	sender_era <- node_list |>
		select(sender_id = node_id, sender_era = era)
	n_dupe_nodes <- sum(duplicated(sender_era$sender_id))
	cat("  duplicate node_id in node_list:", n_dupe_nodes, "\n\n")
	if (n_dupe_nodes > 0L) {
		message("  NOTE keeping first era per node_id.")
		sender_era <- distinct(sender_era, sender_id, .keep_all = TRUE)
	}

##############################
####   Build Era Maps     ####
##############################

	cat("Building era maps...\n")
	map_prior <- build_era_map(node_list, NET_PRIOR, CLU_PRIOR, ERA_PRIOR)
	map_curr  <- build_era_map(node_list, NET_CURR,  CLU_CURR,  ERA_CURR)
	cat("  era", ERA_PRIOR, ":", nrow(map_prior), "nodes,",
		n_distinct(map_prior$community), "communities\n")
	cat("  era", ERA_CURR,  ":", nrow(map_curr),  "nodes,",
		n_distinct(map_curr$community),  "communities\n\n")

##############################
####   Build Work Set     ####
##############################

#	Attach sender era (left_join preserves edge-list order, which strategy A needs)
	cit <- left_join(edges, sender_era, by = "sender_id", relationship = "many-to-one")

#	Split by sender era
	cit_prior <- filter(cit, !is.na(sender_era), sender_era == ERA_PRIOR)
	cit_curr  <- filter(cit, !is.na(sender_era), sender_era == ERA_CURR)
	cat("Citation edges by sender era:  era", ERA_PRIOR, "=", nrow(cit_prior),
		";  era", ERA_CURR, "=", nrow(cit_curr), "\n")

#	Intersection filter, then attach communities by label
	shared_targets <- unique(cit_prior$target_id)
	work <- cit_curr |>
		filter(target_id %in% shared_targets) |>
		left_join(rename(map_curr,  sender_label = node_label, sender_community = community),
				  by = "sender_label", relationship = "many-to-one") |>
		left_join(rename(map_prior, target_label = node_label, target_community = community),
				  by = "target_label", relationship = "many-to-one") |>
		filter(!is.na(sender_community), !is.na(target_community))

	cat("  era", ERA_CURR, "edges whose target is also cited in era", ERA_PRIOR, ":",
		sum(cit_curr$target_id %in% shared_targets), "\n")
	cat("  after both community joins:", nrow(work), "edges\n\n")

##################################
####   Diagnostic: Dedup      ####
##################################

	cat("=== Is the global target_id dedup lossy in practice? ===\n")

	per_target <- work |>
		summarise(n_citing_articles    = n_distinct(sender_id),
				  n_citing_communities = n_distinct(sender_community),
				  .by = target_id)

	cat("  distinct cited targets:", nrow(per_target), "\n")
	cat("  citing articles per target    -- mean",
		round(mean(per_target$n_citing_articles), 2),
		" median", median(per_target$n_citing_articles),
		" max", max(per_target$n_citing_articles), "\n")
	cat("  citing COMMUNITIES per target -- mean",
		round(mean(per_target$n_citing_communities), 2),
		" median", median(per_target$n_citing_communities),
		" max", max(per_target$n_citing_communities), "\n")
	cat("  targets cited by exactly 1 community:",
		sum(per_target$n_citing_communities == 1),
		"(", round(100 * mean(per_target$n_citing_communities == 1), 1), "% )\n")
	cat("  targets cited by >1 community      :",
		sum(per_target$n_citing_communities > 1),
		"(", round(100 * mean(per_target$n_citing_communities > 1), 1), "% )\n")
	cat("  --> community-tie observations lost to the dedup:",
		sum(per_target$n_citing_communities) - nrow(per_target), "\n\n")

##########################################
####   One Pass Serves Three Measures ####
##########################################

#	Citations from each citer community to each individual cited article
	article_cell <- count(work, sender_community, target_community, target_id,
						  name = "n_cites")

#	Strategy B: breadth -- distinct cited articles per community pair
	arcs_b <- article_cell |>
		summarise(n_unique = n(), .by = c(sender_community, target_community)) |>
		normalize_arcs("n_unique")

#	Strategy C: intensity -- citation events per community pair
	arcs_c <- article_cell |>
		summarise(n_citations = sum(n_cites), .by = c(sender_community, target_community)) |>
		normalize_arcs("n_citations")

#	Strategy D: concentration -- Herfindahl over per-article shares within a cell
	arcs_d <- article_cell |>
		mutate(share = n_cites / sum(n_cites), .by = c(sender_community, target_community)) |>
		summarise(hhi = sum(share^2), .by = c(sender_community, target_community))

#	Strategy A: current -- global dedup on target_id, first row wins
	arcs_a <- work |>
		distinct(target_id, .keep_all = TRUE) |>
		summarise(count = n(), .by = c(sender_community, target_community)) |>
		normalize_arcs("count")

##############################
####    Comparison        ####
##############################

	cat("=== Arc counts by strategy ===\n")
	cat("  A current (global dedup) :", nrow(arcs_a), "arcs\n")
	cat("  B breadth (unique/citer) :", nrow(arcs_b), "arcs\n")
	cat("  C intensity (all cites)  :", nrow(arcs_c), "arcs\n")
	cat("  arcs B has that A lacks  :", nrow(arcs_b) - nrow(arcs_a), "\n\n")

	cmp <- arcs_b |>
		select(sender_community, target_community, p_breadth = proportion) |>
		left_join(select(arcs_c, sender_community, target_community, p_intensity = proportion),
				  by = c("sender_community", "target_community"), relationship = "one-to-one") |>
		left_join(arcs_d,
				  by = c("sender_community", "target_community"), relationship = "one-to-one") |>
		left_join(select(arcs_a, sender_community, target_community, p_current = proportion),
				  by = c("sender_community", "target_community"), relationship = "one-to-one")

	shared <- filter(cmp, !is.na(p_current))

	cat("=== Weight agreement ===\n")
	cat("  breadth vs intensity  -- pearson",
		round(cor(cmp$p_breadth, cmp$p_intensity), 3),
		" spearman", round(cor(cmp$p_breadth, cmp$p_intensity, method = "spearman"), 3),
		" (", nrow(cmp), "arcs )\n")
	cat("  current vs breadth    -- pearson",
		round(cor(shared$p_current, shared$p_breadth), 3),
		" spearman", round(cor(shared$p_current, shared$p_breadth, method = "spearman"), 3),
		" (", nrow(shared), "shared arcs )\n\n")

	cat("=== Concentration (HHI) across arcs ===\n")
	print(round(quantile(cmp$hhi, c(0, .25, .5, .75, .9, 1)), 3))
	cat("  arcs running through a single article (hhi == 1):",
		sum(cmp$hhi == 1), "(", round(100 * mean(cmp$hhi == 1), 1), "% )\n\n")

	cat("=== Largest breadth/intensity divergences ===\n")
	cmp |>
		mutate(delta = p_intensity - p_breadth) |>
		arrange(desc(abs(delta))) |>
		slice_head(n = 15) |>
		mutate(across(c(p_breadth, p_intensity, delta, hhi), \(x) round(x, 3))) |>
		as.data.frame() |>
		print(row.names = FALSE)

##############################
####    Write Outputs     ####
##############################

	dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

	out <- arcs_b |>
		select(sender_community, target_community, n_unique, proportion_breadth = proportion) |>
		left_join(select(arcs_c, sender_community, target_community,
						 n_citations, proportion_intensity = proportion),
				  by = c("sender_community", "target_community"), relationship = "one-to-one") |>
		left_join(arcs_d,
				  by = c("sender_community", "target_community"), relationship = "one-to-one") |>
		left_join(select(arcs_a, sender_community, target_community,
						 proportion_current = proportion),
				  by = c("sender_community", "target_community"), relationship = "one-to-one")

	arc_file <- file.path(OUT_DIR, sprintf("arc_strategies_%d_%d.csv", ERA_PRIOR, ERA_CURR))
	readr::write_csv(out, arc_file)

	diag_file <- file.path(OUT_DIR, sprintf("target_diagnostics_%d_%d.csv", ERA_PRIOR, ERA_CURR))
	readr::write_csv(per_target, diag_file)

	cat("\nWrote:\n  ", arc_file, "\n  ", diag_file, "\n")
