#	Test_PathA_TieLayer.R -- Test Suite for Path A Stages 1 to 3
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	27 October 2026
#
#	WHAT THIS PROTECTS. The input, determinism, conservation, consistency, and
#	known-answer fixture tests of specification S14.4, for
#
#		PathA_PajekReaders.R    reading networks and partitions
#		PathA_EraResolution.R   resolving era files, building era maps
#		PathA_TieLayer.R        bridging works, descent ties, the risk set
#
#	The Pajek and labeling lanes are not exercised here; they arrive with their
#	own suite.
#
#	WHAT CHANGED FROM THE SEPTEMBER SUITE.
#
#	1.	THE FIXTURES ARE NEW, BECAUSE THE CONSTRUCTION IS. Eras are seven-year
#		windows and the analytic range is 11 to 17 -- six pairs, not eleven. The
#		September fixtures were measured on annual eras against development
#		partitions and none of them survives. Every fixture below is measured on the
#		rebuilt construction at the primary specification (S14.8) and is recorded
#		here with its source.
#
#	2.	THERE IS A COLLISION GUARD, AND IT IS THE POINT OF SECTION 3. The rebuilt
#		edge list already carries sender_era. A second join would produce
#		sender_era.x and sender_era.y, after which every era filter returns
#		logical(0) and every pair reports zero bridging works WITH NO ERROR RAISED.
#		Section 3 asserts that pa_attach_sender_era is idempotent and that exactly
#		one sender_era column survives. This is the test that would have caught the
#		same failure in the era construction's degree columns.
#
#	3.	THE VERTEX SET IS NO LONGER ASSERTED TO MATCH EXACTLY. An era network keeps
#		only nodes carrying a within-era arc, so a node the node list assigns to an
#		era by publication year but which emits and receives nothing inside that era
#		is absent from the network by design. The September suite asserted exactness;
#		this one reports the shortfall and asserts only that every VERTEX resolves,
#		which is the direction that matters -- a vertex we cannot identify is a
#		community we cannot attribute.
#
#	4.	THE RISK SET IS EXERCISED ON THE SMALLEST PAIR ONLY. Pair (11,12) is 108,770
#		rows. Pair (16,17) is 15.3 million, which belongs in a run and not in a test.
#
#	Run inside the container:
#		Rscript /workspace/caffeine_citation/tests/Test_PathA_TieLayer.R
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

##########################
#####     Config      ####
##########################

#	Setting Paths
	PROJECT_DIR   <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")
	SCRIPT_DIR    <- file.path(PROJECT_DIR, "scripts")
	ERA_DIR       <- file.path(PROJECT_DIR, "data/eras_7yr")
	NODE_LIST_RDA <- file.path(ERA_DIR, "pa_node_list.Rda")
	EDGE_LIST_RDA <- file.path(ERA_DIR, "pa_edge_list.Rda")

#	Corpus Fixtures
#	Measured by PathA_Revised_Eras.R, October 2026.
	FIX_NODES <- 589784L
	FIX_EDGES <- 1238288L

#	Era Fixtures at the Primary Specification
#	vertices and citers from era<NN>_nodes.Rda; communities from the alpha = 1,
#	anchor 120 partitions; bridge is the count of era-e works cited by both an
#	era-e and an era-(e+1) article. All measured by PathA_Granularity.R.
	FIX_ERAS <- data.frame(
		era         = c(   11L,    12L,    13L,    14L,    15L,    16L,    17L),
		year_start  = c( 1971L,  1978L,  1985L,  1992L,  1999L,  2006L,  2013L),
		year_end    = c( 1977L,  1984L,  1991L,  1998L,  2005L,  2012L,  2019L),
		vertices    = c( 2629L,  4268L, 10479L, 27380L, 32848L, 45508L, 76070L),
		citers      = c(  518L,   897L,  1627L,  4244L,  4626L,  5891L,  9080L),
		communities = c(   98L,   146L,   211L,   615L,   622L,   760L,  1105L),
		bridge      = c(  745L,  1329L,  4475L,  8332L,  9839L, 13855L,  9829L),
		stringsAsFactors = FALSE)

#	The Pair the Risk Set Is Exercised On
#	The smallest, at 745 x 146 = 108,770 rows.
	FIX_RISK_PAIR <- c(prior = 11L, curr = 12L)

#	Options
	options(stringsAsFactors = FALSE)
	options(scipen = 999)

##########################
#####    Harness      ####
##########################

#	Establishing Starting Values
	.tests_run <- 0L
	.tests_failed <- 0L

#	Assert a Condition
	check <- function(label, condition, detail = "") {
		#	"""
		#	Args:
		#		label: short description of what is being protected
		#		condition: single logical; NA counts as failure
		#		detail: extra context printed on failure
		#	Returns:
		#		invisible logical, TRUE when the check passed
		#	Notes:
		#		Records into the counters in the enclosing script rather than
		#		signalling, so one failure does not hide the rest of the suite.
		#	"""

		#	Record
			.tests_run <<- .tests_run + 1L
			ok <- isTRUE(condition)
			if (!ok) .tests_failed <<- .tests_failed + 1L

		#	Report
			cat(if (ok) "  PASS  " else "  FAIL  ", label, "\n", sep = "")
			if (!ok && nzchar(detail)) cat("        ", detail, "\n", sep = "")

		#	Assemble result
			return(invisible(ok))
	}

#	Report Without Asserting
	note <- function(text) {
		#	"""
		#	Args:
		#		text: a measurement worth recording that is not a pass or fail
		#	Returns:
		#		invisible NULL
		#	Notes:
		#		For quantities that are expected to vary with the construction and so
		#		cannot carry a fixture, but whose value a reader of the log wants.
		#	"""

		#	Report
			cat("  NOTE  ", text, "\n", sep = "")

		#	Assemble result
			return(invisible(NULL))
	}

#	Compare Two Data Frames Exactly
	identical_frame <- function(a, b) {
		#	"""
		#	Args:
		#		a, b: data.frames to compare
		#	Returns:
		#		logical(1), TRUE when identical after dropping row names
		#	Notes:
		#		Row names are stripped because they carry provenance from subsetting
		#		and are not part of the result. Everything else must match, including
		#		column order and storage mode.
		#	"""

		#	Normalize
			a <- as.data.frame(a)
			b <- as.data.frame(b)
			rownames(a) <- NULL
			rownames(b) <- NULL

		#	Assemble result
			return(identical(a, b))
	}

#	Look Up One Era's Fixture
	fix_era <- function(era) {
		#	"""
		#	Args:
		#		era: integer era number
		#	Returns:
		#		one-row data.frame from FIX_ERAS
		#	"""

		#	Assemble result
			return(FIX_ERAS[FIX_ERAS$era == era, , drop = FALSE])
	}

##########################
#####     Import      ####
##########################

#	Importing Functions
	cat("Path A foundation tests\n")
	cat("=======================\n\n")

#	Loaded through PathA_Pipeline.R when it is available, so the load order lives in
#	one place rather than here and in the runner both. Falls back to sourcing
#	directly, so the suite still runs on a checkout without the pipeline file.
	cat("Sourcing pipeline...\n")
	if (exists("pa_load_pipeline", mode = "function")) {
		pa_load_pipeline(verbose = TRUE)
	} else {
		pipeline_file <- file.path(SCRIPT_DIR, "Path_A/PathA_Pipeline.R")
		if (file.exists(pipeline_file)) {
			source(pipeline_file)
			pa_load_pipeline(verbose = TRUE)
		} else {
			source(file.path(SCRIPT_DIR, "RPajekFunctions_30April2023.r"))
			source(file.path(SCRIPT_DIR, "Path_A/PathA_PajekReaders.R"))
			source(file.path(SCRIPT_DIR, "Path_A/PathA_EraResolution.R"))
			source(file.path(SCRIPT_DIR, "Path_A/PathA_TieLayer.R"))
		}
	}

#	Loading the Node List
#	Named explicitly rather than taken as ls(env)[[1]], which is sensitive to what
#	else the Rda happens to contain.
	cat("Loading node list...\n")
	nl_env <- new.env(parent = emptyenv())
	load(NODE_LIST_RDA, envir = nl_env)
	node_list <- as.data.frame(get("pa_nodes", envir = nl_env, inherits = FALSE))

#	Loading the Edge List
	cat("Loading edge list...\n")
	el_env <- new.env(parent = emptyenv())
	load(EDGE_LIST_RDA, envir = el_env)
	edges_raw <- as.data.frame(get("pa_edges", envir = el_env, inherits = FALSE))

	cat("\n")

##############################
####  0. Corpus           ####
##############################

#	Checking the Corpus Fixtures
	cat("0. Corpus\n")

	check(paste0("node list has ", format(FIX_NODES, big.mark = ","), " rows"),
		  nrow(node_list) == FIX_NODES,
		  paste("got", format(nrow(node_list), big.mark = ",")))
	check(paste0("edge list has ", format(FIX_EDGES, big.mark = ","), " rows"),
		  nrow(edges_raw) == FIX_EDGES,
		  paste("got", format(nrow(edges_raw), big.mark = ",")))
	check("node list carries the rebuilt column names",
		  all(c("node_id", "node_key", "era", "is_article", "is_cited") %in%
			  names(node_list)),
		  paste("missing:", paste(setdiff(c("node_id", "node_key", "era",
											"is_article", "is_cited"),
										 names(node_list)), collapse = ", ")))
	check("edge list already carries sender_era and target_era",
		  all(c("sender_era", "target_era") %in% names(edges_raw)),
		  "the construction attaches both before saving")
	cat("\n")

##############################
####  1. Preflight        ####
##############################

#	Checking Era Inputs
	cat("1. Preflight: era windows, pairs, file resolution\n")

	pairs <- pa_era_pairs(PA_ANALYTIC_ERAS)
	check("six era pairs from the analytic range", nrow(pairs) == 6L,
		  paste("got", nrow(pairs)))
	check("pair index runs 1 to 6", identical(pairs$pair_index, 1:6))
	check("window spans 1971 to 2019",
		  pairs$year_prior_start[[1L]] == 1971L && pairs$year_t_end[[6L]] == 2019L,
		  paste("got", pairs$year_prior_start[[1L]], "to", pairs$year_t_end[[6L]]))

#	Era windows against the fixtures
	win <- pa_era_window(FIX_ERAS$era)
	check("era windows match the construction",
		  identical(win$year_start, FIX_ERAS$year_start) &&
			  identical(win$year_end, FIX_ERAS$year_end),
		  "seven-year tiles from 1901")
	check("era midpoints are evenly spaced",
		  length(unique(diff(win$midpoint))) == 1L,
		  "which is why pair_index carries the same information")

#	Checking Pajek Inputs
	manifest <- pa_era_manifest(PA_ANALYTIC_ERAS)
	check("every era in scope resolves to a network and a partition",
		  nrow(manifest) == 7L && all(file.exists(manifest$net)) &&
			  all(file.exists(manifest$clu)))
	check("every partition matches its network in length",
		  all(manifest$lengths_agree),
		  paste("mismatched for era(s):",
				paste(manifest$era[!manifest$lengths_agree], collapse = ", ")))
	check("one specification and one grid across the run",
		  length(unique(manifest$spec_tag)) == 1L &&
			  length(unique(manifest$grid)) == 1L,
		  paste(unique(manifest$spec_tag), collapse = ", "))

	for (msg in pa_manifest_warnings(manifest)) note(msg)
	cat("\n")

##############################
####  2. Inputs           ####
##############################

#	Loading Inputs
	cat("2. Inputs: networks, partitions, era maps\n")

#	Each era is loaded inside tryCatch. The readers signal on a bad input, which is
#	right for the pipeline but wrong for a test harness: an abort here would hide
#	the determinism and fixture checks below, which are the ones that matter.
	era_data <- list()
	era_status <- data.frame(era = manifest$era, ok = FALSE,
							 absent = NA_integer_, msg = "",
							 stringsAsFactors = FALSE)
	for (i in seq_len(nrow(manifest))) {
		e <- manifest$era[[i]]
		res <- tryCatch({
			net <- pa_read_era_network(manifest$net[[i]], manifest$clu[[i]])
			map <- pa_era_map(node_list, net$vertices, net$partition, e)
			vs  <- attr(map, "vertex_set")
			era_data[[as.character(e)]] <- list(map = map,
												n_vertices = net$n_vertices,
												n_communities = net$n_communities)
			cat("    era ", e, " (", manifest$year_start[[i]], "-",
				manifest$year_end[[i]], "): ", net$n_vertices, " vertices, ",
				net$n_communities, " communities\n", sep = "")
			list(ok = TRUE, absent = vs$n_nodes_absent_from_network, msg = "")
		}, error = function(err) {
			cat("    era ", e, ": FAILED -- ", conditionMessage(err), "\n", sep = "")
			list(ok = FALSE, absent = NA_integer_, msg = conditionMessage(err))
		})
		era_status$ok[[i]]     <- res$ok
		era_status$absent[[i]] <- res$absent
		era_status$msg[[i]]    <- res$msg
	}

	check("every era loads, maps, and agrees with its partition",
		  all(era_status$ok),
		  paste("failed for era(s):",
				paste(era_status$era[!era_status$ok], collapse = ", ")))

#	Vertex and community counts against the fixtures
	if (all(era_status$ok)) {
		got_v <- vapply(as.character(FIX_ERAS$era),
						function(k) era_data[[k]]$n_vertices, integer(1))
		got_c <- vapply(as.character(FIX_ERAS$era),
						function(k) era_data[[k]]$n_communities, integer(1))
		check("era vertex counts match the fixtures",
			  identical(unname(got_v), FIX_ERAS$vertices),
			  paste("got", paste(unname(got_v), collapse = ", ")))
		check("era community counts match the primary specification",
			  identical(unname(got_c), FIX_ERAS$communities),
			  paste("got", paste(unname(got_c), collapse = ", ")))

		#	Citing articles per era, from the map rather than the node table
			got_cite <- vapply(as.character(FIX_ERAS$era), function(k) {
				m <- era_data[[k]]$map
				as.integer(sum(m$is_article, na.rm = TRUE))
			}, integer(1))
			note(paste0("articles per era from the map: ",
						paste(unname(got_cite), collapse = ", ")))
	}

#	The vertex set is NOT asserted to match exactly
#	An era network keeps only nodes carrying a within-era arc, so nodes the node
#	list assigns to an era by publication year but which neither cite nor are cited
#	inside it are absent by design. What matters is the other direction, that every
#	VERTEX resolved -- pa_era_map errors if one does not, so reaching here proves it.
	note(paste0("era nodes absent from their network: ",
				paste(era_status$absent, collapse = ", "),
				" (expected: the network keeps only nodes on a within-era arc)"))
	cat("\n")

##############################
####  3. Edge list        ####
##############################

#	Checking the Edge List and the Collision Guard
	cat("3. Edge list: deduplication and the sender_era collision guard\n")

	dd <- pa_dedupe_edges(edges_raw)
	edges <- dd$edges
	check("deduplication removes nothing from the rebuilt list",
		  dd$n_removed == 0L,
		  paste("removed", format(dd$n_removed, big.mark = ","),
				"-- the construction should already have reduced to one row per citation"))
	check("no repeated sender-target pair survives",
		  !any(duplicated(edges[c("sender_id", "target_id")])))

#	THE COLLISION GUARD. This is the check that matters most in this section.
	edges_dated <- pa_attach_sender_era(edges, node_list)
	era_cols <- grep("^sender_era", names(edges_dated), value = TRUE)
	check("exactly one sender_era column survives the attach",
		  identical(era_cols, "sender_era"),
		  paste("found:", paste(era_cols, collapse = ", "),
				"-- a join onto an edge list that already carried sender_era",
				"produces .x and .y, after which every era filter returns",
				"logical(0) and every pair reports zero bridging works silently"))
	check("sender_era is populated, not NULL",
		  !is.null(edges_dated$sender_era) &&
			  sum(!is.na(edges_dated$sender_era)) > 0L,
		  sprintf("%s of %s edges dated",
				  format(sum(!is.na(edges_dated$sender_era)), big.mark = ","),
				  format(nrow(edges_dated), big.mark = ",")))
	check("the attach does not change the row count",
		  nrow(edges_dated) == nrow(edges))

#	Idempotence: attaching twice must be a no-op
	twice <- pa_attach_sender_era(edges_dated, node_list, verbose = FALSE)
	check("attaching the citing era twice changes nothing",
		  identical_frame(edges_dated, twice))
	cat("\n")

##############################
####  4. Determinism      ####
##############################

#	Checking Order Invariance
	cat("4. Determinism: shuffling the edge list changes nothing\n")

	set.seed(20261027)
	shuffled_raw <- edges_raw[sample.int(nrow(edges_raw)), , drop = FALSE]
	dd_shuf <- pa_dedupe_edges(shuffled_raw, verbose = FALSE)
	check("deduplication is order invariant",
		  identical_frame(dd$edges, dd_shuf$edges))

#	Shuffle AFTER dating, so the sort inside the dedupe does not mask the test
	dated_shuf <- edges_dated[sample.int(nrow(edges_dated)), , drop = FALSE]
	ep <- FIX_RISK_PAIR[["prior"]]
	et <- FIX_RISK_PAIR[["curr"]]
	res_a <- pa_pair_ties(edges_dated, era_data[[as.character(ep)]]$map,
						  era_data[[as.character(et)]]$map, ep, et,
						  with_risk_set = FALSE)
	res_b <- pa_pair_ties(dated_shuf, era_data[[as.character(ep)]]$map,
						  era_data[[as.character(et)]]$map, ep, et,
						  with_risk_set = FALSE)
	check("bridging works are order invariant",
		  identical_frame(res_a$bridging, res_b$bridging))
	check("work-resolution ties are order invariant",
		  identical_frame(res_a$work_ties, res_b$work_ties))
	check("descent weights are order invariant",
		  identical_frame(res_a$descent, res_b$descent))
	cat("\n")

##############################
####  5. Known answers    ####
##############################

#	Checking Every Pair against Its Fixtures
	cat("5. Known-answer fixtures, all six pairs\n")

	results <- vector("list", nrow(pairs))
	for (k in seq_len(nrow(pairs))) {
		p  <- pairs$era_prior[[k]]
		t  <- pairs$era_t[[k]]
		fp <- fix_era(p)
		ft <- fix_era(t)
		r  <- pa_pair_ties(edges_dated, era_data[[as.character(p)]]$map,
						   era_data[[as.character(t)]]$map, p, t,
						   with_risk_set = FALSE)
		results[[k]] <- r
		s <- r$summary
		tag <- paste0(p, "-", t)

		cat("    pair ", tag, ": ", s$n_ties, " ties over ", s$n_bridging_works,
			" bridging works; ", s$n_citing_participating, " of ",
			s$n_citing_communities, " citing and ", s$n_cited_participating,
			" of ", s$n_cited_communities, " cited communities participate\n",
			sep = "")

		check(paste0("pair ", tag, ": bridging work count"),
			  s$n_bridging_works == fp$bridge,
			  paste("expected", fp$bridge, "got", s$n_bridging_works))
		check(paste0("pair ", tag, ": citing community count"),
			  s$n_citing_communities == ft$communities,
			  paste("expected", ft$communities, "got", s$n_citing_communities))
		check(paste0("pair ", tag, ": cited community count"),
			  s$n_cited_communities == fp$communities,
			  paste("expected", fp$communities, "got", s$n_cited_communities))
		check(paste0("pair ", tag, ": ties are non-empty"), s$n_ties > 0L)
		check(paste0("pair ", tag, ": participation within bounds"),
			  s$n_citing_participating <= s$n_citing_communities &&
				  s$n_cited_participating <= s$n_cited_communities)
	}
	cat("\n")

##############################
####  6. Conservation     ####
##############################

#	Checking the Weighting Identities
	cat("6. Conservation and consistency\n")

	for (k in seq_len(nrow(pairs))) {
		p <- pairs$era_prior[[k]]
		t <- pairs$era_t[[k]]
		r <- results[[k]]
		tag <- paste0(p, "-", t)

		#	Fractional weights sum to the number of distinct bridging works that
		#	actually carry a tie -- not to every bridging work, since a work can
		#	bridge the pair and still be cited by no era-t community in the network
			carried <- length(unique(r$work_ties$work_id))
			check(paste0("pair ", tag, ": fractional weights sum to the carrying works"),
				  isTRUE(all.equal(sum(r$descent$w_fractional), carried,
								   tolerance = 1e-8)),
				  paste("sum", round(sum(r$descent$w_fractional), 6), "vs", carried))

		#	Breadth totals equal the number of (work, pair) incidences
			check(paste0("pair ", tag, ": breadth totals equal work-pair incidences"),
				  sum(r$descent$w_breadth) ==
					  nrow(unique(r$work_ties[c("work_id", "citing_community",
												"cited_community")])))

		#	Intensity totals equal total citation events
			check(paste0("pair ", tag, ": intensity totals equal citation events"),
				  sum(r$descent$w_intensity) == sum(r$work_ties$y_cites))

		#	Every realized tie carries at least one citation
			check(paste0("pair ", tag, ": every work tie carries a citation"),
				  all(r$work_ties$y_cites > 0L))

		#	Aggregating the work frame reproduces the community-pair ties exactly
			agg <- unique(r$work_ties[c("citing_community", "cited_community")])
			agg <- agg[order(agg$citing_community, agg$cited_community,
							 method = "radix"), , drop = FALSE]
			rownames(agg) <- NULL
			ref <- r$descent[c("citing_community", "cited_community")]
			rownames(ref) <- NULL
			check(paste0("pair ", tag, ": work frame aggregates to the tie set"),
				  identical_frame(agg, ref))

		#	Concentration is bounded
			check(paste0("pair ", tag, ": hhi lies in (0, 1]"),
				  all(is.na(r$descent$hhi) |
					  (r$descent$hhi > 0 & r$descent$hhi <= 1 + 1e-9)))

		#	The single-work concentration the specification records
			note(paste0("pair ", tag, ": ", r$summary$pct_single_work_ties,
						"% of ties rest on a single shared work"))
	}
	cat("\n")

##############################
####  7. Risk set         ####
##############################

#	Checking the Expanded Frame on the Smallest Pair
	cat("7. Risk set, pair ", FIX_RISK_PAIR[["prior"]], "-",
		FIX_RISK_PAIR[["curr"]], " only\n", sep = "")

	ep <- FIX_RISK_PAIR[["prior"]]
	et <- FIX_RISK_PAIR[["curr"]]
	ft <- fix_era(et)
	ridx <- which(pairs$era_prior == ep & pairs$era_t == et)
	oc <- pa_out_citations(edges_dated, era_data[[as.character(et)]]$map, et)

	check("out-citation exposure covers every era-t community",
		  nrow(oc) == ft$communities,
		  paste("expected", ft$communities, "got", nrow(oc)))
	check("exposure is non-negative everywhere", all(oc$out_citations_i >= 0L))

	risk <- pa_risk_set(results[[ridx]]$work_ties, results[[ridx]]$bridging,
						unique(era_data[[as.character(et)]]$map$community), oc)
	live <- sum(oc$out_citations_i > 0L)
	cat("    ", nrow(results[[ridx]]$bridging), " bridging works x ", live,
		" communities with opportunity = ", format(nrow(risk), big.mark = ","),
		" rows\n", sep = "")

	check("risk set is the full grid over communities with opportunity",
		  nrow(risk) == nrow(results[[ridx]]$bridging) * live,
		  paste("got", format(nrow(risk), big.mark = ",")))
	check("every realized citation survives into the risk set",
		  sum(risk$y_cites) == sum(results[[ridx]]$work_ties$y_cites),
		  paste(sum(risk$y_cites), "vs", sum(results[[ridx]]$work_ties$y_cites)))
	check("the risk set is overwhelmingly zeros",
		  mean(risk$y_cites == 0L) > 0.99,
		  paste("zero share", round(mean(risk$y_cites == 0L), 5)))
	check("no risk-set row has zero opportunity",
		  all(risk$out_citations_i > 0L))
	check("every bridging work appears in the risk set",
		  length(unique(risk$work_id)) == nrow(results[[ridx]]$bridging))
	check("the cited community follows from the work",
		  !any(duplicated(unique(risk[c("work_id", "cited_community")])$work_id)),
		  "a work carrying two cited communities would break the Model II frame")
	cat("\n")

##########################
#####     Report      ####
##########################

#	Reporting
	cat("=======================\n")
	cat(.tests_run - .tests_failed, " of ", .tests_run, " checks passed\n", sep = "")
	if (.tests_failed > 0L) {
		cat("FAILED\n")
		quit(status = 1L)
	}
	cat("OK\n")
