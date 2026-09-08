#	Test_PathA_TieLayer.R -- Test Suite for Path A Stages 1 to 3
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
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
	PAJEK_DIR     <- file.path(PROJECT_DIR, "pajek_files")
	NODE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_node_list_3Oct2024.Rda")
	EDGE_LIST_RDA <- file.path(PROJECT_DIR, "data/citation_edge_list_21Mar2024.Rda")

#	Fixtures recorded in the specification (S14.4)
	FIX_EDGE_ROWS <- 1238336L
	FIX_PAIRS <- list(
		list(prior = 22L, curr = 23L, ties = 561L, works = 877L,
			 citing_part = 151L, citing_all = 394L, cited_part = 234L, cited_all = 794L),
		list(prior = 21L, curr = 22L, ties = 251L, works = 126L,
			 citing_part = NA_integer_, citing_all = 794L, cited_part = NA_integer_, cited_all = 955L))

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
			rownames(a) <- NULL
			rownames(b) <- NULL

		#	Assemble result
			return(identical(a, b))
	}

##########################
#####     Import      ####
##########################

#	Importing Functions
	cat("Path A foundation tests\n")
	cat("=======================\n\n")

	cat("Sourcing pipeline...\n")
	source(file.path(SCRIPT_DIR, "RPajekFunctions_30April2023.r"))
	source(file.path(SCRIPT_DIR, "Path_A/PathA_PajekReaders.R"))
	source(file.path(SCRIPT_DIR, "Path_A/PathA_EraResolution.R"))
	source(file.path(SCRIPT_DIR, "Path_A/PathA_TieLayer.R"))

#	Loading the Node List
	cat("Loading node list...\n")
	nl_env <- new.env(); load(NODE_LIST_RDA, envir = nl_env)
	node_list <- get(ls(nl_env)[[1L]], envir = nl_env)

#	Loading the Edge List
	cat("Loading edge list...\n")
	el_env <- new.env(); load(EDGE_LIST_RDA, envir = el_env)
	edges_raw <- get(ls(el_env)[[1L]], envir = el_env)

	cat("\n")

##############################
####  1. Preflight        ####
##############################

#	Checking Era Inputs
	cat("1. Preflight: era file resolution\n")

	pairs <- pa_era_pairs(PA_INTEGRATION_ERAS)
	check("eleven era pairs from the integration window", nrow(pairs) == 11L,
		  paste("got", nrow(pairs)))
	check("pair index runs 1 to 11", identical(pairs$pair_index, 1:11))
	check("window spans 2009 to 2020",
		  pairs$year_prior[[1L]] == 2009L && pairs$year_t[[11L]] == 2020L)

#	Checking Pajek Inputs
	manifest <- pa_era_manifest(PAJEK_DIR, PA_INTEGRATION_ERAS)
	check("every era in scope resolves to a network and a partition",
		  nrow(manifest) == 12L && all(file.exists(manifest$net)) && all(file.exists(manifest$clu)))
	check("no era resolved ambiguously",
		  all(manifest$n_net_candidates == 1L) && all(manifest$n_clu_candidates == 1L),
		  "more than one candidate existed; see the manifest")

	for (msg in pa_manifest_warnings(manifest)) cat("  NOTE  ", msg, "\n", sep = "")
	cat("\n")

##############################
####  2. Inputs           ####
##############################

#	Loading Inmputs
	cat("2. Inputs: networks, partitions, era maps\n")

#	Each era is loaded inside tryCatch. The readers signal on a bad input, which is
#	right for the pipeline but wrong for a test harness: an abort here would hide
#	the determinism and fixture checks below, which are the ones that matter.
	era_data <- list()
	era_status <- data.frame(era = manifest$era, ok = FALSE, exact = NA,
							 msg = "", stringsAsFactors = FALSE)
	for (i in seq_len(nrow(manifest))) {
		e <- manifest$era[[i]]
		res <- tryCatch({
			net <- pa_read_era_network(manifest$net[[i]], manifest$clu[[i]])
			map <- pa_era_map(node_list, net$vertices, net$partition, e)
			vs  <- attr(map, "vertex_set")
			era_data[[as.character(e)]] <- list(map = map, n_vertices = net$n_vertices,
												n_communities = net$n_communities)
			cat("    era ", e, " (", manifest$year[[i]], "): ", net$n_vertices,
				" vertices, ", net$n_communities, " communities\n", sep = "")
			list(ok = TRUE, exact = vs$exact_match, msg = "")
		}, error = function(err) {
			cat("    era ", e, " (", manifest$year[[i]], "): FAILED -- ",
				conditionMessage(err), "\n", sep = "")
			list(ok = FALSE, exact = NA, msg = conditionMessage(err))
		})
		era_status$ok[[i]]    <- res$ok
		era_status$exact[[i]] <- res$exact
		era_status$msg[[i]]   <- res$msg
	}

	check("every era loads, maps, and agrees with its partition",
		  all(era_status$ok),
		  paste("failed for era(s):", paste(era_status$era[!era_status$ok], collapse = ", ")))
	check("every era's vertex set matches the node list exactly",
		  all(era_status$exact[era_status$ok]),
		  paste("inexact for era(s):",
				paste(era_status$era[era_status$ok & !era_status$exact], collapse = ", ")))
	cat("\n")

##############################
####  3. Edge list        ####
##############################

	cat("3. Edge list deduplication\n")

	dd <- pa_dedupe_edges(edges_raw)
	edges <- dd$edges
	check(paste0("deduplicated edge list has ", format(FIX_EDGE_ROWS, big.mark = ","), " rows"),
		  dd$n_after == FIX_EDGE_ROWS,
		  paste("got", format(dd$n_after, big.mark = ",")))
	check("no repeated sender-target pair survives",
		  !any(duplicated(edges[c("sender_id", "target_id")])))

	edges_dated <- pa_attach_sender_era(edges, node_list)
	check("every edge carries a citing era or an explicit NA",
		  nrow(edges_dated) == nrow(edges))
	cat("\n")

##############################
####  4. Determinism      ####
##############################

	cat("4. Determinism: shuffling the edge list changes nothing\n")

	set.seed(20260908)
	shuffled <- edges_raw[sample.int(nrow(edges_raw)), , drop = FALSE]
	dd_shuf <- pa_dedupe_edges(shuffled, verbose = FALSE)
	check("deduplication is order invariant", identical_frame(dd$edges, dd_shuf$edges))

	dated_shuf <- pa_attach_sender_era(dd_shuf$edges, node_list)
	fix <- FIX_PAIRS[[1L]]
	res_a <- pa_pair_ties(edges_dated, era_data[[as.character(fix$prior)]]$map,
						  era_data[[as.character(fix$curr)]]$map, fix$prior, fix$curr,
						  with_risk_set = FALSE)
	res_b <- pa_pair_ties(dated_shuf, era_data[[as.character(fix$prior)]]$map,
						  era_data[[as.character(fix$curr)]]$map, fix$prior, fix$curr,
						  with_risk_set = FALSE)
	check("bridging works are order invariant", identical_frame(res_a$bridging, res_b$bridging))
	check("work-resolution ties are order invariant", identical_frame(res_a$work_ties, res_b$work_ties))
	check("descent weights are order invariant", identical_frame(res_a$descent, res_b$descent))
	cat("\n")

##############################
####  5. Known answers    ####
##############################

	cat("5. Known-answer fixtures\n")

	results <- vector("list", length(FIX_PAIRS))
	for (k in seq_along(FIX_PAIRS)) {
		f <- FIX_PAIRS[[k]]
		r <- pa_pair_ties(edges_dated, era_data[[as.character(f$prior)]]$map,
						  era_data[[as.character(f$curr)]]$map, f$prior, f$curr,
						  with_risk_set = FALSE)
		results[[k]] <- r
		s <- r$summary
		tag <- paste0(f$prior, "-", f$curr)

		cat("    pair ", tag, ": ", s$n_ties, " ties over ", s$n_bridging_works,
			" bridging works; ", s$n_citing_participating, " of ", s$n_citing_communities,
			" citing and ", s$n_cited_participating, " of ", s$n_cited_communities,
			" cited communities participate\n", sep = "")

		check(paste0("pair ", tag, ": tie count"), s$n_ties == f$ties,
			  paste("expected", f$ties, "got", s$n_ties))
		check(paste0("pair ", tag, ": bridging work count"), s$n_bridging_works == f$works,
			  paste("expected", f$works, "got", s$n_bridging_works))
		check(paste0("pair ", tag, ": citing community count"),
			  s$n_citing_communities == f$citing_all,
			  paste("expected", f$citing_all, "got", s$n_citing_communities))
		check(paste0("pair ", tag, ": cited community count"),
			  s$n_cited_communities == f$cited_all,
			  paste("expected", f$cited_all, "got", s$n_cited_communities))
		if (!is.na(f$citing_part)) {
			check(paste0("pair ", tag, ": participating citing communities"),
				  s$n_citing_participating == f$citing_part,
				  paste("expected", f$citing_part, "got", s$n_citing_participating))
			check(paste0("pair ", tag, ": participating cited communities"),
				  s$n_cited_participating == f$cited_part,
				  paste("expected", f$cited_part, "got", s$n_cited_participating))
		}
	}
	cat("\n")

##############################
####  6. Conservation     ####
##############################

	cat("6. Conservation and consistency\n")

	for (k in seq_along(FIX_PAIRS)) {
		f <- FIX_PAIRS[[k]]
		r <- results[[k]]
		tag <- paste0(f$prior, "-", f$curr)

		#	Fractional weights sum to the number of distinct bridging works that
		#	actually carry a tie -- not to every bridging work, since a work can
		#	bridge the pair and still be cited by no era-t community in the network
			carried <- length(unique(r$work_ties$work_id))
			check(paste0("pair ", tag, ": fractional weights sum to distinct carrying works"),
				  isTRUE(all.equal(sum(r$descent$w_fractional), carried)),
				  paste("sum", round(sum(r$descent$w_fractional), 6), "vs", carried))

		#	Breadth totals equal the number of (work, pair) incidences
			check(paste0("pair ", tag, ": breadth totals equal work-pair incidences"),
				  sum(r$descent$w_breadth) == nrow(unique(r$work_ties[c("work_id", "citing_community", "cited_community")])))

		#	Intensity totals equal total citation events
			check(paste0("pair ", tag, ": intensity totals equal citation events"),
				  sum(r$descent$w_intensity) == sum(r$work_ties$y_cites))

		#	Aggregating the work frame reproduces the community-pair ties exactly
			agg <- unique(r$work_ties[c("citing_community", "cited_community")])
			agg <- agg[order(agg$citing_community, agg$cited_community, method = "radix"), , drop = FALSE]
			rownames(agg) <- NULL
			ref <- r$descent[c("citing_community", "cited_community")]
			rownames(ref) <- NULL
			check(paste0("pair ", tag, ": work frame aggregates to the tie set"),
				  identical_frame(agg, ref))

		#	The single-work concentration the specification records
			cat("    pair ", tag, ": ", r$summary$pct_single_work_ties,
				"% of ties rest on a single shared work\n", sep = "")
	}
	cat("\n")

##############################
####  7. Risk set         ####
##############################

	cat("7. Risk set\n")

	f <- FIX_PAIRS[[1L]]
	oc <- pa_out_citations(edges_dated, era_data[[as.character(f$curr)]]$map, f$curr)
	check("out-citation exposure covers every era-t community",
		  nrow(oc) == f$citing_all, paste("got", nrow(oc)))
	check("exposure is non-negative everywhere", all(oc$out_citations_i >= 0L))

	risk <- pa_risk_set(results[[1L]]$work_ties, results[[1L]]$bridging,
						unique(era_data[[as.character(f$curr)]]$map$community), oc)
	live <- sum(oc$out_citations_i > 0L)
	cat("    ", f$works, " bridging works x ", live, " communities with opportunity = ",
		format(nrow(risk), big.mark = ","), " rows\n", sep = "")
	check("risk set is the full grid over communities with opportunity",
		  nrow(risk) == f$works * live)
	check("every realized citation survives into the risk set",
		  sum(risk$y_cites) == sum(results[[1L]]$work_ties$y_cites))
	check("the risk set is overwhelmingly zeros",
		  mean(risk$y_cites == 0L) > 0.99)
	check("no risk-set row has zero opportunity", all(risk$out_citations_i > 0L))
	cat("\n")

##########################
#####     Report      ####
##########################

	cat("=======================\n")
	cat(.tests_run - .tests_failed, " of ", .tests_run, " checks passed\n", sep = "")
	if (.tests_failed > 0L) {
		cat("FAILED\n")
		quit(status = 1L)
	}
	cat("OK\n")
