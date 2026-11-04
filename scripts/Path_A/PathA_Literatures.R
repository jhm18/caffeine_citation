#	PathA_Literatures.R
#	The community network, and the literatures the blockmodel finds in it.
#
#	Jonathan H. Morgan and Sarah Delmar
#	03 November 2026
#
#	A LITERATURE IS A GROUP OF COMMUNITIES that connect to the rest of the field
#	in the same way (S literatures). Communities are groups of articles within
#	one era; a literature groups communities and may span eras. The criterion is
#	structural equivalence: two communities belong together not because they
#	cite each other but because they have the same pattern of ties to everything
#	else.
#
#	THIS LANE BUILDS THE NETWORK; PYTHON FITS IT. graph-tool is the only mature
#	implementation of the nested degree-corrected SBM with description-length
#	model selection and posterior sampling, and it lives in its own conda prefix
#	at $GT_PYTHON. This file assembles the community network, writes it, invokes
#	the fitter the way run_pajek.R invokes Pajek, and joins the result back.
#
#	THE ATTRIBUTION RULE, AND WHY THERE IS NO CHOICE TO MAKE. Every work in the
#	node list has exactly one era -- 589,784 nodes, 589,784 keys, one row each --
#	so a work has at most one community and there is no ambiguity about which
#	era's partition to read it from. 96.9 per cent of citations sent by
#	analytic-era articles land on works that are themselves in eras 11-17. The
#	3.1 per cent that do not are citations out of the back of the window, and
#	they concentrate in era 11 (60.4 per cent) and era 12 (24.4 per cent), where
#	there is no earlier era to land in. From era 13 on the loss is under ten per
#	cent and by era 17 it is one.
#
#	A second filter applies on top: a work needs a community, so it must be a
#	vertex in its era's NETWORK, not merely a row in that era's node list. That
#	share is not assumed here. pa_community_network reports the whole funnel.
#
#	Input:
#		data/eras_7yr/pa_node_list.Rda, pa_edge_list.Rda
#		the era networks and partitions, through pa_build_era_maps()
#
#	Output:
#		data/literatures/<spec_tag>/community_nodes.csv
#		data/literatures/<spec_tag>/community_edges.csv
#		data/literatures/<spec_tag>/community_literatures.csv   (from python)
#		data/literatures/<spec_tag>/literature_marginals.csv    (from python)
#		data/literatures/literature_summary.csv
#		data/literatures/literature_tests.csv

#################
#   LIBRARIES   #
#################

	suppressPackageStartupMessages({
		library(data.table)
		library(readr)
	})

#####################
#   CONFIGURATION   #
#####################

#	Project Root
	LIT_ROOT <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")

#	Inputs
	LIT_ERA_DIR <- file.path(LIT_ROOT, "data/eras_7yr")

#	Output
	LIT_OUT_DIR <- file.path(LIT_ROOT, "data/literatures")

#	The Fitter
#	Absolute path, never PATH. The container's PATH starts with /opt/sbi-env/bin
#	and RETICULATE_PYTHON and PyCall both point at /usr/bin/python3; graph-tool
#	is a third interpreter that must shadow neither.
	LIT_GT_PYTHON <- Sys.getenv("GT_PYTHON", "/opt/gt-env/bin/python")
	LIT_FIT_SCRIPT <- file.path(LIT_ROOT, "python_env/fit_literatures.py")

#	Which Specifications
#	The same primary-only default the covariate lane uses, and for the same
#	reason: the literatures have to describe the partition the outcome was
#	counted on.
	LIT_SPEC_ROLES <- "primary"

#	Sampler
#	Passed through to the fitter. The defaults are the documented idiom's --
#	equilibrate, then collect -- at a scale suited to 3,557 nodes rather than to
#	a textbook example.
	LIT_DRAWS <- 2000L
	LIT_WAIT  <- 1000L
	LIT_NITER <- 10L
	LIT_SEED  <- 20261103L

#	Keep Self-Loops
#	A community citing its own members is part of how it connects, and a
#	degree-corrected SBM reads self-loops as information rather than noise.
#	Dropping them would silently remove the same-era structure, which is
#	299,814 of the 1.1 million resolved citations -- not a rounding error.
	LIT_KEEP_SELF_LOOPS <- TRUE

########################
#   INTERNAL HELPERS   #
########################

#	Naming a Community Across Eras
#	Community ids restart at one in every era's partition, so "community 3" is
#	seven different things. The key is what makes a node of the pooled network.
	pa_community_key <- function(era, community) {
		return(sprintf("e%02d_c%04d", as.integer(era), as.integer(community)))
	}

#	Resolving the Specifications
	pa_lit_specs <- function(roles = LIT_SPEC_ROLES) {
		if (!exists("TL_SPECS")) {
			stop("pa_lit_specs: TL_SPECS not found; source ",
				 "scripts/Path_A/PathA_TieLayer.R first, or run this file ",
				 "under Rscript, which sources it.")
		}
		specs <- as.data.frame(TL_SPECS)
		if (!is.null(roles) && "spec_role" %in% names(specs)) {
			specs <- specs[specs$spec_role %in% roles, , drop = FALSE]
		}
		if (nrow(specs) == 0L) {
			stop("pa_lit_specs: no specification has spec_role in ",
				 paste(roles, collapse = ", "))
		}
		return(specs)
	}

######################################
#   BUILDING THE COMMUNITY NETWORK   #
######################################

#' @title pa_community_network
#' @description Collapse the citation network onto communities.
#'
#' @param edges The dated, de-duplicated edge list.
#' @param maps Output of pa_build_era_maps for one specification.
#' @param node_list The rebuilt citation node list.
#' @param keep_self_loops Retain i-to-i edges. Default LIT_KEEP_SELF_LOOPS.
#'
#' @details
#' One lookup does the work. Because node_id is globally unique and every work
#' carries exactly one era, a single node-to-community table covers both ends of
#' every edge -- there is no per-pair resolution and no question of which era's
#' partition to read a cited work from.
#'
#' A citation survives when BOTH endpoints resolve, which requires each to be a
#' vertex in its own era's network. The funnel is returned rather than reported
#' as a single percentage, because the two ways of failing are different things:
#' an endpoint in an era outside the analytic window is a boundary effect, while
#' an endpoint inside the window but absent from its network is a property of
#' how the networks were built.
#'
#' @return A list with nodes, edges and retention.
#'
#' @examples
#' net <- pa_community_network(edges, maps, node_list)
#' net$retention
#'
#' @export
pa_community_network <- function(edges, maps, node_list,
								 keep_self_loops = LIT_KEEP_SELF_LOOPS) {
	#	"""
	#	Args:
	#		edges: dated, de-duplicated edge list
	#		maps: output of pa_build_era_maps for one specification
	#		node_list: rebuilt citation node list
	#		keep_self_loops: retain i-to-i edges
	#	Returns:
	#		list(nodes, edges, retention)
	#	"""

	#	Validation
		need <- c("sender_id", "target_id")
		missing <- setdiff(need, names(edges))
		if (length(missing) > 0L) {
			stop("pa_community_network: the edge list is missing: ",
				 paste(missing, collapse = ", "))
		}
		if (length(maps) == 0L) {
			stop("pa_community_network: no era maps supplied")
		}

	#	One Lookup for Both Endpoints
		lookup <- rbindlist(lapply(names(maps), function(era) {
			m <- as.data.table(maps[[era]])
			return(m[, .(node_id = as.integer(node_id),
						 era = as.integer(era),
						 community = as.integer(community))])
		}))
		lookup[, community_key := pa_community_key(era, community)]
		setkey(lookup, node_id)

	#	Attach
		dt <- as.data.table(edges)[, .(sender_id = as.integer(sender_id),
									   target_id = as.integer(target_id))]
		dt[lookup, on = c(sender_id = "node_id"), community_i := i.community_key]
		dt[lookup, on = c(target_id = "node_id"), community_j := i.community_key]

	#	The Funnel
	#	Counted before anything is dropped, and split by which end failed.
		n_total <- nrow(dt)
		has_i <- !is.na(dt$community_i)
		has_j <- !is.na(dt$community_j)
		retention <- data.frame(
			stage = c("all citations",
					  "citing work has a community",
					  "cited work has a community",
					  "both ends resolve"),
			edges = c(n_total, sum(has_i), sum(has_j), sum(has_i & has_j)),
			stringsAsFactors = FALSE)
		retention$pct <- round(100 * retention$edges / n_total, 2)

	#	Collapse
		kept <- dt[has_i & has_j]
		out <- kept[, .(weight = .N), by = .(community_i, community_j)]
		n_self <- sum(out$community_i == out$community_j)
		if (!isTRUE(keep_self_loops)) {
			out <- out[community_i != community_j]
		}
		setorder(out, community_i, community_j)

	#	The Node Table
	#	Every community in the specification, including any that ended up
	#	isolated. An isolated community is a row in Model I like any other and
	#	dropping it here would make the join silently incomplete later.
		nodes <- rbindlist(lapply(names(maps), function(era) {
			m <- as.data.table(maps[[era]])
			one <- m[, .(n_works = .N,
						 n_source = sum(is_article %in% TRUE)),
					 by = .(era = as.integer(era),
							community = as.integer(community))]
			return(one)
		}))
		nodes[, community_key := pa_community_key(era, community)]
		setcolorder(nodes, c("community_key", "era", "community"))
		setorder(nodes, era, community)

	#	Degree, for the Report
		out_strength <- out[, .(out_citations = sum(weight)), by = community_i]
		in_strength  <- out[, .(in_citations  = sum(weight)), by = community_j]
		nodes[out_strength, on = c(community_key = "community_i"),
			  out_citations := i.out_citations]
		nodes[in_strength, on = c(community_key = "community_j"),
			  in_citations := i.in_citations]
		for (col in c("out_citations", "in_citations")) {
			nodes[is.na(get(col)), (col) := 0L]
		}

	#	Assembling Result
		attr(out, "n_self_loops") <- n_self
		return(list(nodes = nodes, edges = out, retention = retention))
}

##############################
#   FITTING THE BLOCKMODEL   #
##############################

#' @title pa_fit_literatures
#' @description Hand the community network to graph-tool and read the fit back.
#'
#' @param output_dir Directory holding community_nodes.csv and community_edges.csv.
#' @param draws Posterior draws to collect. Default LIT_DRAWS.
#' @param wait Equilibration wait. Default LIT_WAIT.
#' @param niter Sweeps per MCMC step. Default LIT_NITER.
#' @param seed Random seed. Default LIT_SEED.
#' @param gt_python The graph-tool interpreter. Default LIT_GT_PYTHON.
#' @param fit_script Path to fit_literatures.py. Default LIT_FIT_SCRIPT.
#' @param verbose Stream the fitter's output. Default TRUE.
#'
#' @details
#' Invoked by absolute path rather than through reticulate. reticulate binds one
#' interpreter per session and this one is already bound to /usr/bin/python3 for
#' the rest of the project; pointing it at the graph-tool prefix instead would
#' trade one lane's convenience for every other lane's.
#'
#' A non-zero exit stops the lane. The fitter promotes graph-tool's warnings to
#' exceptions, so an unrecognised keyword comes back as a failure rather than as
#' a model quietly fitted without degree correction.
#'
#' @return A list with assignments and marginals.
#'
#' @examples
#' fit <- pa_fit_literatures(spec_dir)
#'
#' @export
pa_fit_literatures <- function(output_dir, draws = LIT_DRAWS,
							   wait = LIT_WAIT, niter = LIT_NITER,
							   seed = LIT_SEED, gt_python = LIT_GT_PYTHON,
							   fit_script = LIT_FIT_SCRIPT, verbose = TRUE) {
	#	"""
	#	Args:
	#		output_dir: directory holding the node and edge CSVs
	#		draws, wait, niter, seed: sampler settings
	#		gt_python: the graph-tool interpreter
	#		fit_script: path to fit_literatures.py
	#		verbose: stream the fitter's output
	#	Returns:
	#		list(assignments, marginals)
	#	"""

	#	Validation
		if (!file.exists(gt_python)) {
			stop("pa_fit_literatures: graph-tool interpreter not found at ",
				 gt_python, ". Run scripts/install_graph_tool.sh, or set ",
				 "GT_PYTHON.")
		}
		if (!file.exists(fit_script)) {
			stop("pa_fit_literatures: fitter not found at ", fit_script)
		}
		for (f in c("community_nodes.csv", "community_edges.csv")) {
			if (!file.exists(file.path(output_dir, f))) {
				stop("pa_fit_literatures: ", f, " not found in ", output_dir)
			}
		}

	#	Call
		arguments <- c(shQuote(fit_script),
					   "--nodes", shQuote(file.path(output_dir, "community_nodes.csv")),
					   "--edges", shQuote(file.path(output_dir, "community_edges.csv")),
					   "--out",   shQuote(output_dir),
					   "--draws", as.integer(draws),
					   "--wait",  as.integer(wait),
					   "--niter", as.integer(niter),
					   "--seed",  as.integer(seed))
		status <- system2(gt_python, arguments,
						  stdout = if (verbose) "" else FALSE,
						  stderr = if (verbose) "" else FALSE)
		if (!identical(as.integer(status), 0L)) {
			stop("pa_fit_literatures: the fitter exited with status ", status)
		}

	#	Read Back
		assignments <- as.data.table(readr::read_csv(
			file.path(output_dir, "community_literatures.csv"),
			show_col_types = FALSE, progress = FALSE))
		marginals <- as.data.table(readr::read_csv(
			file.path(output_dir, "literature_marginals.csv"),
			show_col_types = FALSE, progress = FALSE))

	#	Assembling Result
		return(list(assignments = assignments, marginals = marginals))
}

######################################
#   EXECUTING THE LITERATURES LANE   #
######################################

#' @title pa_run_literatures
#' @description Build the community network, fit the blockmodel, and write the result.
#'
#' @param specs A data.frame of specifications, or NULL for pa_lit_specs().
#' @param eras Integer eras, or NULL for PA_ANALYTIC_ERAS.
#' @param era_dir Directory holding the node and edge lists. Default LIT_ERA_DIR.
#' @param output_dir Destination root. Default LIT_OUT_DIR.
#' @param fit Run the fitter. FALSE builds and writes the network only.
#' @param verbose Report progress. Default TRUE.
#'
#' @return Invisibly, a list with nodes, edges, retention, assignments and tests.
#'
#' @examples
#' res <- pa_run_literatures()
#' res$retention
#'
#' @export
pa_run_literatures <- function(specs = NULL, eras = NULL,
							   era_dir = LIT_ERA_DIR,
							   output_dir = LIT_OUT_DIR,
							   fit = TRUE, verbose = TRUE) {
	#	"""
	#	Args:
	#		specs: specification table, or NULL
	#		eras: integer eras, or NULL
	#		era_dir: directory holding the node and edge lists
	#		output_dir: destination root
	#		fit: run the fitter, or stop after writing the network
	#		verbose: report progress
	#	Returns:
	#		invisible list(nodes, edges, retention, assignments, tests)
	#	"""

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- pa_lit_specs()
		if (is.null(eras))  eras  <- PA_ANALYTIC_ERAS

	#	Validation
		for (fn in c("pa_build_era_maps", "pa_spec_tag", "pa_dedupe_edges",
					 "pa_attach_sender_era")) {
			if (!exists(fn, mode = "function")) {
				stop("pa_run_literatures: ", fn, "() not found; source ",
					 "PathA_TieLayer.R first")
			}
		}

	#	Load the Corpus
		nf <- file.path(era_dir, "pa_node_list.Rda")
		ef <- file.path(era_dir, "pa_edge_list.Rda")
		for (f in c(nf, ef)) {
			if (!file.exists(f)) stop("pa_run_literatures: not found: ", f)
		}
		env <- new.env(parent = emptyenv())
		load(nf, envir = env)
		load(ef, envir = env)
		node_list <- as.data.frame(get("pa_nodes", envir = env, inherits = FALSE))
		edge_list <- as.data.frame(get("pa_edges", envir = env, inherits = FALSE))
		if (verbose) {
			cat("Loaded ", nrow(node_list), " nodes and ", nrow(edge_list),
				" edges\n", sep = "")
		}

		ded <- pa_dedupe_edges(edge_list, verbose = verbose)
		edges <- pa_attach_sender_era(ded$edges, node_list, verbose = verbose)

	#	Collect
		rows <- list()
		results <- list()

	#	Iterating Over Specifications
		for (s in seq_len(nrow(specs))) {
			tag <- pa_spec_tag(specs$alpha[s], specs$anchor[s],
							   specs$anchor_era[s])
			if (verbose) cat("\nSpecification ", tag, "\n", sep = "")

			maps <- pa_build_era_maps(node_list, eras = eras,
									  grid = specs$grid[s],
									  alpha = specs$alpha[s],
									  anchor = specs$anchor[s],
									  anchor_era = specs$anchor_era[s],
									  verbose = verbose)

			net <- pa_community_network(edges, maps, node_list)
			spec_dir <- file.path(output_dir, tag)
			dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(net$nodes,
							 file.path(spec_dir, "community_nodes.csv"))
			readr::write_csv(net$edges,
							 file.path(spec_dir, "community_edges.csv"))

			if (verbose) {
				cat("\nWhere the citations went\n")
				print(net$retention, row.names = FALSE)
				cat("\nCommunity network: ", nrow(net$nodes), " node(s), ",
					nrow(net$edges), " distinct pair(s), ",
					format(sum(net$edges$weight), big.mark = ","),
					" citation(s), ", attr(net$edges, "n_self_loops"),
					" self-loop(s)\n", sep = "")
				cat("Isolates: ",
					sum(net$nodes$out_citations == 0 &
						net$nodes$in_citations == 0), "\n", sep = "")
			}

			assignments <- NULL
			if (isTRUE(fit)) {
				if (verbose) cat("\n--- handing to graph-tool ---\n")
				fitted <- pa_fit_literatures(spec_dir, verbose = verbose)
				assignments <- fitted$assignments
			}

			results[[tag]] <- list(nodes = net$nodes, edges = net$edges,
								   retention = net$retention,
								   assignments = assignments)

			rows[[length(rows) + 1L]] <- data.frame(
				spec_tag        = tag,
				n_communities   = nrow(net$nodes),
				n_pairs         = nrow(net$edges),
				n_citations     = sum(net$edges$weight),
				n_self_loops    = attr(net$edges, "n_self_loops"),
				pct_resolved    = net$retention$pct[4],
				n_isolates      = sum(net$nodes$out_citations == 0 &
									  net$nodes$in_citations == 0),
				n_literatures_l0 = if (is.null(assignments)) NA_integer_ else
									 length(unique(assignments$literature_l0)),
				stringsAsFactors = FALSE)
		}

	#	Bind and Write
		summary <- do.call(rbind, rows)
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
		readr::write_csv(summary, file.path(output_dir, "literature_summary.csv"))

	#	Check
		first <- results[[1L]]
		tests <- pa_run_literature_tests(first$nodes, first$edges,
										 first$assignments, output_dir)

	#	Report
		if (isTRUE(verbose)) {
			cat("\nLiteratures by specification\n")
			print(summary, row.names = FALSE)
			cat("\n")
			print(tests[c("check", "result", "detail")], row.names = FALSE)
			n_fail <- sum(tests$result == "FAIL")
			cat("\n", sum(tests$result == "PASS"), " of ",
				sum(tests$result != "SKIP"), " check(s) passed",
				if (n_fail > 0L) sprintf(", %d FAILED", n_fail) else "",
				"\n", sep = "")
		}

	#	Assembling Result
		return(invisible(list(results = results, summary = summary,
							  tests = tests)))
}

#' @title pa_run_literature_tests
#' @description Verify the community network and the fit against what they did not use.
#'
#' @param nodes The community node table.
#' @param edges The community edge table.
#' @param assignments The fitted assignments, or NULL.
#' @param output_dir Destination for the test table, or NULL.
#'
#' @details
#' The checks that would catch a silent error. A citation count that does not
#' survive the collapse means the lookup dropped rows. A community in the
#' network but not in the fit means the join will be incomplete when
#' literature_i is attached. And group counts that rise up the hierarchy mean
#' the nesting was read wrong -- the mistake the first graph-tool probe made.
#'
#' @return A data.frame with check, result and detail.
#'
#' @examples
#' tests <- pa_run_literature_tests(nodes, edges, assignments)
#'
#' @export
pa_run_literature_tests <- function(nodes, edges, assignments = NULL,
									output_dir = LIT_OUT_DIR) {
	#	"""
	#	Args:
	#		nodes: community node table
	#		edges: community edge table
	#		assignments: fitted assignments, or NULL
	#		output_dir: destination, or NULL
	#	Returns:
	#		data.frame(check, result, detail)
	#	"""

	#	Collect
		rows <- list()
		record <- function(check, result, detail) {
			rows[[length(rows) + 1L]] <<- data.frame(
				check = check, result = result, detail = detail,
				stringsAsFactors = FALSE)
		}
		nd <- as.data.table(nodes)
		ed <- as.data.table(edges)

	#	Every Endpoint Is a Known Community
		unknown <- length(setdiff(union(ed$community_i, ed$community_j),
								  nd$community_key))
		record("every edge endpoint is a node",
			   if (unknown == 0L) "PASS" else "FAIL",
			   sprintf("%d unknown key(s)", unknown))

	#	Strength Reconciles With the Edge List
	#	The node table's degrees were computed separately from the edges, so
	#	their totals agreeing is a real check rather than a tautology.
		total_edge <- sum(as.numeric(ed$weight))
		total_out  <- sum(as.numeric(nd$out_citations))
		total_in   <- sum(as.numeric(nd$in_citations))
		record("out- and in-strength both sum to the edge weight",
			   if (isTRUE(all.equal(total_out, total_edge)) &&
				   isTRUE(all.equal(total_in, total_edge))) "PASS" else "FAIL",
			   sprintf("%.0f edges, %.0f out, %.0f in", total_edge,
					   total_out, total_in))

	#	Community Keys Are Unique
		record("one row per community",
			   if (!any(duplicated(nd$community_key))) "PASS" else "FAIL",
			   sprintf("%d duplicate key(s)", sum(duplicated(nd$community_key))))

	#	Weights Are Positive Counts
		record("every weight is a positive integer",
			   if (all(ed$weight > 0) &&
				   all(ed$weight == as.integer(ed$weight))) "PASS" else "FAIL",
			   sprintf("min %s, max %s", min(ed$weight), max(ed$weight)))

	#	The Fit Covers the Network
		if (!is.null(assignments) && nrow(assignments) > 0L) {
			ass <- as.data.table(assignments)
			missing <- length(setdiff(nd$community_key, ass$community_key))
			record("every community carries a literature",
				   if (missing == 0L) "PASS" else "FAIL",
				   sprintf("%d community(s) absent from the fit", missing))

			#	The Hierarchy Narrows
				levels <- grep("^literature_l", names(ass), value = TRUE)
				levels <- levels[order(as.integer(sub("literature_l", "",
													  levels)))]
				counts <- vapply(levels,
								 function(col) length(unique(ass[[col]])),
								 integer(1))
				record("group counts do not rise up the hierarchy",
					   if (all(diff(counts) <= 0L)) "PASS" else "FAIL",
					   paste(counts, collapse = " -> "))
		} else {
			record("every community carries a literature", "SKIP",
				   "no fit supplied")
			record("group counts do not rise up the hierarchy", "SKIP",
				   "no fit supplied")
		}

	#	Assembling Result
		tests <- do.call(rbind, rows)
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests,
							 file.path(output_dir, "literature_tests.csv"))
		}
		return(tests)
}

########################
#   RUNNING THE LANE   #
########################

#	Run Under Rscript, Not Under source()
	if (sys.nframe() <= 1L) {

		cat("PathA_Literatures.R -- the community network and its literatures\n")
		cat("================================================================\n\n")

		#	Dependencies
			PA_ROOT    <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")
			PA_SCRIPTS <- file.path(PA_ROOT, "scripts")
			deps <- c("RPajekFunctions_30April2023.r",
					  "Path_A/PathA_PajekReaders.R",
					  "Path_A/PathA_EraResolution.R",
					  "Path_A/PathA_TieLayer.R")
			cat("Sourcing dependencies...\n")
			for (dep in deps) {
				path <- file.path(PA_SCRIPTS, dep)
				if (!file.exists(path)) {
					stop("PathA_Literatures.R: dependency not found: ", path)
				}
				source(path)
				cat("  loaded ", basename(dep), "\n", sep = "")
			}

		#	Preflight
			cat("\nPreflight...\n")
			preflight <- data.frame(
				what = c("node list", "edge list", "graph-tool", "fitter"),
				path = c(file.path(LIT_ERA_DIR, "pa_node_list.Rda"),
						 file.path(LIT_ERA_DIR, "pa_edge_list.Rda"),
						 LIT_GT_PYTHON, LIT_FIT_SCRIPT),
				stringsAsFactors = FALSE)
			preflight$found <- file.exists(preflight$path)
			print(preflight, row.names = FALSE)
			if (!all(preflight$found)) {
				stop("PathA_Literatures.R: missing: ",
					 paste(preflight$what[!preflight$found], collapse = ", "))
			}

		#	Run
			cat("\n")
			started <- Sys.time()
			res <- pa_run_literatures()

			cat("\n================================================================\n")
			cat("Built in ", round(as.numeric(difftime(Sys.time(), started,
													   units = "secs")), 1),
				"s\n", sep = "")
			cat("Output: ", LIT_OUT_DIR, "\n", sep = "")
			if (any(res$tests$result == "FAIL")) {
				cat("FAILED checks present\n")
				quit(status = 1L)
			}
			cat("OK\n")
	}
