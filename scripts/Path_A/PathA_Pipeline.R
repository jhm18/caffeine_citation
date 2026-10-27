#	PathA_Pipeline.R -- The Single Entry Point for Path A
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	27 October 2026
#
#	WHAT THIS IS. The one file you source. It owns the load order, the preflight,
#	the stage sequence, and the run log, so that none of those is duplicated
#	anywhere else. Before it existed, the load order lived in the test suite's
#	source block and in whatever a runner happened to do, and nothing caught a
#	disagreement between them.
#
#		source("scripts/Path_A/PathA_Pipeline.R")
#		res <- pa_pipeline()
#
#	WHAT IT DOES NOT DO. It does not run the era construction and it does not run
#	the Julia community detection. Those are once-only jobs measured in hours, one
#	of them in another language, and folding them in would make a ninety-minute
#	Julia sweep a "stage" of an R script. What the pipeline does instead is VERIFY
#	their outputs exist and RECORD their provenance -- size and modification time --
#	in the run log, so a set of descent ties can always be traced back to the
#	construction and the partitions that produced it.
#
#	THE SENTINEL CHECK, AND WHY IT IS HERE. Every file in the load order declares
#	one function it must define. After sourcing, the pipeline checks for it. This
#	exists because a full-file rewrite of PathA_Granularity.R silently dropped four
#	functions, and the failure surfaced only when a downstream call hit a missing
#	name twenty minutes later. A sentinel turns that into an error at load.
#
#	STAGES. Declared in order, each one skippable:
#
#		granularity   measure every partition on disk, validate the calibration
#		tie_layer     bridging works, descent ties, the risk set, per specification
#		test          the foundation suite of S14.4
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

#####################
#   CONFIGURATION   #
#####################

#	Project Root
	PA_ROOT    <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")
	PA_SCRIPTS <- file.path(PA_ROOT, "scripts")
	PA_TESTS   <- file.path(PA_ROOT, "/data/tie_tests")
	PA_LOG_DIR <- file.path(PA_ROOT, "data/run_logs")

#	Stages, in Order
	PA_STAGES <- c("granularity", "tie_layer", "test")

#	The Test Suite
#	A single declared path rather than a search over candidate locations. The
#	preflight checks it whenever the test stage is requested, so a missing suite
#	fails in seconds instead of after the tie layer has run.
	PA_TEST_FILE <- file.path(PA_TESTS, "Test_PathA_TieLayer.R")

#	Upstream Outputs the Pipeline Consumes but Does Not Produce
#	Verified and recorded, never rebuilt here. The era construction and the Julia
#	detection are separate once-only jobs.
	PA_UPSTREAM <- c("data/eras_7yr/pa_node_list.Rda",
					 "data/eras_7yr/pa_edge_list.Rda",
					 "data/eras_7yr/era_construction_summary.csv",
					 "data/eras_7yr/era_construction_tests.csv")

###############
#   HELPERS   #
###############

#	The Load Order, as Data
	pa_pipeline_files <- function(scripts = PA_SCRIPTS) {
		#	"""
		#	Args:
		#		scripts: the scripts directory
		#	Returns:
		#		data.frame(order, lane, file, sentinel)
		#	Notes:
		#		The load order is data rather than a run of source() calls, so the test
		#		suite and any runner read it from one place. sentinel names a function
		#		the file must define; see the header for why.
		#
		#		RPajekFunctions_30April2023.r comes first because PathA_PajekReaders.R
		#		wraps its read_net(). PathA_EraResolution.R needs the readers, and
		#		PathA_TieLayer.R needs the era maps. PathA_Granularity.R is independent
		#		of the other three and is loaded last.
		#	"""

		#	Declare
			f <- c("RPajekFunctions_30April2023.r",
				   "Path_A/PathA_PajekReaders.R",
				   "Path_A/PathA_EraResolution.R",
				   "Path_A/PathA_TieLayer.R",
				   "Path_A/PathA_Granularity.R")
			sentinel <- c("read_net",
						  "pa_read_era_network",
						  "pa_era_map",
						  "pa_run_tie_layer",
						  "pa_run_granularity")
			lane <- c("pajek", "pajek", "era", "tie", "granularity")

		#	Assembling Result
			return(data.frame(order    = seq_along(f),
							  lane     = lane,
							  file     = file.path(scripts, f),
							  sentinel = sentinel,
							  stringsAsFactors = FALSE))
	}

#	Provenance of One File
	pa_file_stamp <- function(path) {
		#	"""
		#	Args:
		#		path: a file path
		#	Returns:
		#		data.frame(path, exists, bytes, modified)
		#	Notes:
		#		Recorded for every input and every loaded script, so a result can be
		#		traced to the code and the data that produced it without relying on
		#		anyone having written it down.
		#	"""

		#	Stamp
			ok <- file.exists(path)

		#	Assembling Result
			return(data.frame(path     = path,
							  exists   = ok,
							  bytes    = if (ok) file.size(path) else NA_real_,
							  modified = if (ok) {
											 format(file.mtime(path),
													"%Y-%m-%dT%H:%M:%S")
										 } else {
											 NA_character_
										 },
							  stringsAsFactors = FALSE))
	}

#	Record One Stage Result
	pa_stage_row <- function(stage, status, seconds, detail = "") {
		#	"""
		#	Args:
		#		stage: stage name
		#		status: "OK", "FAILED", or "SKIPPED"
		#		seconds: elapsed time
		#		detail: message or summary
		#	Returns:
		#		one-row data.frame
		#	"""

		#	Assembling Result
			return(data.frame(stage    = stage,
							  status   = status,
							  seconds  = round(as.numeric(seconds), 1),
							  detail   = as.character(detail),
							  finished = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
							  stringsAsFactors = FALSE))
	}

#########################
#   PRIMARY FUNCTIONS   #
#########################

#' @title pa_load_pipeline
#' @description Source every Path A file in the declared order and verify each one defined what it should.
#'
#' @param scripts The scripts directory. Default PA_SCRIPTS.
#' @param force Reload even if the pipeline appears already loaded. Default FALSE.
#' @param verbose Report each file as it loads. Default TRUE.
#'
#' @details
#' The load order is read from pa_pipeline_files rather than written out as a run
#' of source() calls, so the test suite and any runner cannot disagree about it.
#'
#' After each file is sourced, the function it declares as its sentinel must exist.
#' That check is here because a full-file rewrite once dropped four functions from
#' PathA_Granularity.R and the failure surfaced only when a downstream call hit the
#' missing name. A sentinel turns a silently truncated file into an error at load.
#'
#' @return Invisibly, the load table with a loaded column.
#'
#' @examples
#' pa_load_pipeline()
#'
#' @export
pa_load_pipeline <- function(scripts = PA_SCRIPTS, force = FALSE, verbose = TRUE) {
	#	"""
	#	Args:
	#		scripts: the scripts directory
	#		force: reload even if already loaded
	#		verbose: report each file
	#	Returns:
	#		invisible data.frame, the load table plus a loaded column
	#	"""

	#	The Load Order
		tab <- pa_pipeline_files(scripts)
		tab$loaded <- FALSE

	#	Early return
		if (!force && all(vapply(tab$sentinel, exists, logical(1),
								 mode = "function"))) {
			if (verbose) cat("Pipeline already loaded; pass force = TRUE to reload\n")
			tab$loaded <- TRUE
			return(invisible(tab))
		}

	#	Iterating Over the Load Order
		for (i in seq_len(nrow(tab))) {
			#	Validation
				if (!file.exists(tab$file[i])) {
					stop("pa_load_pipeline: file not found: ", tab$file[i])
				}

			#	Source
				source(tab$file[i])

			#	The sentinel check
				if (!exists(tab$sentinel[i], mode = "function")) {
					stop("pa_load_pipeline: ", basename(tab$file[i]),
						 " sourced without defining ", tab$sentinel[i], "(). ",
						 "The file is truncated, or a rewrite dropped it.")
				}
				tab$loaded[i] <- TRUE
				if (verbose) {
					cat("  loaded ", basename(tab$file[i]), " (", tab$lane[i], ")\n",
						sep = "")
				}
		}

	#	Assembling Result
		return(invisible(tab))
}

#' @title pa_preflight
#' @description Verify every input the pipeline needs, before anything expensive runs.
#'
#' @param specs Specification table. Default NULL, resolved to TL_SPECS after loading.
#' @param eras Integer vector of eras. Default NULL, resolved to PA_ANALYTIC_ERAS.
#' @param scripts The scripts directory. Default PA_SCRIPTS.
#' @param root The project root. Default PA_ROOT.
#'
#' @details
#' Fails the whole run in seconds rather than twenty minutes into it. Three groups
#' of check: the scripts in the load order exist; the upstream outputs the pipeline
#' consumes exist; and every era resolves to a network and a partition whose
#' lengths agree, for every specification requested.
#'
#' Returns rather than signalling, so the caller can decide. pa_pipeline stops on a
#' fatal preflight by default.
#'
#' @return A list with ok, checks and stamps.
#'
#' @export
pa_preflight <- function(specs = NULL, eras = NULL, stages = PA_STAGES,
						 scripts = PA_SCRIPTS, root = PA_ROOT) {
	#	"""
	#	Args:
	#		specs: specification table, or NULL for TL_SPECS
	#		eras: integer eras, or NULL for PA_ANALYTIC_ERAS
	#		stages: the stages about to run, so their inputs can be checked
	#		scripts: the scripts directory
	#		root: the project root
	#	Returns:
	#		list(ok, checks, stamps)
	#	Notes:
	#		The defaults are NULL rather than TL_SPECS and PA_ANALYTIC_ERAS because
	#		those constants come from sourced files and may not exist when this
	#		function's formals are evaluated.
	#
	#		stages is taken so that a stage's own inputs are checked before anything
	#		runs. The first version did not do this, and a missing test suite was
	#		discovered only after the tie layer had spent two minutes building
	#		output -- which is exactly what a preflight exists to prevent.
	#	"""

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- get("TL_SPECS")
		if (is.null(eras))  eras  <- get("PA_ANALYTIC_ERAS")

	#	Scripts
		checks <- list()
		tab <- pa_pipeline_files(scripts)
		missing_scripts <- tab$file[!file.exists(tab$file)]
		checks[[length(checks) + 1L]] <- data.frame(
			group  = "scripts",
			check  = "every file in the load order exists",
			ok     = length(missing_scripts) == 0L,
			detail = if (length(missing_scripts) == 0L) {
						 paste(nrow(tab), "files")
					 } else {
						 paste("missing:", paste(basename(missing_scripts),
												 collapse = ", "))
					 },
			stringsAsFactors = FALSE)

	#	Upstream Outputs
		up <- file.path(root, PA_UPSTREAM)
		missing_up <- up[!file.exists(up)]
		checks[[length(checks) + 1L]] <- data.frame(
			group  = "upstream",
			check  = "the era construction's outputs exist",
			ok     = length(missing_up) == 0L,
			detail = if (length(missing_up) == 0L) {
						 paste(length(up), "files")
					 } else {
						 paste("missing:", paste(basename(missing_up),
												 collapse = ", "))
					 },
			stringsAsFactors = FALSE)

	#	Era Node Tables
		era_files <- file.path(root, "data/eras_7yr",
							   sprintf("era%02d_nodes.Rda", eras))
		missing_era <- era_files[!file.exists(era_files)]
		checks[[length(checks) + 1L]] <- data.frame(
			group  = "upstream",
			check  = "every era has a node table",
			ok     = length(missing_era) == 0L,
			detail = if (length(missing_era) == 0L) {
						 paste(length(era_files), "eras")
					 } else {
						 paste("missing:", paste(basename(missing_era),
												 collapse = ", "))
					 },
			stringsAsFactors = FALSE)

	#	Networks and Partitions, per Specification
		for (s in seq_len(nrow(specs))) {
			tag <- pa_spec_tag(specs$alpha[s], specs$anchor[s], specs$anchor_era[s])
			res <- tryCatch({
				man <- pa_era_manifest(eras, specs$alpha[s], specs$anchor[s],
									   specs$anchor_era[s], specs$grid[s])
				list(ok = all(man$lengths_agree),
					 detail = if (all(man$lengths_agree)) {
								  paste(nrow(man), "eras resolve, lengths agree")
							  } else {
								  paste("length mismatch for era(s):",
										paste(man$era[!man$lengths_agree],
											  collapse = ", "))
							  })
			}, error = function(e) list(ok = FALSE, detail = conditionMessage(e)))
			checks[[length(checks) + 1L]] <- data.frame(
				group  = "partitions",
				check  = paste("specification", tag),
				ok     = res$ok,
				detail = res$detail,
				stringsAsFactors = FALSE)
		}

	#	The Test Suite, When the Test Stage Is Requested
		if ("test" %in% stages) {
			checks[[length(checks) + 1L]] <- data.frame(
				group  = "stages",
				check  = "the test suite exists",
				ok     = file.exists(PA_TEST_FILE),
				detail = if (file.exists(PA_TEST_FILE)) {
							 PA_TEST_FILE
						 } else {
							 paste("not found:", PA_TEST_FILE)
						 },
				stringsAsFactors = FALSE)
		}

	#	Provenance Stamps
		stamp_files <- c(tab$file, up, era_files)
		if ("test" %in% stages) stamp_files <- c(stamp_files, PA_TEST_FILE)
		stamps <- do.call("rbind", lapply(stamp_files, pa_file_stamp))

	#	Assembling Result
		out <- do.call("rbind", checks)
		rownames(out) <- NULL
		return(list(ok = all(out$ok), checks = out, stamps = stamps))
}

#' @title pa_pipeline
#' @description Preflight, run the requested stages in order, test, and write a run log.
#'
#' @param stages Stages to run, in the order declared by PA_STAGES. Default PA_STAGES.
#' @param specs Specification table. Default NULL, resolved to TL_SPECS.
#' @param eras Integer vector of eras. Default NULL, resolved to PA_ANALYTIC_ERAS.
#' @param preflight_only Stop after the preflight. Default FALSE.
#' @param stop_on_fail Stop when a stage fails. Default TRUE.
#' @param log_dir Destination for the run log. Default PA_LOG_DIR.
#'
#' @details
#' The pipeline loads the files, preflights, then runs each requested stage in the
#' declared order, timing each one. A stage that fails is recorded and, by default,
#' stops the run -- a tie layer built on a failed granularity validation is worse
#' than no tie layer.
#'
#' Three files go into the run log directory, stamped with the run time: the stage
#' table, the preflight checks, and the provenance stamps of every script and input.
#' That last one is what lets a set of descent ties be traced back to the
#' construction and partitions that produced it.
#'
#' The era construction and the Julia community detection are not stages. They are
#' once-only jobs, one of them in another language, and the pipeline verifies and
#' records their outputs rather than driving them.
#'
#' @return Invisibly, a list with stages, preflight, results and log_paths.
#'
#' @examples
#' res <- pa_pipeline()
#' res$stages
#'
#' res <- pa_pipeline(preflight_only = TRUE)
#' res$preflight$checks
#'
#' @export
pa_pipeline <- function(stages = PA_STAGES, specs = NULL, eras = NULL,
						preflight_only = FALSE, stop_on_fail = TRUE,
						log_dir = PA_LOG_DIR) {
	#	"""
	#	Args:
	#		stages: stages to run
	#		specs: specification table, or NULL for TL_SPECS
	#		eras: integer eras, or NULL for PA_ANALYTIC_ERAS
	#		preflight_only: stop after the preflight?
	#		stop_on_fail: stop when a stage fails?
	#		log_dir: destination for the run log
	#	Returns:
	#		invisible list(stages, preflight, results, log_paths)
	#	"""

	#	Validation
		unknown <- setdiff(stages, PA_STAGES)
		if (length(unknown) > 0L) {
			stop("pa_pipeline: unknown stage(s): ", paste(unknown, collapse = ", "),
				 ". Known stages: ", paste(PA_STAGES, collapse = ", "))
		}

	#	Load
		run_started <- Sys.time()
		cat("Path A pipeline\n")
		cat("===============\n\n")
		cat("Loading...\n")
		loaded <- pa_load_pipeline()

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- TL_SPECS
		if (is.null(eras))  eras  <- PA_ANALYTIC_ERAS

	#	Preflight
		cat("\nPreflight...\n")
		pre <- pa_preflight(specs, eras, stages)
		print(pre$checks[, c("group", "check", "ok", "detail")], row.names = FALSE)
		if (!pre$ok) {
			cat("\nPreflight FAILED\n")
			if (stop_on_fail) {
				stop("pa_pipeline: preflight failed; nothing was run")
			}
		}

	#	Early return
		if (preflight_only) {
			cat("\nPreflight only; stopping before the stages\n")
			return(invisible(list(stages = data.frame(), preflight = pre,
								  results = list(), log_paths = character(0))))
		}

	#	Iterating Over Stages, in the Declared Order
		rows <- list()
		results <- list()
		for (st in PA_STAGES) {
			if (!(st %in% stages)) {
				rows[[length(rows) + 1L]] <- pa_stage_row(st, "SKIPPED", 0,
														  "not requested")
				next
			}
			cat("\nStage: ", st, "\n", sep = "")
			t0 <- Sys.time()
			res <- tryCatch({
				out <- switch(st,
					granularity = pa_run_granularity(),
					tie_layer   = pa_run_tie_layer(specs = specs, eras = eras),
					test        = {
						if (!file.exists(PA_TEST_FILE)) {
							stop("test file not found: ", PA_TEST_FILE,
								 " -- the preflight should have caught this")
						}

						#	Sourced into the global environment, NOT a local one.
						#	The suite's check() increments .tests_run with <<-, and
						#	<<- begins its search at the PARENT of the function's
						#	enclosing environment. Under source(local = new.env())
						#	the counters live in that new environment, <<- skips it,
						#	assigns into globalenv instead, and the suite's final
						#	report reads a counter that never moved -- so it prints
						#	"0 of 0 checks passed / OK" however many checks failed.
							source(PA_TEST_FILE)
							"suite completed"
					})
				list(ok = TRUE, value = out, detail = "")
			}, error = function(e) {
				list(ok = FALSE, value = NULL, detail = conditionMessage(e))
			})
			secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

			#	Record
				detail <- if (res$ok) {
							  switch(st,
								  granularity = sprintf("%d partition(s) measured",
														if (is.null(res$value$profiles)) {
															0L
														} else {
															nrow(res$value$profiles)
														}),
								  tie_layer   = sprintf("%d pair-specification row(s)",
														if (is.null(res$value$summary)) {
															0L
														} else {
															nrow(res$value$summary)
														}),
								  test        = as.character(res$value))
						  } else {
							  res$detail
						  }
				rows[[length(rows) + 1L]] <- pa_stage_row(
					st, if (res$ok) "OK" else "FAILED", secs, detail)
				results[[st]] <- res$value
				cat("  ", st, ": ", if (res$ok) "OK" else "FAILED", " in ",
					round(secs, 1), "s -- ", detail, "\n", sep = "")
				if (!res$ok && stop_on_fail) {
					stage_tab <- do.call("rbind", rows)
					pa_write_run_log(stage_tab, pre, run_started, log_dir)
					stop("pa_pipeline: stage ", st, " failed: ", res$detail)
				}
		}

	#	Write the Run Log
		stage_tab <- do.call("rbind", rows)
		rownames(stage_tab) <- NULL
		log_paths <- pa_write_run_log(stage_tab, pre, run_started, log_dir)

	#	Report
		cat("\n===============\n")
		print(stage_tab[, c("stage", "status", "seconds", "detail")],
			  row.names = FALSE)
		cat("\nRun log: ", dirname(log_paths[[1L]]), "\n", sep = "")
		cat(if (all(stage_tab$status != "FAILED")) "OK\n" else "FAILED\n")

	#	Assembling Result
		return(invisible(list(stages = stage_tab, preflight = pre,
							  results = results, log_paths = log_paths)))
}

#' @title pa_write_run_log
#' @description Write the stage table, the preflight checks, and the provenance stamps of one run.
#'
#' @param stage_tab The stage table from pa_pipeline.
#' @param preflight The preflight result from pa_preflight.
#' @param run_started The run's start time.
#' @param log_dir Destination directory. Default PA_LOG_DIR.
#'
#' @details
#' Three files under log_dir/run_<timestamp>/. The stamps file is the one that
#' matters later: it records the size and modification time of every script and
#' every input, so a set of descent ties can be traced to the construction and the
#' partitions that produced it without anyone having written it down.
#'
#' @return A character vector of the paths written.
#'
#' @export
pa_write_run_log <- function(stage_tab, preflight, run_started,
							 log_dir = PA_LOG_DIR) {
	#	"""
	#	Args:
	#		stage_tab: stage table
	#		preflight: preflight result
	#		run_started: POSIXct start time
	#		log_dir: destination directory
	#	Returns:
	#		character vector of paths written
	#	"""

	#	One Directory per Run
		stamp <- format(run_started, "%Y%m%dT%H%M%S")
		dir <- file.path(log_dir, paste0("run_", stamp))
		dir.create(dir, recursive = TRUE, showWarnings = FALSE)

	#	Write
		p1 <- file.path(dir, "stages.csv")
		p2 <- file.path(dir, "preflight.csv")
		p3 <- file.path(dir, "provenance.csv")
		readr::write_csv(stage_tab, p1)
		readr::write_csv(preflight$checks, p2)
		readr::write_csv(preflight$stamps, p3)

	#	Assembling Result
		return(c(p1, p2, p3))
}

#############################
#   HOW TO RUN THIS LANE    #
#############################

#	NO MAIN BLOCK, DELIBERATELY. Sourcing this file must load the pipeline and
#	nothing more, so that the preflight can be inspected before anything expensive
#	runs and so the test suite can source it without triggering a build.
#
#		source("scripts/Path_A/PathA_Pipeline.R")
#
#		res <- pa_pipeline(preflight_only = TRUE)     # check the inputs
#		res <- pa_pipeline()                          # everything
#		res <- pa_pipeline(stages = "tie_layer")      # one stage
#		res <- pa_pipeline(stages = c("tie_layer", "test"), stop_on_fail = FALSE)
#
#	UPSTREAM, NOT STAGES. Run these by hand when the construction changes:
#
#		PathA_Revised_Eras.R          the node list, edge list, era networks
#		champ_sweep_new_eras.jl       the calibration curves
#		the partition writer          the specification-tagged .clu files
#
#	The pipeline verifies their outputs in the preflight and records their size and
#	modification time in every run log.
