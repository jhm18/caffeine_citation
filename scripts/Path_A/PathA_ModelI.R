#	PathA_ModelI.R
#	The Model I frame, less the columns the blockmodel has to supply.
#
#	Jonathan H. Morgan and Sarah Delmar
#	03 November 2026
#
#	One row per ordered community pair: an era-t community i against an
#	era-(t-1) community j, zeros included (S13.2). The outcome is n_coupling,
#	the number of works both communities cite -- bibliographic coupling, not
#	descent. Exposure is carried as the components the spec names, R_i and R_j,
#	with their product and the pair's degree correction folded into
#	log_exposure for the offset.
#
#	NOTHING IS ESTIMATED HERE, AND NOTHING CAN BE YET. This lane assembles and
#	checks. It cannot produce a fittable design matrix, because S13.2 puts an
#	ordering dependency ahead of it: literature_i and literature_j come from the
#	blockmodel, and topical_prox is the S_ij that both of Model I's headline
#	coefficients multiply. See MI_PENDING, which is printed on every run and
#	written beside the output so the gap travels with the file.
#
#	WHAT IS JOINED ONTO WHAT. The grid and the exposure come from the coupling
#	lane, already expanded and zero-filled by pa_load_coupling_grid. The
#	community-level covariates come from the covariate lane's profiles. The
#	three pair-level covariates are computed here, by calling pa_pair_overlap
#	and pa_pair_distance against the covariate lane's set tables.
#
#	HOW BIG THIS ACTUALLY IS. Communities against communities: the largest era
#	pair is 1,105 x 760 = 839,800 ordered pairs and all six together are 1.87
#	million rows, a few hundred megabytes. An earlier version of this file
#	planned around 15.3 million rows and wrote one file per pair to avoid ever
#	binding them. That figure was Model II's grid -- bridging WORKS against
#	citing communities (S13.1) -- and it does not apply here. The per-pair files
#	are still written, because a pair is the unit a check reconciles against,
#	but they are bound at the end into one design matrix, which is the object
#	the model is actually fitted to.
#
#	SHARES AND COUNTS. The community-level cognitive measures are carried both
#	ways: cog_subject as a share, and n_core_title over n_scored as the counts it
#	came from. The share is for reading; the counts are for fitting. Most
#	communities in this corpus are small -- the median holds six vertices, and
#	915 of era 17's 1,105 communities fall below the covariate lane's
#	five-article floor -- so a share is NA or a very noisy fraction for most of
#	the grid, while the counts are exactly what they are and let a three-article
#	community contribute three articles' worth of evidence.
#
#	Input:
#		data/coupling/<spec_tag>/pair_XX_YY_*.csv        pa_run_coupling()
#		data/covariates/<spec_tag>/community_profiles.csv pa_run_covariates()
#		data/covariates/<spec_tag>/community_*.rds        pa_run_covariates()
#
#	Output:
#		data/model_i/<spec_tag>/pair_XX_YY_frame.rds
#		data/model_i/<spec_tag>/plots/*.png
#		data/model_i/model_i_frame.rds
#		data/model_i/design_summary.csv
#		data/model_i/design_tests.csv
#		data/model_i/frame_pending.csv

#################
#   LIBRARIES   #
#################

	suppressPackageStartupMessages({
		library(data.table)
		library(Matrix)
		library(readr)
	})

#####################
#   CONFIGURATION   #
#####################

#	Project Root
	MI_ROOT <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")

#	Inputs
	MI_COUPLING_DIR  <- file.path(MI_ROOT, "data/coupling")
	MI_COVARIATE_DIR <- file.path(MI_ROOT, "data/covariates")

#	Output
	MI_OUT_DIR <- file.path(MI_ROOT, "data/model_i")

#	Where a Community Pair Shares No Research Area
#	Hellinger distance is one when two profiles have nothing in common, so the
#	fill is one rather than zero. Getting this backwards would make the most
#	distant pairs look like the closest, and it would not show up in any range
#	check, because one is inside [0, 1].
	MI_DISTANCE_FILL <- 1

#	The Plot Sample
#	A run-sequence plot of 15 million points is a filled rectangle. The four-plots
#	are drawn from a sample of this many rows per pair, drawn without replacement
#	and ordered by pair_index, so the run-sequence panel reads as six blocks and
#	shows what matters at this stage: whether a covariate shifts between pairs and
#	is therefore partly collinear with the time trend.
	MI_PLOT_SAMPLE <- 2000L
	MI_WRITE_PLOTS <- TRUE

#	Reproducibility
	MI_SEED <- 20261103L

#	What S13.2 Asks For, and What Is Not Here Yet
#	The spec names an ordering dependency and this lane sits on the wrong side of
#	it: literature_i and literature_j come from the blockmodel of the literatures
#	section, "which means this table cannot be built until that has been fit --
#	the only ordering dependency between the two models."
#
#	topical_prox is the larger gap. S_ijp is what both of Model I's headline
#	terms multiply -- alpha_{S,p}, the proximity slope drifting across era pairs,
#	and alpha_{S,l}, the same slope varying by literature. Without it there is no
#	alpha_S to estimate, and the frame carries only the outcome, the exposure and
#	the z nuisance covariates.
#
#	So what this lane writes is not a fittable design matrix. It is the part of
#	the frame that does not depend on the blockmodel, and it is named and
#	reported as such. The missing columns attach later by (era, community) and
#	by pair, so nothing here has to be rebuilt -- but nothing here should be fit
#	either.
	MI_PENDING <- data.frame(
		column = c("topical_prox", "literature_i", "literature_j"),
		role   = c("covariate", "grouping", "grouping"),
		needs  = c("S_ij from text and venue; the titles-vs-abstracts attenuation test first",
				   "the nested degree-corrected stochastic blockmodel",
				   "the nested degree-corrected stochastic blockmodel"),
		carries = c("alpha_{S,p} and alpha_{S,l}", "alpha_{S,l}", "alpha_{S,l}"),
		stringsAsFactors = FALSE)

###########################
#   ASSEMBLING ONE PAIR   #
###########################

#' @title pa_model_i_pair
#' @description Assemble the Model I design matrix for one era pair.
#'
#' @param spec_tag Specification tag.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#' @param pair_index Position of the pair in the window, 1 to 6.
#' @param profiles The covariate lane's community profiles for this specification.
#' @param authors The covariate lane's author sets.
#' @param venues The covariate lane's venue sets.
#' @param areas The covariate lane's research-area sets.
#' @param coupling_dir Coupling output root. Default MI_COUPLING_DIR.
#'
#' @details
#' Built by reference throughout. Every covariate is added to the grid with :=
#' rather than by binding a second table to it, which keeps one copy of the
#' grid rather than one per join.
#'
#' The fills are not cosmetic. Two communities with no author in common have an
#' author overlap of exactly zero, which is a measurement and not a missing
#' value; two with no research area in common are at maximal Hellinger distance,
#' which is one. pa_pair_overlap and pa_pair_distance return only the cells they
#' found, so what they leave out is what gets filled.
#'
#' @return A data.table, one row per ordered community pair.
#'
#' @examples
#' design <- pa_model_i_pair("alpha1.00_g120_e14", 16, 17, 6, profiles,
#'                           authors, venues, areas)
#'
#' @export
pa_model_i_pair <- function(spec_tag, era_prior, era_t, pair_index, profiles,
							authors, venues, areas,
							coupling_dir = MI_COUPLING_DIR) {
	#	"""
	#	Args:
	#		spec_tag: specification tag
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#		pair_index: position of the pair in the window
	#		profiles: community profiles for this specification
	#		authors, venues, areas: the covariate lane's set tables
	#		coupling_dir: coupling output root
	#	Returns:
	#		data.table, one row per ordered community pair
	#	"""

	#	Validation
		for (fn in c("pa_load_coupling_grid", "pa_pair_overlap",
					 "pa_pair_distance")) {
			if (!exists(fn, mode = "function")) {
				stop("pa_model_i_pair: ", fn, "() not found; source ",
					 "PathA_Coupling.R and PathA_Covariates.R first")
			}
		}

	#	The Grid and the Exposure
		grid <- pa_load_coupling_grid(spec_tag, era_prior, era_t, coupling_dir)
		rows <- sort(unique(grid$community_i))
		cols <- sort(unique(grid$community_j))

	#	Identifiers
		grid[, `:=`(spec_tag = spec_tag,
					era_prior = as.integer(era_prior),
					era_t = as.integer(era_t),
					pair_index = as.integer(pair_index))]

	#	The Two Overlaps
		for (spec in list(list(sets = authors, name = "author_overlap"),
						  list(sets = venues,  name = "venue_overlap"))) {
			side_t <- as.data.table(spec$sets)[era == as.integer(era_t)]
			side_p <- as.data.table(spec$sets)[era == as.integer(era_prior)]
			found <- pa_pair_overlap(side_t, side_p, communities_i = rows,
									 communities_j = cols)
			grid[, (spec$name) := 0]
			if (nrow(found) > 0L) {
				grid[found, on = .(community_i, community_j),
					 (spec$name) := i.jaccard]
			}
			grid[is.na(get(spec$name)), (spec$name) := 0]
		}

	#	The Disciplinary Distance
		areas_t <- as.data.table(areas)[era == as.integer(era_t)]
		areas_p <- as.data.table(areas)[era == as.integer(era_prior)]
		found <- pa_pair_distance(areas_t, areas_p, communities_i = rows,
								  communities_j = cols)
		grid[, disc_distance := MI_DISTANCE_FILL]
		if (nrow(found) > 0L) {
			grid[found, on = .(community_i, community_j),
				 disc_distance := i.hellinger]
		}
		grid[is.na(disc_distance), disc_distance := MI_DISTANCE_FILL]

	#	The Community-Level Covariates, Once per Side
		prof <- as.data.table(profiles)
		carry <- intersect(c("n_source", "n_authors", "n_venues",
							 "n_scored", "n_core_title", "n_cog_venue",
							 "cog_venue", "cog_subject"), names(prof))
		for (side in list(list(key = "community_i", era = era_t, suffix = "_i"),
						  list(key = "community_j", era = era_prior,
							   suffix = "_j"))) {
			block <- prof[era == as.integer(side$era),
						  c("community", carry), with = FALSE]
			setnames(block, carry, paste0(carry, side$suffix))
			grid[block, on = setNames("community", side$key),
				 (paste0(carry, side$suffix)) := mget(paste0("i.", carry,
															side$suffix))]
		}

	#	The Exposure, as Three Components and a Convenience
	#	S13.2 requires the components to be stored rather than the product: so
	#	that kappa_p can be varied or recomputed without rebuilding the frame,
	#	and so that the exposure test has something to check against. kappa_p is
	#	constant within a pair and an earlier version of this file dropped it for
	#	that reason, which was both against the spec and arithmetically pointless
	#	-- a constant column over 1.87 million rows is fifteen megabytes.
	#	log_exposure is kept beside them as the offset the fit actually uses.
		setnames(grid, "kappa", "kappa_p")
		kappa_used <- grid$kappa_p[1L]
		grid[, log_exposure := log(R_i) + log(R_j) + log(kappa_p)]

	#	Assembling Result
		setcolorder(grid, c("spec_tag", "pair_index", "era_prior", "era_t",
							"community_i", "community_j", "n_coupling",
							"R_i", "R_j", "kappa_p", "log_exposure"))
		attr(grid, "kappa_used") <- kappa_used
		return(grid)
}

#############################
#   CHECKING THE ASSEMBLY   #
#############################

#' @title pa_model_i_tests
#' @description Verify one pair's design matrix against quantities it did not use.
#'
#' @param design Output of pa_model_i_pair.
#' @param spec_tag Specification tag.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#' @param coupling_dir Coupling output root. Default MI_COUPLING_DIR.
#'
#' @details
#' The coupling lane wrote a summary row per pair before any of this was
#' assembled, so it is an independent record of what the grid should contain:
#' how many communities on each side, how many nonzero cells, and the total
#' coupling. A design matrix that disagrees with it has lost or gained rows in
#' a join. The remaining checks are range checks on the covariates and a
#' completeness check, because an NA that reaches a model fit becomes a dropped
#' row and a silently different estimation sample.
#'
#' @return A data.frame with check, result and detail.
#'
#' @examples
#' tests <- pa_model_i_tests(design, "alpha1.00_g120_e14", 16, 17)
#'
#' @export
pa_model_i_tests <- function(design, spec_tag, era_prior, era_t,
							 coupling_dir = MI_COUPLING_DIR) {
	#	"""
	#	Args:
	#		design: output of pa_model_i_pair
	#		spec_tag: specification tag
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#		coupling_dir: coupling output root
	#	Returns:
	#		data.frame(check, result, detail)
	#	"""

	#	Collect
		rows <- list()
		record <- function(check, result, detail) {
			rows[[length(rows) + 1L]] <<- data.frame(
				pair = sprintf("%02d_%02d", era_prior, era_t),
				check = check, result = result, detail = detail,
				stringsAsFactors = FALSE)
		}
		dt <- as.data.table(design)

	#	The Coupling Lane's Own Summary
		sf <- file.path(coupling_dir, spec_tag,
						sprintf("pair_%02d_%02d_summary.csv", era_prior, era_t))
		sm <- if (file.exists(sf)) {
				  readr::read_csv(sf, show_col_types = FALSE, progress = FALSE)
			  } else {
				  NULL
			  }

	#	Every Ordered Pair Appears Exactly Once
		expected <- uniqueN(dt$community_i) * uniqueN(dt$community_j)
		record("the grid is complete and has no duplicates",
			   if (nrow(dt) == expected) "PASS" else "FAIL",
			   sprintf("%d row(s) against %d x %d", nrow(dt),
					   uniqueN(dt$community_i), uniqueN(dt$community_j)))

	#	The Outcome Reconciles With the Coupling Lane
		if (!is.null(sm) && "total_coupling" %in% names(sm)) {
			here <- sum(as.numeric(dt$n_coupling))
			there <- as.numeric(sm$total_coupling[1L])
			record("total coupling matches the coupling lane",
				   if (isTRUE(all.equal(here, there))) "PASS" else "FAIL",
				   sprintf("%.0f here against %.0f there", here, there))

			nonzero_here <- sum(dt$n_coupling > 0L)
			nonzero_there <- as.integer(sm$n_nonzero[1L])
			record("nonzero cell count matches the coupling lane",
				   if (nonzero_here == nonzero_there) "PASS" else "FAIL",
				   sprintf("%d here against %d there", nonzero_here,
						   nonzero_there))
		} else {
			record("total coupling matches the coupling lane", "SKIP",
				   "no coupling summary found")
			record("nonzero cell count matches the coupling lane", "SKIP",
				   "no coupling summary found")
		}

	#	The Covariates Lie Where They Should
		bounded <- intersect(c("author_overlap", "venue_overlap",
							   "disc_distance", "cog_venue_i", "cog_venue_j",
							   "cog_subject_i", "cog_subject_j"), names(dt))
		offenders <- character(0)
		for (col in bounded) {
			value <- dt[[col]]
			value <- value[!is.na(value)]
			if (length(value) > 0L && (min(value) < 0 || max(value) > 1)) {
				offenders <- c(offenders, col)
			}
		}
		record("every bounded covariate lies in [0, 1]",
			   if (length(offenders) == 0L) "PASS" else "FAIL",
			   if (length(offenders) == 0L)
				   sprintf("%d column(s) checked", length(bounded))
			   else paste(offenders, collapse = ", "))

	#	The Offset Is Finite
		finite <- is.finite(dt$log_exposure)
		record("the offset is finite everywhere",
			   if (all(finite)) "PASS" else "FAIL",
			   sprintf("%d non-finite of %d", sum(!finite), nrow(dt)))

	#	Nothing the Model Would Silently Drop
	#	Shares are allowed to be NA -- the covariate lane suppresses them below
	#	its article floor -- and the model has to decide what to do about that.
	#	The structural columns are not allowed to be.
		structural <- intersect(c("n_coupling", "R_i", "R_j", "log_exposure",
								  "author_overlap", "venue_overlap",
								  "disc_distance", "pair_index"), names(dt))
		missing_counts <- vapply(structural, function(c) sum(is.na(dt[[c]])),
								 integer(1))
		record("no structural column is missing a value",
			   if (sum(missing_counts) == 0L) "PASS" else "FAIL",
			   if (sum(missing_counts) == 0L)
				   sprintf("%d column(s) checked", length(structural))
			   else paste(sprintf("%s: %d", names(missing_counts)[
							  missing_counts > 0L],
						  missing_counts[missing_counts > 0L]),
						  collapse = ", "))

	#	How Much of the Grid Carries a Share
		share_cols <- intersect(c("cog_subject_i", "cog_subject_j"), names(dt))
		if (length(share_cols) > 0L) {
			complete <- sum(stats::complete.cases(dt[, ..share_cols]))
			record("the cognitive shares reach most of the grid", "SKIP",
				   sprintf("%.1f%% of rows have both sides",
						   100 * complete / nrow(dt)))
		}

	#	Assembling Result
		return(do.call(rbind, rows))
}

##############################
#   EXECUTING THE ASSEMBLY   #
##############################

#' @title pa_run_model_i
#' @description Assemble, check and write the Model I design matrix for every pair.
#'
#' @param specs A data.frame of specifications, or NULL for TL_SPECS.
#' @param eras Integer eras, or NULL for PA_ANALYTIC_ERAS.
#' @param coupling_dir Coupling output root. Default MI_COUPLING_DIR.
#' @param covariate_dir Covariate output root. Default MI_COVARIATE_DIR.
#' @param output_dir Destination root. Default MI_OUT_DIR.
#' @param verbose Report progress. Default TRUE.
#'
#' @details
#' One file per pair, written as it is built and then dropped from memory. What
#' the lane keeps is a stratified sample for the four-plots and a summary row,
#' both small.
#'
#' @return Invisibly, a list with summary, tests, sample and paths.
#'
#' @examples
#' res <- pa_run_model_i()
#' res$summary
#'
#' @export
pa_run_model_i <- function(specs = NULL, eras = NULL,
						   coupling_dir = MI_COUPLING_DIR,
						   covariate_dir = MI_COVARIATE_DIR,
						   output_dir = MI_OUT_DIR, verbose = TRUE) {
	#	"""
	#	Args:
	#		specs: specification table, or NULL for TL_SPECS
	#		eras: integer eras, or NULL for PA_ANALYTIC_ERAS
	#		coupling_dir: coupling output root
	#		covariate_dir: covariate output root
	#		output_dir: destination root
	#		verbose: report progress
	#	Returns:
	#		invisible list(summary, tests, sample, paths)
	#	"""

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- TL_SPECS
		if (is.null(eras))  eras  <- PA_ANALYTIC_ERAS
		set.seed(MI_SEED)

	#	Collect
		rows <- list()
		checks <- list()
		samples <- list()
		assembled <- list()
		paths <- character(0)

	#	Iterating Over Specifications
		for (s in seq_len(nrow(specs))) {
			tag <- pa_spec_tag(specs$alpha[s], specs$anchor[s],
							   specs$anchor_era[s])
			if (verbose) cat("\nSpecification ", tag, "\n", sep = "")

			#	The Covariate Lane's Outputs
				cov_dir <- file.path(covariate_dir, tag)
				pf <- file.path(cov_dir, "community_profiles.csv")
				if (!file.exists(pf)) {
					stop("pa_run_model_i: community profiles not found at ", pf,
						 ". Run pa_run_covariates() first.")
				}
				profiles <- as.data.table(readr::read_csv(
					pf, show_col_types = FALSE, progress = FALSE))

			#	The Profiles Must Be Current
			#	pa_model_i_pair selects covariates with intersect(), so a stale
			#	profile table does not fail -- it quietly yields a design matrix
			#	with no cognitive columns at all. Named here instead. n_source
			#	is the tell: the covariate lane called it n_articles until the
			#	column was renamed to say what it counts.
				expected <- c("n_source", "n_scored", "n_core_title",
							  "n_cog_venue", "cog_venue", "cog_subject")
				stale <- setdiff(expected, names(profiles))
				if (length(stale) > 0L) {
					stop("pa_run_model_i: ", pf, " is missing: ",
						 paste(stale, collapse = ", "),
						 ". This is an older covariate run; rerun ",
						 "pa_run_covariates() before assembling.")
				}
				sets <- list()
				for (field in c("authors", "venues", "areas")) {
					sf <- file.path(cov_dir, paste0("community_", field, ".rds"))
					if (!file.exists(sf)) {
						stop("pa_run_model_i: set table not found at ", sf)
					}
					sets[[field]] <- as.data.table(readRDS(sf))
				}

			#	Destination
				spec_dir <- file.path(output_dir, tag)
				dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

			#	Iterating Over Pairs
				pairs <- pa_era_pairs(eras)
				for (p in seq_len(nrow(pairs))) {
					ep <- pairs$era_prior[p]
					et <- pairs$era_t[p]
					idx <- pairs$pair_index[p]
					if (verbose) {
						cat("  pair ", idx, ": ", ep, " -> ", et, "\n", sep = "")
					}

					design <- pa_model_i_pair(tag, ep, et, idx, profiles,
											  sets$authors, sets$venues,
											  sets$areas, coupling_dir)

					#	Write Before Anything Else Touches It
						df <- file.path(spec_dir,
										sprintf("pair_%02d_%02d_frame.rds",
												ep, et))
						saveRDS(design, df, compress = FALSE)
						paths <- c(paths, df)

					#	Check
						checks[[length(checks) + 1L]] <- cbind(
							spec_tag = tag,
							pa_model_i_tests(design, tag, ep, et, coupling_dir))

					#	Keep the Table and a Sample of It
					#	The whole design matrix is 1.87 million rows, so holding
					#	the pairs and binding them at the end costs a few hundred
					#	megabytes and saves re-reading six files. The sample is
					#	kept separately because the four-plots want equal weight
					#	per pair, not weight in proportion to pair size.
						assembled[[length(assembled) + 1L]] <- design
						take <- min(MI_PLOT_SAMPLE, nrow(design))
						samples[[length(samples) + 1L]] <-
							design[sample.int(nrow(design), take)]

					#	Summarize
						rows[[length(rows) + 1L]] <- data.frame(
							spec_tag       = tag,
							pair_index     = as.integer(idx),
							era_prior      = as.integer(ep),
							era_t          = as.integer(et),
							n_rows         = nrow(design),
							n_communities_i = uniqueN(design$community_i),
							n_communities_j = uniqueN(design$community_j),
							n_nonzero      = sum(design$n_coupling > 0L),
							pct_nonzero    = round(100 * mean(
												design$n_coupling > 0L), 3),
							total_coupling = sum(as.numeric(design$n_coupling)),
							max_coupling   = max(design$n_coupling),
							kappa_used     = attr(design, "kappa_used"),
							mean_author_overlap = round(mean(
												design$author_overlap), 6),
							pct_any_author = round(100 * mean(
												design$author_overlap > 0), 3),
							mean_venue_overlap = round(mean(
												design$venue_overlap), 6),
							mean_disc_distance = round(mean(
												design$disc_distance), 4),
							file_mb        = round(file.size(df) / 1024^2, 1),
							stringsAsFactors = FALSE)

					rm(design)
				}
		}

	#	Bind
		summary <- do.call(rbind, rows)
		tests <- do.call(rbind, checks)
		sample_rows <- rbindlist(samples, fill = TRUE)
		setorder(sample_rows, pair_index)
		design <- rbindlist(assembled, fill = TRUE)
		rm(assembled)
		invisible(gc(verbose = FALSE))
		setorder(design, spec_tag, pair_index, community_i, community_j)

	#	Write
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
		mf <- file.path(output_dir, "model_i_frame.rds")
		saveRDS(design, mf, compress = FALSE)
		sf <- file.path(output_dir, "design_summary.csv")
		readr::write_csv(summary, sf)
		tf <- file.path(output_dir, "design_tests.csv")
		readr::write_csv(tests, tf)
		saveRDS(sample_rows, file.path(output_dir, "design_sample.rds"),
				compress = FALSE)
		pf_pending <- file.path(output_dir, "frame_pending.csv")
		readr::write_csv(MI_PENDING, pf_pending)
		paths <- c(paths, mf, sf, tf, pf_pending)

	#	One Check on the Bound Matrix
	#	The per-pair checks cannot see a bind that lost or duplicated a pair.
		bound <- data.frame(
			pair = "all",
			check = "the bound matrix is the pairs, summed",
			result = if (nrow(design) == sum(summary$n_rows)) "PASS" else "FAIL",
			detail = sprintf("%s row(s) against %s expected",
							 format(nrow(design), big.mark = ","),
							 format(sum(summary$n_rows), big.mark = ",")),
			stringsAsFactors = FALSE)
		tests <- rbind(tests, cbind(spec_tag = summary$spec_tag[1L], bound))
		readr::write_csv(tests, tf)

	#	Draw
	#	Ordered by pair_index, which is the ordering the time trend is fitted
	#	against: a covariate that steps between pairs is partly collinear with it
	#	and the run-sequence panel is where that shows.
		if (isTRUE(MI_WRITE_PLOTS) && exists("pa_covariate_plots",
											 mode = "function")) {
			columns <- intersect(c("n_coupling", "log_exposure", "R_i", "R_j",
								   "author_overlap", "venue_overlap",
								   "disc_distance", "cog_venue_i",
								   "cog_venue_j", "cog_subject_i", "cog_subject_j"),
								 names(sample_rows))
			drawn <- pa_covariate_plots(sample_rows,
										file.path(output_dir, "plots"),
										columns = columns)
			paths <- c(paths, drawn)
			if (verbose) {
				cat("\n", length(drawn), " four-plot(s) written from a sample of ",
					nrow(sample_rows), " row(s)\n", sep = "")
			}
		}

	#	Report
		if (isTRUE(verbose)) {
			cat("\nThe Model I design matrix\n")
			print(summary[c("pair_index", "era_prior", "era_t", "n_rows",
							"pct_nonzero", "total_coupling", "pct_any_author",
							"mean_disc_distance", "file_mb")],
				  row.names = FALSE)
			cat("\n")
			print(tests[c("pair", "check", "result", "detail")],
				  row.names = FALSE)
			n_fail <- sum(tests$result == "FAIL")
			cat("\n", sum(tests$result == "PASS"), " of ",
				sum(tests$result != "SKIP"), " check(s) passed",
				if (n_fail > 0L) sprintf(", %d FAILED", n_fail) else "",
				"\n", sep = "")
			cat("Frame: ",
				format(nrow(design), big.mark = ",", scientific = FALSE),
				" row(s), ", ncol(design), " columns, ",
				round(file.size(mf) / 1024^2), " MB at ", mf, "\n", sep = "")
			cat("\nNOT A FITTABLE DESIGN MATRIX. S13.2 columns still missing:\n")
			print(MI_PENDING[c("column", "role", "needs")], row.names = FALSE)
			cat("\nThey attach by (era, community) and by pair, so nothing here\n",
				"has to be rebuilt -- but nothing here should be fit either.\n",
				sep = "")
		}

	#	Assembling Result
		return(invisible(list(design = design, summary = summary,
							  tests = tests, sample = sample_rows,
							  paths = paths)))
}

########################
#   RUNNING THE LANE   #
########################

#	Run Under Rscript, Not Under source()
	if (sys.nframe() <= 1L) {
		cat("PathA_ModelI.R -- the Model I design matrix\n")
		cat("===========================================\n\n")
		started <- Sys.time()

		res <- pa_run_model_i()

		cat("\n===========================================\n")
		cat("Built in ", round(as.numeric(difftime(Sys.time(), started,
												   units = "secs")), 1),
			"s\n", sep = "")
		cat("Output: ", MI_OUT_DIR, "\n", sep = "")
		if (any(res$tests$result == "FAIL")) {
			cat("FAILED checks present\n")
			quit(status = 1L)
		}
		cat("OK\n")
	}
