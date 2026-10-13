#	PathA_BackwardTies.R -- Are Backward Ties Canon, or Ongoing Research Cut by a Boundary?
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	13 October 2026
#
#	THE QUESTION. Three quarters of the citations an era emits point outside it, at a
#	mean distance of 1.5 to 1.9 eras. Two readings of that are possible and they have
#	opposite consequences:
#
#		CANON.	Backward ties are citations to a small number of foundational works.
#				They reach far back, they concentrate on few targets, and dropping
#				them is what makes the era networks about ongoing exchange rather
#				than shared inheritance. Nothing is lost.
#
#		BOUNDARY.	Backward ties are ordinary current citation that happens to cross
#				a tile edge -- a 2013 article citing a 2012 paper. They sit at
#				distance one, they spread thin across many targets, and dropping
#				them discards live contact for an arbitrary reason.
#
#	WHAT SEPARATES THEM. Three measurements, in rising order of sharpness:
#
#		1.	Distance. Canon reaches far; a boundary artifact sits at distance one.
#		2.	Concentration. Canon lands repeatedly on the same works, so a few targets
#			absorb most of the ties and the Gini over targets is high. Ordinary
#			citation spreads.
#		3.	Singleton share. A work cited exactly once in the whole corpus cannot be
#			canonical. The share of backward ties landing on such works is the
#			cleanest single number here, because it needs no threshold.
#
#	The answer is almost certainly a mixture, and the useful output is the mixture's
#	composition by distance: how much of what the tiling discards is canon and how
#	much is live work cut by an edge.
#
#	INPUT, from PathA_Revised_Eras.R
#		data/eras_7yr/pa_node_list.Rda      pa_nodes: node_id, node_key, era, degrees
#		data/eras_7yr/pa_edge_list.Rda      pa_edges: sender_era, target_era
#
#	OUTPUT
#		backward_tie_profile.csv    one row per era distance
#		backward_tie_by_era.csv     one row per citing era x distance class
#		backward_top_targets.csv    the 50 most-cited backward targets, for eyeballing
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else,
#	data.table for the aggregation.

################
#   PACKAGES   #
################

#	Load Packages
	library(data.table)

#####################
#   CONFIGURATION   #
#####################

#	Working Directory
	setwd("/workspace/caffeine_citation")

#	Input and Output
	DATA_DIR <- "data/eras_7yr"

#	Analytic Range
#	Eras 11 to 17 are the range where mean degree is at its plateau and the
#	both-degree share of citing articles exceeds 30%. Era 18 is a two-year era and
#	eras 1 to 10 are too small to carry structure.
	ANALYTIC_ERAS <- 11:17

#	Era Width, for reporting distances in years
	ERA_WIDTH <- 7L

#	How Many Targets to List
	TOP_N <- 50L

###############
#   HELPERS   #
###############

#	Helper Function for pa_backward_profile: Concentration of a Count Vector
	pa_concentration <- function(counts) {
		#	"""
		#	Args:
		#		counts: integer vector, ties per target
		#	Returns:
		#		list(n_targets, gini, top1_share, top5_share, singleton_share)
		#	Notes:
		#		The Gini is computed over targets, so 0 means every target receives the
		#		same number of ties and 1 means one target receives them all. Canonical
		#		citation should push it high; ordinary citation should not.
		#
		#		singleton_share is the share of TIES whose target receives exactly one
		#		tie in this set -- not the share of targets. A work cited once cannot
		#		be canonical, so a high value here is decisive against the canon
		#		reading whatever the Gini says.
		#	"""

		#	Early return
			if (length(counts) == 0L) {
				return(list(n_targets = 0L, gini = NA_real_, top1_share = NA_real_,
							top5_share = NA_real_, singleton_share = NA_real_))
			}

		#	Sort ascending for the Gini
			x <- sort(as.numeric(counts))
			n <- length(x)
			total <- sum(x)

		#	Gini over targets
			gini <- if (total == 0) NA_real_ else
				(2 * sum(seq_len(n) * x) / (n * total)) - (n + 1) / n

		#	Shares absorbed by the heaviest targets
			ord <- sort(as.numeric(counts), decreasing = TRUE)
			k1 <- max(1L, ceiling(0.01 * n))
			k5 <- max(1L, ceiling(0.05 * n))

		#	Assembling Result
			return(list(n_targets       = n,
						gini            = round(gini, 4),
						top1_share      = round(sum(ord[seq_len(k1)]) / total, 4),
						top5_share      = round(sum(ord[seq_len(k5)]) / total, 4),
						singleton_share = round(sum(x[x == 1]) / total, 4)))
	}

#	Helper Function for the profile functions: Backward Arcs with Their Distance
	pa_backward_arcs <- function(edges, eras = ANALYTIC_ERAS) {
		#	"""
		#	Args:
		#		edges: pa_edges, carrying sender_era and target_era
		#		eras: citing eras to include (default ANALYTIC_ERAS)
		#	Returns:
		#		data.table(sender_id, target_id, sender_era, target_era, distance)
		#	Notes:
		#		Backward means the cited work was published in an earlier era than the
		#		citing article. Arcs whose target has no era are excluded and counted
		#		by the caller, since a work with no publication year has no distance.
		#	"""

		#	Restrict and compute the distance
			e <- as.data.table(edges)
			back <- e[!is.na(sender_era) & !is.na(target_era) &
					  sender_era %in% eras & target_era < sender_era]
			back[, distance := sender_era - target_era]

		#	Assembling Result
			return(back)
	}

#################
#   PROFILING   #
#################

#' @title pa_backward_profile
#' @description Profile backward citation ties by era distance, testing whether they concentrate on a few canonical works or spread across many.
#'
#' @param edges pa_edges from PathA_Revised_Eras.R, carrying sender_era and target_era.
#' @param nodes pa_nodes from the same run, carrying node_id, node_key, era and in_degree.
#' @param eras Citing eras to include. Default ANALYTIC_ERAS.
#' @param era_width Era width in years, used to report distances in years. Default ERA_WIDTH.
#'
#' @details Three readings separate canonical citation from ties cut by a tile boundary: how far back the tie reaches, how concentrated the targets are, and what share of ties land on works cited only once in the whole corpus. The third needs no threshold and is the one to trust when the first two disagree.
#'
#' corpus_in_degree is each target's in-degree over the entire edge list, not within the backward set, so a work's standing as canon is judged against the whole literature rather than against the ties being examined.
#'
#' @return A data.table with one row per era distance.
#'
#' @examples
#' \dontrun{
#' prof <- pa_backward_profile(pa_edges, pa_nodes)
#' prof[, .(distance, n_arcs, pct_of_backward, gini, singleton_share)]
#' }
#'
#' @export
pa_backward_profile <- function(edges, nodes, eras = ANALYTIC_ERAS,
								era_width = ERA_WIDTH) {
	#	"""
	#	Args:
	#		edges: pa_edges carrying sender_era and target_era
	#		nodes: pa_nodes carrying node_id, era and in_degree
	#		eras: citing eras to include (default ANALYTIC_ERAS)
	#		era_width: era width in years (default ERA_WIDTH)
	#	Returns:
	#		data.table, one row per distance
	#	Notes:
	#		Distance 1 is the boundary case: a tie a sliding window of the same width
	#		would mostly have kept.
	#	"""

	#	Backward arcs
		back <- pa_backward_arcs(edges, eras)
		if (nrow(back) == 0L) stop("no backward arcs in the requested eras")

	#	Attach each target's standing in the whole corpus
		deg <- as.data.table(nodes)[, .(target_id = node_id,
										corpus_in_degree = in_degree)]
		back <- as.data.table(dplyr::left_join(back, deg, by = "target_id",
											   relationship = "many-to-one"))

	#	Iterating Over Distances
		rows <- vector("list", 0L)
		for (d in sort(unique(back$distance))) {
			#	Ties at this distance
				at_d <- back[distance == d]
				per_target <- at_d[, .N, by = target_id]
				conc <- pa_concentration(per_target$N)

			#	Collect
				rows[[length(rows) + 1L]] <- data.table(
					distance            = d,
					distance_years      = d * era_width,
					n_arcs              = nrow(at_d),
					n_targets           = conc$n_targets,
					arcs_per_target     = round(nrow(at_d) / conc$n_targets, 3),
					gini                = conc$gini,
					top1_share          = conc$top1_share,
					top5_share          = conc$top5_share,
					singleton_share     = conc$singleton_share,
					median_corpus_in_degree = as.numeric(median(at_d$corpus_in_degree,
																na.rm = TRUE)),
					mean_corpus_in_degree   = round(mean(at_d$corpus_in_degree,
														 na.rm = TRUE), 2),
					pct_target_cited_once   = round(100 * mean(at_d$corpus_in_degree == 1L,
															   na.rm = TRUE), 2))
		}

	#	Shares of the whole backward set
		prof <- rbindlist(rows)
		prof[, pct_of_backward := round(100 * n_arcs / sum(n_arcs), 2)]
		setcolorder(prof, c("distance", "distance_years", "n_arcs", "pct_of_backward"))

	#	Assembling Result
		return(prof)
}

#' @title pa_backward_by_era
#' @description Split each citing era's backward ties into distance one and the rest, so the boundary effect can be read era by era.
#'
#' @param edges pa_edges carrying sender_era and target_era.
#' @param nodes pa_nodes carrying node_id and in_degree.
#' @param eras Citing eras to include. Default ANALYTIC_ERAS.
#'
#' @details Distance one is the tie a sliding window would mostly have retained; distance three or more is where canonical citation should live. Reporting them separately per era shows whether the mixture is stable over time or whether the recent eras behave differently.
#'
#' @return A data.table with one row per citing era and distance class.
#'
#' @examples
#' \dontrun{
#' pa_backward_by_era(pa_edges, pa_nodes)
#' }
#'
#' @export
pa_backward_by_era <- function(edges, nodes, eras = ANALYTIC_ERAS) {
	#	"""
	#	Args:
	#		edges: pa_edges carrying sender_era and target_era
	#		nodes: pa_nodes carrying node_id and in_degree
	#		eras: citing eras to include (default ANALYTIC_ERAS)
	#	Returns:
	#		data.table, one row per citing era and distance class
	#	Notes:
	#		Classes are 1, 2 and 3+, which is as fine as the counts support once the
	#		early eras are excluded.
	#	"""

	#	Backward arcs with the target's corpus standing
		back <- pa_backward_arcs(edges, eras)
		deg <- as.data.table(nodes)[, .(target_id = node_id, corpus_in_degree = in_degree)]
		back <- as.data.table(dplyr::left_join(back, deg, by = "target_id",
											   relationship = "many-to-one"))

	#	Distance classes
		back[, distance_class := ifelse(distance == 1L, "1",
								 ifelse(distance == 2L, "2", "3+"))]

	#	Aggregate
		out <- back[, .(n_arcs = .N,
						n_targets = uniqueN(target_id),
						pct_target_cited_once = round(100 * mean(corpus_in_degree == 1L,
																 na.rm = TRUE), 2),
						median_corpus_in_degree = as.numeric(median(corpus_in_degree,
																	na.rm = TRUE))),
					by = .(era = sender_era, distance_class)]
		out[, pct_of_era_backward := round(100 * n_arcs / sum(n_arcs), 2), by = era]
		setorder(out, era, distance_class)

	#	Assembling Result
		return(out)
}

#	The Heaviest Backward Targets
	pa_backward_top_targets <- function(edges, nodes, eras = ANALYTIC_ERAS,
										top_n = TOP_N) {
		#	"""
		#	Args:
		#		edges: pa_edges carrying sender_era and target_era
		#		nodes: pa_nodes carrying node_id, node_key, year_published and in_degree
		#		eras: citing eras to include (default ANALYTIC_ERAS)
		#		top_n: how many targets to return (default TOP_N)
		#	Returns:
		#		data.table of the most-cited backward targets
		#	Notes:
		#		The qualitative half of the test. If the canon reading is right these
		#		rows should be recognisable -- method papers, early pharmacology, the
		#		works everyone cites -- and a glance at the keys settles it faster than
		#		any statistic.
		#	"""

		#	Count backward ties per target
			back <- pa_backward_arcs(edges, eras)
			per_target <- back[, .(n_backward_ties = .N,
								   mean_distance = round(mean(distance), 2),
								   n_citing_eras = uniqueN(sender_era)), by = target_id]

		#	Attach identity
			ndt <- as.data.table(nodes)[, .(target_id = node_id, node_key, key_type,
											year_published, era, in_degree, is_article)]
			per_target <- as.data.table(dplyr::left_join(per_target, ndt, by = "target_id",
														 relationship = "many-to-one"))

		#	Rank
			setorder(per_target, -n_backward_ties)

		#	Assembling Result
			return(head(per_target, top_n))
	}

############
#   MAIN   #
############

#	Load the Construction Output
	cat("Loading node and edge lists...\n")
	env <- new.env()
	load(file.path(DATA_DIR, "pa_node_list.Rda"), envir = env)
	load(file.path(DATA_DIR, "pa_edge_list.Rda"), envir = env)
	pa_nodes <- get("pa_nodes", envir = env)
	pa_edges <- get("pa_edges", envir = env)
	cat("  ", nrow(pa_nodes), " nodes, ", nrow(pa_edges), " arcs\n", sep = "")

#	Profile by Distance
	cat("\nBackward ties by era distance, citing eras ",
		min(ANALYTIC_ERAS), " to ", max(ANALYTIC_ERAS), ":\n", sep = "")
	profile <- pa_backward_profile(pa_edges, pa_nodes)
	print(profile[, .(distance, distance_years, n_arcs, pct_of_backward, arcs_per_target,
					  gini, top5_share, singleton_share, pct_target_cited_once)])

#	Profile by Era
	cat("\nBackward ties by citing era and distance class:\n")
	by_era <- pa_backward_by_era(pa_edges, pa_nodes)
	print(by_era)

#	The Heaviest Targets
	cat("\nThe ", TOP_N, " most-cited backward targets:\n", sep = "")
	top_targets <- pa_backward_top_targets(pa_edges, pa_nodes)
	print(head(top_targets[, .(node_key, year_published, n_backward_ties,
							   mean_distance, n_citing_eras, in_degree)], 20L))

#	Write
	readr::write_csv(profile, file.path(DATA_DIR, "backward_tie_profile.csv"))
	readr::write_csv(by_era, file.path(DATA_DIR, "backward_tie_by_era.csv"))
	readr::write_csv(top_targets, file.path(DATA_DIR, "backward_top_targets.csv"))
	cat("\nWritten to ", DATA_DIR, "\n", sep = "")

#	Read the Result
	cat("\nHow to read this:\n")
	cat("  CANON    far distances carry most of the ties, gini high, singleton_share low\n")
	cat("  BOUNDARY distance 1 carries most of the ties, gini low, singleton_share high\n")
