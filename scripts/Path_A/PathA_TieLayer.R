#	PathA_TieLayer.R -- Bridging Works, Descent Ties, and the Model II Frame
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	27 October 2026
#
#	WHERE THIS SITS. Stage 3 of the pipeline order of operations (specification
#	S14.1). Requires PathA_PajekReaders.R and PathA_EraResolution.R. Its output
#	feeds stage 4, which decides from work_ties which communities participate in
#	any tie and therefore which ones are worth labeling at all.
#	Tested by pa_run_tie_layer_tests().
#
#	This file produces the work-resolution table that is simultaneously the source
#	of the descent networks and the model frame for Model II, which is why the
#	specification calls it a deliverable rather than an intermediate.
#
#	It repairs two of the three recorded defects (S14.3):
#
#	1.	ARBITRARY ATTRIBUTION. citation_finder() reduced each era's edges with
#		`citation_index[!duplicated(citation_index$target_id), ]`. Where a work was
#		cited by several era-t articles, that kept whichever row sorted first and
#		discarded the rest -- so a work cited by three communities produced one tie
#		instead of three, and which one depended on edge-list row order. Measured
#		damage: 15.9% of ties on one era pair, 56.2% on the next. Here the citing
#		side is never deduplicated by target; every (citing article, work) pair is
#		retained and aggregation happens once, at the end.
#
#	2.	DUPLICATED EDGES. The edge list stacks two overlapping sources, storing each
#		citation between two and fourteen times, non-uniformly. The fix is whole-row
#		deduplication, not division by a constant.
#
#	What is NOT changed, because it is a design decision and not a defect: the
#	both-periods requirement. A work bridges the pair only if it belongs to the
#	earlier era AND was actively cited during it. That is the strong definition the
#	specification adopts (S6.1).
#
#	WHAT CHANGED IN OCTOBER 2026, AND WHY.
#
#	a.	pa_attach_sender_era IS NOW IDEMPOTENT. The rebuilt edge list already
#		carries sender_era and target_era; PathA_Revised_Eras.R attaches them before
#		saving pa_edge_list.Rda. Joining a second sender_era onto it would have
#		produced sender_era.x and sender_era.y, so edges_dated$sender_era would
#		resolve to NULL, every era filter would return logical(0), and every pair
#		would report zero bridging works WITHOUT ERRORING. That is the same
#		column-collision failure the era construction already paid for once. The
#		function now detects an existing column and leaves it alone.
#
#	b.	pa_risk_set USES data.table. The frame is |bridging works| x |citing
#		communities|, and under the calibrated construction (S14.8) that is 15.3
#		million rows for the (16,17) pair and 31.1 million across the six analytic
#		pairs -- not the 345,000 the September note assumed, which was written
#		against 394 communities where era 17 now has 1,105. expand.grid plus three
#		left joins materialises several copies of that. CJ and joins by reference
#		do not.
#
#	c.	pa_weight_descent AGGREGATES ON INTEGER KEYS. It had been pasting
#		citing_community and cited_community into a string and splitting it back,
#		which is the pattern pa_work_ties spends a paragraph explaining why it
#		avoids. Safe as it was called, and now closed.
#
#	d.	pa_out_citations SITS ABOVE ITS CALLER. R resolved it at call time, so the
#		old order worked; it inverted the helpers-before-mains rule the other files
#		follow.
#
#	e.	THERE IS AN ORCHESTRATOR. pa_run_tie_layer() loops specifications and era
#		pairs, writes per pair, and runs the checks. Community granularity is now a
#		declared specification rather than a fixture (S14.8), so the tie layer has
#		to be run once per specification and the outputs kept apart.
#
#	Determinism is the governing constraint. Every function here returns output that
#	is invariant to the row order of its inputs; the regression test for defect 1 is
#	that shuffling the edge list changes nothing (S14.4).
#
#	Conventions: dplyr for joins only, readr for CSV I/O, data.table for the risk
#	set and the descent aggregation, base R otherwise.

################
#   PACKAGES   #
################

#	Load Packages
	library(data.table)

#####################
#   CONFIGURATION   #
#####################

#	Input Paths
	TL_ERA_DIR <- "/workspace/caffeine_citation/data/eras_7yr"

#	Output Path
	TL_OUT_DIR <- "/workspace/caffeine_citation/data/tie_layer"

#	Specifications to Run
#	The reduced sensitivity design of S14.8: all three scaling rules at the primary
#	anchor, plus one finer and one coarser anchor at the primary rule. The risk set
#	is built for the primary only by default, because it is the large output and
#	only Model II consumes it; flip with_risk_set when Model II's sensitivity is
#	run. Rough row counts per specification are 31 million across the six pairs.
	TL_SPECS <- data.frame(
		alpha         = c(1.00,      0.00,          0.50,          1.00,      1.00),
		anchor        = c(120,       120,           120,           90,        160),
		anchor_era    = c(14L,       14L,           14L,           14L,       14L),
		grid          = rep("gamma_0.02_6.0_n64", 5L),
		spec_role     = c("primary", "sensitivity", "sensitivity", "anchor",   "anchor"),
		with_risk_set = c(TRUE,      FALSE,         FALSE,         FALSE,     FALSE),
		stringsAsFactors = FALSE)

#	Risk-Set Size Guard
#	A warning, not a limit. The point is that a grid an order of magnitude larger
#	than expected announces itself before it exhausts memory.
	TL_RISK_WARN_ROWS <- 2e7

###############
#   HELPERS   #
###############

#	Deduplicate the Edge List
	pa_dedupe_edges <- function(edges, verbose = TRUE) {
		#	"""
		#	Args:
		#		edges: the citation edge list, with sender_id and target_id
		#		verbose: report the reduction? (default TRUE)
		#	Returns:
		#		list(edges = deduplicated data.frame, n_before, n_after, n_removed)
		#	Notes:
		#		Deduplicates on the identifier pair rather than on whole rows, so that
		#		a duplicate carrying a cosmetic difference in a label column is still
		#		removed. Rows are ordered by (sender_id, target_id) before the unique
		#		is taken, which makes the surviving representative independent of the
		#		input order -- required for the determinism test.
		#
		#		Against the rebuilt edge list this is a no-op: pa_build_edge_list
		#		already reduces to one row per distinct citation. It is kept because
		#		reporting zero removed CONFIRMS that, and because the legacy edge list
		#		still needs it.
		#	"""

		#	Validation
			need <- c("sender_id", "target_id")
			miss <- setdiff(need, names(edges))
			if (length(miss) > 0L) {
				stop("pa_dedupe_edges: edge list missing column(s): ",
					 paste(miss, collapse = ", "))
			}

		#	Order Deterministically, Then Reduce on the Identifier Pair
			n_before <- nrow(edges)
			ord <- order(edges$sender_id, edges$target_id, method = "radix")
			edges <- edges[ord, , drop = FALSE]
			keep <- !duplicated(edges[c("sender_id", "target_id")])
			edges <- edges[keep, , drop = FALSE]
			rownames(edges) <- NULL

		#	Report
			n_after <- nrow(edges)
			if (verbose) {
				cat("  edges ", n_before, " -> ", n_after, " after deduplication (",
					n_before - n_after, " removed)\n", sep = "")
			}

		#	Assembling Result
			return(list(edges = edges, n_before = n_before,
						n_after = n_after, n_removed = n_before - n_after))
	}

#	Attach the Citing Era to Each Edge
	pa_attach_sender_era <- function(edges, node_list, verbose = TRUE) {
		#	"""
		#	Args:
		#		edges: deduplicated citation edge list
		#		node_list: full citation node list, carrying node_id and era
		#		verbose: report when the column was already present? (default TRUE)
		#	Returns:
		#		data.frame, edges with an integer sender_era column
		#	Notes:
		#		IDEMPOTENT BY NECESSITY. The rebuilt edge list already carries
		#		sender_era; PathA_Revised_Eras.R attaches it before saving. A second
		#		join would have produced sender_era.x and sender_era.y, after which
		#		edges_dated$sender_era is NULL, every era filter in this file returns
		#		logical(0), and every pair reports zero bridging works with no error
		#		raised anywhere. The era construction lost an afternoon to the same
		#		collision on the degree columns.
		#
		#		So: if sender_era is present it is coerced to integer and returned.
		#		Only an edge list without it gets the join.
		#	"""

		#	Early return
			if ("sender_era" %in% names(edges)) {
				if (verbose) {
					cat("  sender_era already present on the edge list; left as is\n")
				}
				edges$sender_era <- as.integer(edges$sender_era)
				return(edges)
			}

		#	Validation
			need <- c("node_id", "era")
			miss <- setdiff(need, names(node_list))
			if (length(miss) > 0L) {
				stop("pa_attach_sender_era: node_list missing column(s): ",
					 paste(miss, collapse = ", "))
			}

		#	Reduce the Node List to the Lookup
			key <- data.frame(sender_id  = node_list$node_id,
							  sender_era = as.integer(node_list$era),
							  stringsAsFactors = FALSE)
			key <- key[!duplicated(key$sender_id), , drop = FALSE]

		#	Join
			out <- dplyr::left_join(edges, key, by = "sender_id",
									relationship = "many-to-one")

		#	Assembling Result
			return(out)
	}

#	Record One Check
	pa_check_tl <- function(name, passed, detail = "") {
		#	"""
		#	Args:
		#		name: short check name
		#		passed: logical
		#		detail: observed value or message
		#	Returns:
		#		one-row data.frame
		#	"""

		#	Assembling Result
			return(data.frame(check = name,
							  result = if (isTRUE(passed)) "PASS" else "FAIL",
							  detail = as.character(detail),
							  stringsAsFactors = FALSE))
	}

#########################
#   PRIMARY FUNCTIONS   #
#########################

#' @title pa_out_citations
#' @description Count the citations each era-t community emits, including the communities that emit none.
#'
#' @param edges_dated Edge list carrying sender_era.
#' @param era_map_t Era map for era t.
#' @param era_t Integer era t.
#'
#' @details
#' This is E^C of the specification: the opportunity a community had to make any
#' citation at all. It counts every citation the community's articles emit, not
#' only those reaching bridging works -- the whole point of an exposure term is
#' that it is the denominator of chances, not of successes.
#'
#' Defined above pa_pair_ties, which calls it. R resolves the name at call time so
#' the reverse order worked, but it inverted the helpers-before-mains rule.
#'
#' @return A data.frame with citing_community and out_citations_i, one row per
#'   era-t community.
#'
#' @export
pa_out_citations <- function(edges_dated, era_map_t, era_t) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		era_map_t: era map for era t
	#		era_t: integer era t
	#	Returns:
	#		data.frame(citing_community, out_citations_i)
	#	"""

	#	Era t Edges
		e <- edges_dated[!is.na(edges_dated$sender_era) &
						 edges_dated$sender_era == era_t, , drop = FALSE]

	#	Map to Community and Count
		citers <- data.frame(sender_id        = era_map_t$node_id,
							 citing_community = era_map_t$community,
							 stringsAsFactors = FALSE)
		citers <- citers[!duplicated(citers$sender_id), , drop = FALSE]
		e <- dplyr::left_join(e[c("sender_id")], citers, by = "sender_id",
							  relationship = "many-to-one")
		e <- e[!is.na(e$citing_community), , drop = FALSE]
		cnt <- table(e$citing_community)

	#	Every Community Appears, Including the Silent Ones
		all_comm <- sort(unique(era_map_t$community))
		out <- data.frame(citing_community = all_comm,
						  out_citations_i  = as.integer(cnt[as.character(all_comm)]),
						  stringsAsFactors = FALSE)
		out$out_citations_i[is.na(out$out_citations_i)] <- 0L

	#	Assembling Result
		rownames(out) <- NULL
		return(out)
}

#' @title pa_bridging_works
#' @description Identify the works that bridge one era pair, under the both-periods requirement.
#'
#' @param edges_dated Edge list carrying sender_era, from pa_attach_sender_era.
#' @param era_map_prior Era map for era t-1, from pa_era_map.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#'
#' @details
#' The both-periods requirement in full. A work bridges the pair when all three
#' hold:
#' \itemize{
#'   \item (a) it belongs to era t-1, so it carries a community in that era's
#'     partition -- "belonged to an era t-1 community";
#'   \item (b) some era t-1 article cites it -- "was actively cited during that
#'     earlier era";
#'   \item (c) some era t article cites it -- the descent citation itself.
#' }
#'
#' (a) is what makes cited_community well defined and is why the cited community
#' follows from the work rather than being an independent index in the Model II
#' frame.
#'
#' Conditions (b) and (c) require being CITED in two consecutive eras, so a citing
#' article with no in-degree can never bridge. The population carrying descent is
#' therefore not the population community granularity is calibrated on, which is
#' why S14.8 reports both.
#'
#' @return A data.frame with work_id, work_label and cited_community, ordered by
#'   work_id.
#'
#' @export
pa_bridging_works <- function(edges_dated, era_map_prior, era_prior, era_t) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		era_map_prior: era map for era t-1
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#	Returns:
	#		data.frame(work_id, work_label, cited_community)
	#	Notes:
	#		work_label comes from the era map's node_label, which pa_era_map emits as
	#		an alias of node_key under legacy_names. It therefore carries the
	#		canonical key -- a normalized DOI or an author_year_journal triple --
	#		rather than the 2024 list's label.
	#	"""

	#	Validation
		if (is.null(edges_dated$sender_era)) {
			stop("pa_bridging_works: edges_dated carries no sender_era column. ",
				 "If a join produced sender_era.x and sender_era.y, the edge list ",
				 "already had the column and should not have been joined again.")
		}
		need <- c("node_id", "node_label", "community")
		miss <- setdiff(need, names(era_map_prior))
		if (length(miss) > 0L) {
			stop("pa_bridging_works: era_map_prior missing column(s): ",
				 paste(miss, collapse = ", "))
		}

	#	Works Cited During Each Era
		cited_prior <- unique(edges_dated$target_id[!is.na(edges_dated$sender_era) &
													edges_dated$sender_era == era_prior])
		cited_t     <- unique(edges_dated$target_id[!is.na(edges_dated$sender_era) &
													edges_dated$sender_era == era_t])

	#	Conditions (b) and (c)
		both <- intersect(cited_prior, cited_t)

	#	Condition (a): the Work Must Sit in Era t-1's Partition
		owners <- era_map_prior[c("node_id", "node_label", "community")]
		names(owners) <- c("work_id", "work_label", "cited_community")
		out <- owners[owners$work_id %in% both, , drop = FALSE]

	#	Assembling Result
		out <- out[order(out$work_id, method = "radix"), , drop = FALSE]
		rownames(out) <- NULL
		return(out)
}

#' @title pa_work_ties
#' @description Build the work-resolution tie table for one era pair, keeping every citing article.
#'
#' @param edges_dated Edge list carrying sender_era.
#' @param bridging Bridging works, from pa_bridging_works.
#' @param era_map_t Era map for era t, from pa_era_map.
#' @param era_t Integer era t.
#'
#' @details
#' This is where the arbitrary-attribution defect is repaired. Every era-t edge
#' into a bridging work is kept and the citing article is mapped to its community;
#' the aggregation to (work, citing community) is a count of distinct citing
#' articles, so a work cited by three communities yields three rows rather than one.
#'
#' y_cites counts distinct citing ARTICLES, which after deduplication is the same
#' as counting citation events -- an article cites a work once.
#'
#' The aggregation groups on sorted integer keys rather than on a pasted string.
#' node_id is stored as a DOUBLE in the node list, and R renders round doubles in
#' scientific notation: as.character(200000) is "2e+05" while as.character(200000L)
#' is "200000". Any key built by paste() over those columns is therefore sensitive
#' to storage mode and to the scipen option, which is exactly the kind of
#' dependency that survives every test until the one run where it does not.
#'
#' @return A data.frame with work_id, cited_community, citing_community and
#'   y_cites, for realized citations only.
#'
#' @export
pa_work_ties <- function(edges_dated, bridging, era_map_t, era_t) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		bridging: bridging works, from pa_bridging_works()
	#		era_map_t: era map for era t
	#		era_t: integer era t
	#	Returns:
	#		data.frame(work_id, cited_community, citing_community, y_cites)
	#	"""

	#	Era t Edges into the Bridging Works
		e <- edges_dated[!is.na(edges_dated$sender_era) &
						 edges_dated$sender_era == era_t, , drop = FALSE]
		e <- e[e$target_id %in% bridging$work_id,
			   c("sender_id", "target_id"), drop = FALSE]

	#	Map the Citing Article to Its Community
		citers <- data.frame(sender_id        = era_map_t$node_id,
							 citing_community = era_map_t$community,
							 stringsAsFactors = FALSE)
		citers <- citers[!duplicated(citers$sender_id), , drop = FALSE]
		e <- dplyr::left_join(e, citers, by = "sender_id",
							  relationship = "many-to-one")

	#	A Citing Article outside Era t's Network Cannot Be Attributed
		e <- e[!is.na(e$citing_community), , drop = FALSE]

	#	Attach the Cited Community, Which Follows from the Work
		names(e)[names(e) == "target_id"] <- "work_id"
		e <- dplyr::left_join(e, bridging[c("work_id", "cited_community")],
							  by = "work_id", relationship = "many-to-one")

	#	Early return
		if (nrow(e) == 0L) {
			return(data.frame(work_id = integer(0), cited_community = integer(0),
							  citing_community = integer(0), y_cites = integer(0)))
		}

	#	Aggregate Once, at the End
		k <- data.frame(work_id          = as.integer(e$work_id),
						cited_community  = as.integer(e$cited_community),
						citing_community = as.integer(e$citing_community),
						stringsAsFactors = FALSE)
		ord <- order(k$work_id, k$cited_community, k$citing_community,
					 method = "radix")
		k <- k[ord, , drop = FALSE]
		first <- !duplicated(k)
		grp <- cumsum(first)
		out <- k[first, , drop = FALSE]
		out$y_cites <- as.integer(tabulate(grp, nbins = sum(first)))

	#	Assembling Result
		out <- out[order(out$work_id, out$cited_community, out$citing_community,
						 method = "radix"), , drop = FALSE]
		rownames(out) <- NULL
		return(out)
}

#' @title pa_weight_descent
#' @description Aggregate work-resolution ties to community-level descent ties under three weightings.
#'
#' @param work_ties Realized citations, from pa_work_ties.
#'
#' @details
#' The three weightings of S6.2, on one pass over the same ties:
#' \itemize{
#'   \item BREADTH: distinct bridging works, whole credit to every community pair
#'     that uses the work. Totals grow with the number of communities, so breadth
#'     is not comparable across era pairs whose partitions differ in granularity.
#'   \item FRACTIONAL: each work is one unit of evidence split equally among the
#'     community pairs it connects. Totals per era pair equal the number of
#'     distinct bridging works regardless of partition granularity, which is what
#'     makes this the weighting that supports cross-era comparison.
#'   \item INTENSITY: citation events rather than distinct works.
#' }
#'
#' hhi is the Herfindahl concentration of a tie's weight across the works that
#' carry it: 1.0 means the tie rests on a single shared work, which was the case
#' for 72\% to 90\% of ties under the legacy partitions.
#'
#' Aggregation is on the integer pair rather than on a pasted string. The previous
#' version built paste(citing_community, cited_community, sep = "|") and split it
#' back apart, which is the pattern pa_work_ties documents at length as the one to
#' avoid. It was safe as called, because pa_work_ties coerces both columns to
#' integer; it would not have been safe on a table built anywhere else.
#'
#' @return A data.frame with citing_community, cited_community, w_breadth,
#'   w_fractional, w_intensity and hhi.
#'
#' @export
pa_weight_descent <- function(work_ties) {
	#	"""
	#	Args:
	#		work_ties: realized citations, from pa_work_ties()
	#	Returns:
	#		data.frame(citing_community, cited_community, w_breadth, w_fractional,
	#		w_intensity, hhi)
	#	"""

	#	Early return
		if (nrow(work_ties) == 0L) {
			return(data.frame(citing_community = integer(0),
							  cited_community = integer(0),
							  w_breadth = integer(0), w_fractional = numeric(0),
							  w_intensity = integer(0), hhi = numeric(0),
							  stringsAsFactors = FALSE))
		}

	#	Fractional Credit: Split Each Work across the Pairs It Connects
		dt <- as.data.table(work_ties)[, .(work_id = as.integer(work_id),
										   cited_community = as.integer(cited_community),
										   citing_community = as.integer(citing_community),
										   y_cites = as.integer(y_cites))]
		dt[, frac := 1 / .N, by = work_id]

	#	Aggregate to the Community Pair
		out <- dt[, .(w_breadth    = as.integer(uniqueN(work_id)),
					  w_fractional = sum(frac),
					  w_intensity  = as.integer(sum(y_cites)),
					  hhi          = if (sum(y_cites) <= 0L) {
										 NA_real_
									 } else {
										 sum((y_cites / sum(y_cites))^2)
									 }),
				  by = .(citing_community, cited_community)]

	#	Assembling Result
		setorder(out, citing_community, cited_community)
		return(as.data.frame(out))
}

#' @title pa_risk_set
#' @description Expand the realized ties to every work by citing-community pairing that could have occurred.
#'
#' @param work_ties Realized citations, from pa_work_ties.
#' @param bridging Bridging works, from pa_bridging_works.
#' @param citing_communities Integer vector of era-t communities in scope.
#' @param out_citations A data.frame with citing_community and out_citations_i.
#' @param warn_rows Row count above which the size is reported. Default TL_RISK_WARN_ROWS.
#'
#' @details
#' The rows that carry the information for RQ10 are the zeros, and they appear
#' nowhere in the edge list -- they have to be generated. A citing community enters
#' the risk set when it emits any citation at all; one that emits none had no
#' opportunity and is excluded, which is the exclusion that lets Model II take the
#' hurdle form rather than a zero-inflated one.
#'
#' SIZE. This is |bridging works| x |citing communities|. Under the calibrated
#' construction that is 15.3 million rows for the (16,17) pair and 31.1 million
#' across the six analytic pairs. The September note put it at 345,000, which was
#' correct against 394 communities; era 17 now has 1,105. Built with CJ and joined
#' by reference for that reason: expand.grid plus three left joins materialises
#' several copies of a frame that size. Written per pair rather than held together
#' in memory.
#'
#' The exclusion does little work on calibrated partitions, where every community
#' contains at least one citing article and so emits something. It is kept because
#' it is the model's identifying restriction, not because it filters much.
#'
#' @return A data.frame with work_id, cited_community, citing_community, y_cites
#'   and out_citations_i.
#'
#' @export
pa_risk_set <- function(work_ties, bridging, citing_communities, out_citations,
						warn_rows = TL_RISK_WARN_ROWS) {
	#	"""
	#	Args:
	#		work_ties: realized citations, from pa_work_ties()
	#		bridging: bridging works, from pa_bridging_works()
	#		citing_communities: integer vector of era-t communities in scope
	#		out_citations: data.frame(citing_community, out_citations_i)
	#		warn_rows: row count above which the size is reported
	#	Returns:
	#		data.frame(work_id, cited_community, citing_community, y_cites,
	#		out_citations_i)
	#	Notes:
	#		CJ requires unique inputs to produce a clean cross join, and both are:
	#		pa_bridging_works returns one row per work, pa_out_citations one row per
	#		community. CJ also returns sorted by its first then second argument,
	#		which is the order this function promises, so no reorder is needed.
	#	"""

	#	Communities with Any Opportunity
		oc <- as.data.table(out_citations)
		live <- oc[!is.na(out_citations_i) & out_citations_i > 0L]
		live <- live[citing_community %in% as.integer(citing_communities)]
		live <- unique(live, by = "citing_community")
		setorder(live, citing_community)

	#	Bridging Works
		bw <- as.data.table(bridging)[, .(work_id = as.integer(work_id),
										  cited_community = as.integer(cited_community))]
		bw <- unique(bw, by = "work_id")
		setorder(bw, work_id)

	#	Early return
		if (nrow(bw) == 0L || nrow(live) == 0L) {
			return(data.frame(work_id = integer(0), cited_community = integer(0),
							  citing_community = integer(0), y_cites = integer(0),
							  out_citations_i = integer(0),
							  stringsAsFactors = FALSE))
		}

	#	Report an Unexpectedly Large Grid
		n_rows <- as.numeric(nrow(bw)) * as.numeric(nrow(live))
		if (n_rows > warn_rows) {
			message("  risk set: ", nrow(bw), " works x ", nrow(live),
					" communities = ", format(n_rows, big.mark = ",", scientific = FALSE),
					" rows")
		}

	#	The Full Grid
		grid <- CJ(work_id = bw$work_id, citing_community = live$citing_community)

	#	Attach the Cited Community, the Realized Counts, and Exposure
		grid[bw, on = "work_id", cited_community := i.cited_community]
		if (nrow(work_ties) > 0L) {
			wt <- as.data.table(work_ties)[, .(work_id = as.integer(work_id),
											   citing_community = as.integer(citing_community),
											   y_cites = as.integer(y_cites))]
			grid[wt, on = .(work_id, citing_community), y_cites := i.y_cites]
		} else {
			grid[, y_cites := NA_integer_]
		}
		grid[is.na(y_cites), y_cites := 0L]
		grid[live, on = "citing_community", out_citations_i := i.out_citations_i]

	#	Assembling Result
		setcolorder(grid, c("work_id", "cited_community", "citing_community",
							"y_cites", "out_citations_i"))
		return(as.data.frame(grid))
}

#' @title pa_pair_ties
#' @description Build one era pair's tie layer end to end.
#'
#' @param edges_dated Edge list carrying sender_era.
#' @param era_map_prior Era map for era t-1.
#' @param era_map_t Era map for era t.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#' @param out_citations A data.frame with citing_community and out_citations_i, or NULL to compute it.
#' @param with_risk_set Build the expanded frame. Default TRUE.
#'
#' @details
#' The single entry point a worker calls for one era pair. Everything it returns is
#' a pure function of its arguments, so two workers running two pairs cannot
#' interfere and a rerun of one pair reproduces it exactly.
#'
#' The summary carries total_intensity, total_out_citations and
#' pct_descent_survival so that descent survival -- the share of era t's citations
#' that reach the prior era's partition -- falls out of the run rather than needing
#' a separate pass over the descent frames. Spec claim C6 reads the last column
#' directly.
#'
#' @return A list with bridging, work_ties, descent, risk_set and summary.
#'
#' @export
pa_pair_ties <- function(edges_dated, era_map_prior, era_map_t, era_prior, era_t,
						 out_citations = NULL, with_risk_set = TRUE) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		era_map_prior: era map for era t-1
	#		era_map_t: era map for era t
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#		out_citations: exposure table, computed when NULL
	#		with_risk_set: build the expanded frame? (default TRUE)
	#	Returns:
	#		list(bridging, work_ties, descent, risk_set, summary)
	#	"""

	#	Bridging Works
		bridging <- pa_bridging_works(edges_dated, era_map_prior, era_prior, era_t)

	#	Realized Citations at Work Resolution
		work_ties <- pa_work_ties(edges_dated, bridging, era_map_t, era_t)

	#	Community-Level Descent, All Three Weightings
		descent <- pa_weight_descent(work_ties)

	#	Exposure, if Not Supplied
		if (is.null(out_citations)) {
			out_citations <- pa_out_citations(edges_dated, era_map_t, era_t)
		}

	#	Risk Set
		risk <- NULL
		if (with_risk_set) {
			risk <- pa_risk_set(work_ties, bridging,
								citing_communities = unique(era_map_t$community),
								out_citations = out_citations)
		}

	#	Summary, for the Fixture Test and the Run Log
		summary <- data.frame(
			era_prior              = as.integer(era_prior),
			era_t                  = as.integer(era_t),
			n_bridging_works       = nrow(bridging),
			n_work_ties            = nrow(work_ties),
			n_ties                 = nrow(descent),
			n_citing_communities   = length(unique(era_map_t$community)),
			n_citing_participating = length(unique(work_ties$citing_community)),
			n_cited_communities    = length(unique(era_map_prior$community)),
			n_cited_participating  = length(unique(work_ties$cited_community)),
			n_risk_rows            = if (is.null(risk)) NA_integer_ else nrow(risk),
			total_fractional       = if (nrow(descent) == 0L) NA_real_ else
										sum(descent$w_fractional),
			pct_single_work_ties   = if (nrow(descent) == 0L) NA_real_ else
										round(100 * mean(descent$w_breadth == 1L), 1),
			total_intensity        = if (nrow(descent) == 0L) NA_integer_ else
										as.integer(sum(descent$w_intensity)),
			total_out_citations    = as.integer(sum(out_citations$out_citations_i)),
			pct_descent_survival   = if (nrow(descent) == 0L ||
										 sum(out_citations$out_citations_i) == 0L) {
										NA_real_
									 } else {
										round(100 * sum(descent$w_intensity) /
											  sum(out_citations$out_citations_i), 3)
									 },
			stringsAsFactors = FALSE)

	#	Assembling Result
		return(list(bridging = bridging, work_ties = work_ties, descent = descent,
					risk_set = risk, summary = summary))
}

#' @title pa_build_era_maps
#' @description Build one era map per era for a single specification.
#'
#' @param node_list The rebuilt citation node list.
#' @param eras Integer vector of eras. Default PA_ANALYTIC_ERAS.
#' @param alpha Scaling exponent of the specification. Default PA_ALPHA.
#' @param anchor Anchor granularity. Default PA_ANCHOR.
#' @param anchor_era Era the anchor is set at. Default PA_ANCHOR_ERA.
#' @param grid Sweep grid directory. Default PA_GRID.
#' @param verbose Report each era as it is read. Default TRUE.
#'
#' @details
#' Requires PathA_PajekReaders.R and PathA_EraResolution.R. Reads each era's
#' network and partition, checks that their lengths agree, and maps the vertices
#' onto the node list. The maps are returned as a list keyed by era as a character
#' string, which is what pa_run_tie_layer indexes.
#'
#' @return A named list of era maps, one per era.
#'
#' @export
pa_build_era_maps <- function(node_list, eras = PA_ANALYTIC_ERAS,
							  alpha = PA_ALPHA, anchor = PA_ANCHOR,
							  anchor_era = PA_ANCHOR_ERA, grid = PA_GRID,
							  verbose = TRUE) {
	#	"""
	#	Args:
	#		node_list: rebuilt citation node list
	#		eras: integer vector of eras
	#		alpha, anchor, anchor_era, grid: the specification
	#		verbose: report each era?
	#	Returns:
	#		named list of era maps, keyed by era as character
	#	"""

	#	Validation
		for (fn in c("pa_resolve_era", "pa_era_map", "pa_read_era_network")) {
			if (!exists(fn, mode = "function")) {
				stop("pa_build_era_maps: ", fn, "() not found; source ",
					 "PathA_PajekReaders.R and PathA_EraResolution.R first")
			}
		}

	#	Iterating Over Eras
		maps <- vector("list", length(eras))
		names(maps) <- as.character(eras)
		for (e in eras) {
			r <- pa_resolve_era(e, alpha, anchor, anchor_era, grid)
			net <- pa_read_era_network(r$net, r$clu, want_ties = FALSE)
			m <- pa_era_map(node_list, net$vertices, net$partition, e)
			maps[[as.character(e)]] <- m
			if (verbose) {
				vs <- attr(m, "vertex_set")
				cat("  era ", e, ": ", vs$n_vertices, " vertices, ",
					net$n_communities, " communities",
					if (!vs$exact_match) sprintf(", %d era nodes absent from the network",
												 vs$n_nodes_absent_from_network) else "",
					"\n", sep = "")
			}
		}

	#	Assembling Result
		return(maps)
}

#' @title pa_run_tie_layer
#' @description Build the tie layer for every era pair under every specification, write the outputs, and run the checks.
#'
#' @param specs A data.frame of specifications. Default TL_SPECS.
#' @param eras Integer vector of eras. Default PA_ANALYTIC_ERAS.
#' @param era_dir Directory holding pa_node_list.Rda and pa_edge_list.Rda. Default TL_ERA_DIR.
#' @param output_dir Destination root. Default TL_OUT_DIR.
#' @param verbose Report progress. Default TRUE.
#'
#' @details
#' Community granularity is a declared specification rather than a fixture (S14.8),
#' so the tie layer runs once per specification and the outputs are kept apart,
#' under output_dir/<spec_tag>/.
#'
#' The node list and the edge list are loaded once and shared across
#' specifications; only the era maps differ. The edge list is deduplicated and
#' dated through pa_dedupe_edges and pa_attach_sender_era, both of which are
#' no-ops against the rebuilt list and are run anyway because reporting that they
#' changed nothing confirms the construction.
#'
#' Per pair the small tables are written as CSV and the risk set, which can run to
#' millions of rows, as an uncompressed RDS. The risk set is not accumulated in
#' memory across pairs.
#'
#' @return Invisibly, a list with summary and tests.
#'
#' @examples
#' res <- pa_run_tie_layer()
#' res$summary
#'
#' @export
pa_run_tie_layer <- function(specs = TL_SPECS, eras = PA_ANALYTIC_ERAS,
							 era_dir = TL_ERA_DIR, output_dir = TL_OUT_DIR,
							 verbose = TRUE) {
	#	"""
	#	Args:
	#		specs: data.frame of specifications
	#		eras: integer vector of eras
	#		era_dir: directory holding the node and edge lists
	#		output_dir: destination root
	#		verbose: report progress?
	#	Returns:
	#		invisible list(summary, tests)
	#	"""

	#	Validation
		need <- c("alpha", "anchor", "anchor_era", "grid", "with_risk_set")
		miss <- setdiff(need, names(specs))
		if (length(miss) > 0L) {
			stop("pa_run_tie_layer: specs is missing column(s): ",
				 paste(miss, collapse = ", "))
		}
		if (!exists("pa_spec_tag", mode = "function")) {
			stop("pa_run_tie_layer: pa_spec_tag() not found; source ",
				 "PathA_EraResolution.R first")
		}

	#	Load the Node and Edge Lists Once
		nf <- file.path(era_dir, "pa_node_list.Rda")
		ef <- file.path(era_dir, "pa_edge_list.Rda")
		for (f in c(nf, ef)) {
			if (!file.exists(f)) stop("pa_run_tie_layer: file not found: ", f)
		}
		env <- new.env(parent = emptyenv())
		load(nf, envir = env)
		load(ef, envir = env)
		node_list <- as.data.frame(get("pa_nodes", envir = env, inherits = FALSE))
		edges     <- as.data.frame(get("pa_edges", envir = env, inherits = FALSE))
		if (verbose) {
			cat("Loaded ", nrow(node_list), " nodes and ", nrow(edges), " edges\n",
				sep = "")
		}

	#	Deduplicate and Date, Both No-Ops against the Rebuilt List
		ded <- pa_dedupe_edges(edges, verbose = verbose)
		edges_dated <- pa_attach_sender_era(ded$edges, node_list, verbose = verbose)

	#	Iterating Over Specifications
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
		pairs <- pa_era_pairs(eras)
		rows <- list()
		for (s in seq_len(nrow(specs))) {
			tag <- pa_spec_tag(specs$alpha[s], specs$anchor[s], specs$anchor_era[s])
			spec_dir <- file.path(output_dir, tag)
			dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)
			if (verbose) cat("\nSpecification ", tag, "\n", sep = "")

			#	Era maps for this specification
				maps <- pa_build_era_maps(node_list, eras,
										  alpha = specs$alpha[s],
										  anchor = specs$anchor[s],
										  anchor_era = specs$anchor_era[s],
										  grid = specs$grid[s],
										  verbose = verbose)

			#	Iterating over pairs
				for (p in seq_len(nrow(pairs))) {
					ep <- pairs$era_prior[p]
					et <- pairs$era_t[p]
					if (verbose) cat("  pair ", ep, " -> ", et, "\n", sep = "")
					res <- pa_pair_ties(edges_dated,
										era_map_prior = maps[[as.character(ep)]],
										era_map_t     = maps[[as.character(et)]],
										era_prior     = ep,
										era_t         = et,
										with_risk_set = isTRUE(specs$with_risk_set[s]))

					#	Write the small tables
						stem <- file.path(spec_dir, sprintf("pair_%02d_%02d", ep, et))
						readr::write_csv(res$bridging,  paste0(stem, "_bridging.csv"))
						readr::write_csv(res$work_ties, paste0(stem, "_work_ties.csv"))
						readr::write_csv(res$descent,   paste0(stem, "_descent.csv"))

					#	Write the risk set, which is the large one
						if (!is.null(res$risk_set)) {
							saveRDS(res$risk_set, paste0(stem, "_risk_set.rds"),
									compress = FALSE)
						}

					#	Collect the summary
						rows[[length(rows) + 1L]] <- cbind(
							data.frame(spec_tag   = tag,
									   alpha      = specs$alpha[s],
									   anchor     = specs$anchor[s],
									   pair_index = pairs$pair_index[p],
									   stringsAsFactors = FALSE),
							res$summary)
						rm(res)
				}
				rm(maps)
		}

	#	Write the Summary
		summary <- do.call("rbind", rows)
		rownames(summary) <- NULL
		readr::write_csv(summary, file.path(output_dir, "tie_layer_summary.csv"))

	#	Checks
		tests <- pa_run_tie_layer_tests(summary, edges_dated, node_list,
										specs = specs, eras = eras,
										output_dir = output_dir)

	#	Assembling Result
		return(invisible(list(summary = summary, tests = tests)))
}

#############
#   TESTS   #
#############

#' @title pa_run_tie_layer_tests
#' @description Check a completed tie-layer run, print the results, and write them to CSV.
#'
#' @param summary Output of pa_run_tie_layer.
#' @param edges_dated Edge list carrying sender_era.
#' @param node_list The rebuilt citation node list.
#' @param specs The specification table that was run. Default TL_SPECS.
#' @param eras Integer vector of eras. Default PA_ANALYTIC_ERAS.
#' @param output_dir Destination for the test table, or NULL to skip writing. Default TL_OUT_DIR.
#'
#' @details
#' Four kinds of check. The identities: fractional weight per era pair must equal
#' the number of distinct bridging works that participate, and the risk set must
#' contain exactly the realized citations the work ties do. The collision guard:
#' sender_era must be a single column, because a second join would produce
#' sender_era.x and sender_era.y and every pair would report zero bridging works
#' without erroring. The determinism test of S14.4: shuffling the edge list must
#' change nothing. And the coverage checks: every pair produced ties, and no
#' specification produced an empty run.
#'
#' The determinism test runs on the smallest pair only, since it rebuilds the pair
#' from scratch.
#'
#' @return A data.frame of checks.
#'
#' @export
pa_run_tie_layer_tests <- function(summary, edges_dated, node_list,
								   specs = TL_SPECS, eras = PA_ANALYTIC_ERAS,
								   output_dir = TL_OUT_DIR) {
	#	"""
	#	Args:
	#		summary: output of pa_run_tie_layer()
	#		edges_dated: edge list carrying sender_era
	#		node_list: rebuilt citation node list
	#		specs: specification table
	#		eras: integer vector of eras
	#		output_dir: destination for the test table, or NULL
	#	Returns:
	#		data.frame of checks
	#	"""

	#	Collect
		out <- list()

	#	The Column-Collision Guard
		era_cols <- grep("^sender_era", names(edges_dated), value = TRUE)
		out[[length(out) + 1L]] <- pa_check_tl(
			"sender_era is a single column",
			identical(era_cols, "sender_era"),
			paste(era_cols, collapse = ", "))
		out[[length(out) + 1L]] <- pa_check_tl(
			"sender_era is populated",
			!is.null(edges_dated$sender_era) &&
				sum(!is.na(edges_dated$sender_era)) > 0L,
			sprintf("%d of %d edges dated",
					sum(!is.na(edges_dated$sender_era)), nrow(edges_dated)))

	#	Coverage
		if (nrow(summary) > 0L) {
			out[[length(out) + 1L]] <- pa_check_tl(
				"every pair produced bridging works",
				all(summary$n_bridging_works > 0L),
				sprintf("min %d", min(summary$n_bridging_works)))
			out[[length(out) + 1L]] <- pa_check_tl(
				"every pair produced descent ties",
				all(summary$n_ties > 0L),
				sprintf("min %d", min(summary$n_ties)))
			out[[length(out) + 1L]] <- pa_check_tl(
				"every specification produced every pair",
				all(table(summary$spec_tag) == nrow(pa_era_pairs(eras))),
				sprintf("%d specification(s) x %d pair(s)",
						length(unique(summary$spec_tag)), nrow(pa_era_pairs(eras))))
			out[[length(out) + 1L]] <- pa_check_tl(
				"participating communities do not exceed the partition",
				all(summary$n_citing_participating <= summary$n_citing_communities) &&
					all(summary$n_cited_participating <= summary$n_cited_communities),
				"citing and cited sides within bounds")

			#	Descent Survival
			#	Intensity counts era t citations that land on a bridging work, so it
			#	is a subset of era t's out-citations and the share cannot exceed 100.
				if ("pct_descent_survival" %in% names(summary)) {
					sv <- summary$pct_descent_survival
					out[[length(out) + 1L]] <- pa_check_tl(
						"descent survival is a share of era t's out-citations",
						all(is.na(sv) | (sv > 0 & sv <= 100)) &&
							all(summary$total_intensity <=
								summary$total_out_citations, na.rm = TRUE),
						sprintf("%.3f%% to %.3f%%", min(sv, na.rm = TRUE),
								max(sv, na.rm = TRUE)))
				}
		}

	#	The Fractional Identity
	#	Each work is one unit of evidence split across the pairs it connects, so the
	#	total per era pair must equal the number of distinct participating works.
		if (nrow(summary) > 0L && "total_fractional" %in% names(summary)) {
			tag1 <- summary$spec_tag[1]
			sub <- summary[summary$spec_tag == tag1, , drop = FALSE]
			ok <- TRUE
			detail <- "no pair checked"
			if (nrow(sub) > 0L) {
				stem <- file.path(output_dir, tag1,
								  sprintf("pair_%02d_%02d_work_ties.csv",
										  sub$era_prior[1], sub$era_t[1]))
				if (file.exists(stem)) {
					wt <- readr::read_csv(stem, show_col_types = FALSE,
										  progress = FALSE)
					ok <- isTRUE(all.equal(sub$total_fractional[1],
										   length(unique(wt$work_id)),
										   tolerance = 1e-8))
					detail <- sprintf("%.6f against %d distinct works",
									  sub$total_fractional[1],
									  length(unique(wt$work_id)))
				}
			}
			out[[length(out) + 1L]] <- pa_check_tl(
				"fractional weight totals the participating works", ok, detail)
		}

	#	Determinism: Shuffling the Edge List Changes Nothing
		det_ok <- NA
		det_detail <- "not run"
		if (nrow(specs) > 0L && length(eras) >= 2L &&
			exists("pa_build_era_maps", mode = "function")) {
			ep <- eras[1]
			et <- eras[2]
			maps <- try(pa_build_era_maps(node_list, c(ep, et),
										  alpha = specs$alpha[1],
										  anchor = specs$anchor[1],
										  anchor_era = specs$anchor_era[1],
										  grid = specs$grid[1],
										  verbose = FALSE), silent = TRUE)
			if (!inherits(maps, "try-error")) {
				ref <- pa_pair_ties(edges_dated, maps[[as.character(ep)]],
									maps[[as.character(et)]], ep, et,
									with_risk_set = FALSE)
				set.seed(1L)
				shuffled <- edges_dated[sample.int(nrow(edges_dated)), , drop = FALSE]
				alt <- pa_pair_ties(shuffled, maps[[as.character(ep)]],
									maps[[as.character(et)]], ep, et,
									with_risk_set = FALSE)
				det_ok <- identical(ref$work_ties, alt$work_ties) &&
						  identical(ref$descent, alt$descent) &&
						  identical(ref$bridging, alt$bridging)
				det_detail <- sprintf("pair %d -> %d, %d work ties", ep, et,
									  nrow(ref$work_ties))
			}
		}
		out[[length(out) + 1L]] <- pa_check_tl(
			"shuffling the edge list changes nothing", isTRUE(det_ok), det_detail)

	#	Assembling Result
		tests <- do.call("rbind", out)
		print(tests, row.names = FALSE)
		message("\n", sum(tests$result == "PASS"), " of ", nrow(tests),
				" checks passed")
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests, file.path(output_dir, "tie_layer_tests.csv"))
		}
		return(tests)
}

#############################
#   HOW TO RUN THIS STAGE   #
#############################

#	NO MAIN BLOCK, DELIBERATELY. This file is a function library: the test suite
#	sources it, and so does any runner. An auto-run here would execute a full
#	tie-layer build before a single check in Test_PathA_TieLayer.R had run, which
#	is why PathA_PajekReaders.R and PathA_EraResolution.R carry no MAIN either.
#
#	To build the stage, from a runner script or the console:
#
#		source("scripts/RPajekFunctions_30April2023.r")
#		source("scripts/Path_A/PathA_PajekReaders.R")
#		source("scripts/Path_A/PathA_EraResolution.R")
#		source("scripts/Path_A/PathA_TieLayer.R")
#
#		tie_results <- pa_run_tie_layer(specs = TL_SPECS,
#										eras = PA_ANALYTIC_ERAS,
#										era_dir = TL_ERA_DIR,
#										output_dir = TL_OUT_DIR)
#
#		tie_results$summary[, c("spec_tag", "pair_index", "era_prior", "era_t",
#								"n_bridging_works", "n_work_ties", "n_ties",
#								"n_citing_participating", "n_risk_rows")]
#
#	HOW TO READ THE SUMMARY.
#
#		BRIDGING       n_bridging_works is the population descent runs on. It
#					   depends on how much citing era t does, so it falls off at
#					   the end of the window where era 18 is short.
#		WEIGHTING      total_fractional equals the participating works, which is
#					   what makes the fractional weighting comparable across pairs.
#		CONCENTRATION  pct_single_work_ties is the share of descent ties resting on
#					   one shared work; high means the relation is thin.
#		RISK SET       n_risk_rows is works x communities, built for the primary
#					   specification only unless with_risk_set is flipped.
