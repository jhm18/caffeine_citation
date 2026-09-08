#	PathA_TieLayer.R -- Bridging Works, Descent Ties, and the Model II Frame
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHERE THIS SITS. Stage 3 of the pipeline order of operations (specification
#	S14.1). Requires PathA_PajekReaders.R and PathA_EraResolution.R. Its output
#	feeds stage 4, which decides from work_ties which communities participate in
#	any tie and therefore which ones are worth labeling at all.
#	Tested by tests/Test_PathA_TieLayer.R.
#
#	This file
#	produces the work-resolution table that is simultaneously the source of the
#	descent networks and the model frame for Model II, which is why the
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
#	specification adopts (S6.1), and it is why only 0.54% and 1.55% of an era's
#	emitted citations survive into the descent network.
#
#	Determinism is the governing constraint. Every function here returns output that
#	is invariant to the row order of its inputs; the regression test for defect 1 is
#	that shuffling the edge list changes nothing (S14.4).
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

#################
#   FUNCTIONS   #
#################

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
		#	"""

		#	Validation
			need <- c("sender_id", "target_id")
			miss <- setdiff(need, names(edges))
			if (length(miss) > 0L) stop("Edge list missing column(s): ", paste(miss, collapse = ", "))

		#	Order deterministically, then reduce on the identifier pair
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

		#	Assemble result
			return(list(edges = edges, n_before = n_before,
						n_after = n_after, n_removed = n_before - n_after))
	}

#	Attach the Citing Era to Each Edge
	pa_attach_sender_era <- function(edges, node_list) {
		#	"""
		#	Args:
		#		edges: deduplicated citation edge list
		#		node_list: full citation node list
		#	Returns:
		#		data.frame, edges plus an integer sender_era column
		#	Notes:
		#		An edge is dated by the era of the article that EMITS it. This is the
		#		one place the pipeline needs that mapping, so it is done once and the
		#		result reused across every era pair rather than recomputed per pair.
		#	"""

		#	Reduce the node list to the lookup
			key <- data.frame(sender_id  = node_list$node_id,
							  sender_era = as.integer(node_list$era),
							  stringsAsFactors = FALSE)
			key <- key[!duplicated(key$sender_id), , drop = FALSE]

		#	Join
			out <- dplyr::left_join(edges, key, by = "sender_id", relationship = "many-to-one")

		#	Assemble result
			return(out)
	}

#	Identify the Bridging Works for One Era Pair
	pa_bridging_works <- function(edges_dated, era_map_prior, era_prior, era_t) {
		#	"""
		#	Args:
		#		edges_dated: edge list carrying sender_era, from pa_attach_sender_era()
		#		era_map_prior: era map for era t-1, from pa_era_map()
		#		era_prior: integer era t-1
		#		era_t: integer era t
		#	Returns:
		#		data.frame(work_id, work_label, cited_community) for works that bridge
		#		the pair, ordered by work_id
		#	Notes:
		#		The both-periods requirement in full. A work bridges the pair when all
		#		three hold:
		#
		#			(a) it belongs to era t-1, so it carries a community in that era's
		#				partition -- "belonged to an era t-1 community";
		#			(b) some era t-1 article cites it -- "was actively cited during
		#				that earlier era";
		#			(c) some era t article cites it -- the descent citation itself.
		#
		#		(a) is what makes cited_community well defined and is why the cited
		#		community follows from the work rather than being an independent index
		#		in the Model II frame.
		#	"""

		#	Works cited during each era
			cited_prior <- unique(edges_dated$target_id[!is.na(edges_dated$sender_era) &
														edges_dated$sender_era == era_prior])
			cited_t     <- unique(edges_dated$target_id[!is.na(edges_dated$sender_era) &
														edges_dated$sender_era == era_t])

		#	Condition (b) and (c)
			both <- intersect(cited_prior, cited_t)

		#	Condition (a): the work must sit in era t-1's partition
			owners <- era_map_prior[c("node_id", "node_label", "community")]
			names(owners) <- c("work_id", "work_label", "cited_community")
			out <- owners[owners$work_id %in% both, , drop = FALSE]

		#	Assemble result
			out <- out[order(out$work_id, method = "radix"), , drop = FALSE]
			rownames(out) <- NULL
			return(out)
	}

#	Build the Work-Resolution Tie Table
	pa_work_ties <- function(edges_dated, bridging, era_map_t, era_t) {
		#	"""
		#	Args:
		#		edges_dated: edge list carrying sender_era
		#		bridging: bridging works, from pa_bridging_works()
		#		era_map_t: era map for era t, from pa_era_map()
		#		era_t: integer era t
		#	Returns:
		#		data.frame(work_id, cited_community, citing_community, y_cites)
		#		for REALIZED citations only, ordered by the three keys
		#	Notes:
		#		This is where the arbitrary-attribution defect is repaired. Every era-t
		#		edge into a bridging work is kept and the citing article is mapped to
		#		its community; the aggregation to (work, citing community) is a count
		#		of distinct citing articles, so a work cited by three communities
		#		yields three rows rather than one.
		#
		#		y_cites counts distinct citing ARTICLES, which after deduplication is
		#		the same as counting citation events -- an article cites a work once.
		#
		#		The aggregation groups on sorted integer keys rather than on a pasted
		#		string. node_id is stored as a DOUBLE in the node list, and R renders
		#		round doubles in scientific notation -- as.character(200000) is
		#		"2e+05" while as.character(200000L) is "200000". Any key built by
		#		paste() over those columns is therefore sensitive to storage mode and
		#		to the scipen option, which is exactly the kind of dependency that
		#		survives every test until the one run where it does not.
		#	"""

		#	Era t edges into the bridging works
			e <- edges_dated[!is.na(edges_dated$sender_era) & edges_dated$sender_era == era_t, , drop = FALSE]
			e <- e[e$target_id %in% bridging$work_id, c("sender_id", "target_id"), drop = FALSE]

		#	Map the citing article to its community
			citers <- data.frame(sender_id        = era_map_t$node_id,
								 citing_community = era_map_t$community,
								 stringsAsFactors = FALSE)
			citers <- citers[!duplicated(citers$sender_id), , drop = FALSE]
			e <- dplyr::left_join(e, citers, by = "sender_id", relationship = "many-to-one")

		#	A citing article outside era t's network cannot be attributed
			e <- e[!is.na(e$citing_community), , drop = FALSE]

		#	Attach the cited community, which follows from the work
			names(e)[names(e) == "target_id"] <- "work_id"
			e <- dplyr::left_join(e, bridging[c("work_id", "cited_community")],
								  by = "work_id", relationship = "many-to-one")

		#	Aggregate once, at the end
			if (nrow(e) == 0L) {
				return(data.frame(work_id = integer(0), cited_community = integer(0),
								  citing_community = integer(0), y_cites = integer(0)))
			}
			k <- data.frame(work_id          = as.integer(e$work_id),
							cited_community  = as.integer(e$cited_community),
							citing_community = as.integer(e$citing_community),
							stringsAsFactors = FALSE)
			ord <- order(k$work_id, k$cited_community, k$citing_community, method = "radix")
			k <- k[ord, , drop = FALSE]
			first <- !duplicated(k)
			grp <- cumsum(first)
			out <- k[first, , drop = FALSE]
			out$y_cites <- as.integer(tabulate(grp, nbins = sum(first)))

		#	Assemble result
			out <- out[order(out$work_id, out$cited_community, out$citing_community, method = "radix"), , drop = FALSE]
			rownames(out) <- NULL
			return(out)
	}

#	Expand to the Risk Set
	pa_risk_set <- function(work_ties, bridging, citing_communities, out_citations) {
		#	"""
		#	Args:
		#		work_ties: realized citations, from pa_work_ties()
		#		bridging: bridging works, from pa_bridging_works()
		#		citing_communities: integer vector of era-t communities in scope
		#		out_citations: data.frame(citing_community, out_citations_i)
		#	Returns:
		#		data.frame(work_id, cited_community, citing_community, y_cites,
		#		out_citations_i) covering every pairing that COULD have occurred
		#	Notes:
		#		The rows that carry the information for RQ10 are the zeros, and they
		#		appear nowhere in the edge list -- they have to be generated. A citing
		#		community enters the risk set when it emits any citation at all; one
		#		that emits none had no opportunity and is excluded, which is the
		#		exclusion that lets Model II take the hurdle form rather than a
		#		zero-inflated one.
		#
		#		Size warning: this is |bridging works| x |citing communities|. For the
		#		2019-2020 pair that is 877 x 394, on the order of 345,000 rows. Over
		#		eleven pairs the frames are large but not unreasonable; they are
		#		written per pair rather than held together in memory.
		#	"""

		#	Communities with any opportunity
			live <- out_citations[!is.na(out_citations$out_citations_i) &
								  out_citations$out_citations_i > 0L, , drop = FALSE]
			live <- live[live$citing_community %in% citing_communities, , drop = FALSE]
			live <- live[order(live$citing_community, method = "radix"), , drop = FALSE]

		#	The full grid
			grid <- expand.grid(work_id          = bridging$work_id,
								citing_community = live$citing_community,
								KEEP.OUT.ATTRS   = FALSE,
								stringsAsFactors = FALSE)
			grid <- dplyr::left_join(grid, bridging[c("work_id", "cited_community")],
									 by = "work_id", relationship = "many-to-one")

		#	Fill in the realized counts
			grid <- dplyr::left_join(grid, work_ties[c("work_id", "citing_community", "y_cites")],
									 by = c("work_id", "citing_community"), relationship = "many-to-one")
			grid$y_cites[is.na(grid$y_cites)] <- 0L

		#	Attach exposure
			grid <- dplyr::left_join(grid, live, by = "citing_community", relationship = "many-to-one")

		#	Assemble result
			grid <- grid[order(grid$work_id, grid$citing_community, method = "radix"), , drop = FALSE]
			rownames(grid) <- NULL
			return(grid[c("work_id", "cited_community", "citing_community", "y_cites", "out_citations_i")])
	}

#	Weight the Descent Ties
	pa_weight_descent <- function(work_ties) {
		#	"""
		#	Args:
		#		work_ties: realized citations, from pa_work_ties()
		#	Returns:
		#		data.frame(citing_community, cited_community, w_breadth, w_fractional,
		#		w_intensity, hhi)
		#	Notes:
		#		The three weightings of S6.2, on one pass over the same ties:
		#
		#		BREADTH		distinct bridging works, whole credit to every community
		#					pair that uses the work. Totals grow with the number of
		#					communities, so breadth is not comparable across era pairs
		#					whose partitions differ in granularity.
		#		FRACTIONAL	each work is one unit of evidence split equally among the
		#					community pairs it connects. Totals per era pair equal the
		#					number of distinct bridging works regardless of partition
		#					granularity, which is what makes this the weighting that
		#					supports cross-year comparison.
		#		INTENSITY	citation events rather than distinct works.
		#
		#		hhi is the Herfindahl concentration of a tie's weight across the works
		#		that carry it: 1.0 means the tie rests on a single shared work, which
		#		is the case for 72% to 90% of ties in this network.
		#	"""

		#	Early return
			if (nrow(work_ties) == 0L) {
				return(data.frame(citing_community = integer(0), cited_community = integer(0),
								  w_breadth = integer(0), w_fractional = numeric(0),
								  w_intensity = integer(0), hhi = numeric(0)))
			}

		#	Fractional credit: split each work across the pairs it connects
			pairs_per_work <- table(work_ties$work_id)
			work_ties$frac <- 1 / as.numeric(pairs_per_work[as.character(work_ties$work_id)])

		#	Aggregate to the community pair
			key <- paste(work_ties$citing_community, work_ties$cited_community, sep = "|")
			breadth   <- tapply(work_ties$work_id, key, function(x) length(unique(x)))
			fractional <- tapply(work_ties$frac,    key, sum)
			intensity <- tapply(work_ties$y_cites,  key, sum)

		#	Concentration across the works carrying each tie
			hhi <- tapply(seq_len(nrow(work_ties)), key, function(idx) {
				w <- work_ties$y_cites[idx]
				s <- sum(w)
				if (s <= 0) return(NA_real_)
				return(sum((w / s)^2))
			})

		#	Assemble result
			nm <- names(breadth)
			parts <- do.call("rbind", strsplit(nm, "|", fixed = TRUE))
			out <- data.frame(citing_community = as.integer(parts[, 1L]),
							  cited_community  = as.integer(parts[, 2L]),
							  w_breadth        = as.integer(breadth),
							  w_fractional     = as.numeric(fractional),
							  w_intensity      = as.integer(intensity),
							  hhi              = as.numeric(hhi),
							  stringsAsFactors = FALSE)
			out <- out[order(out$citing_community, out$cited_community, method = "radix"), , drop = FALSE]
			rownames(out) <- NULL
			return(out)
	}

#	Build One Era Pair's Tie Layer End to End
	pa_pair_ties <- function(edges_dated, era_map_prior, era_map_t, era_prior, era_t,
							 out_citations = NULL, with_risk_set = TRUE) {
		#	"""
		#	Args:
		#		edges_dated: edge list carrying sender_era
		#		era_map_prior: era map for era t-1
		#		era_map_t: era map for era t
		#		era_prior: integer era t-1
		#		era_t: integer era t
		#		out_citations: data.frame(citing_community, out_citations_i); computed
		#			from era_map_t and edges_dated when NULL
		#		with_risk_set: build the expanded frame? (default TRUE)
		#	Returns:
		#		list(bridging, work_ties, descent, risk_set, summary)
		#	Notes:
		#		The single entry point a worker calls for one era pair. Everything it
		#		returns is a pure function of its arguments, so two workers running two
		#		pairs cannot interfere and a rerun of one pair reproduces it exactly.
		#	"""

		#	Bridging works
			bridging <- pa_bridging_works(edges_dated, era_map_prior, era_prior, era_t)

		#	Realized citations at work resolution
			work_ties <- pa_work_ties(edges_dated, bridging, era_map_t, era_t)

		#	Community-level descent, all three weightings
			descent <- pa_weight_descent(work_ties)

		#	Exposure, if not supplied
			if (is.null(out_citations)) {
				out_citations <- pa_out_citations(edges_dated, era_map_t, era_t)
			}

		#	Risk set
			risk <- NULL
			if (with_risk_set) {
				risk <- pa_risk_set(work_ties, bridging,
									citing_communities = unique(era_map_t$community),
									out_citations = out_citations)
			}

		#	Summary, for the fixture test and the run log
			summary <- data.frame(
				era_prior            = as.integer(era_prior),
				era_t                = as.integer(era_t),
				n_bridging_works     = nrow(bridging),
				n_ties               = nrow(descent),
				n_citing_communities = length(unique(era_map_t$community)),
				n_citing_participating = length(unique(work_ties$citing_community)),
				n_cited_communities  = length(unique(era_map_prior$community)),
				n_cited_participating  = length(unique(work_ties$cited_community)),
				n_risk_rows          = if (is.null(risk)) NA_integer_ else nrow(risk),
				pct_single_work_ties = if (nrow(descent) == 0L) NA_real_ else
										round(100 * mean(descent$w_breadth == 1L), 1),
				stringsAsFactors = FALSE)

		#	Assemble result
			return(list(bridging = bridging, work_ties = work_ties, descent = descent,
						risk_set = risk, summary = summary))
	}

#	Citations Emitted per Era-t Community
	pa_out_citations <- function(edges_dated, era_map_t, era_t) {
		#	"""
		#	Args:
		#		edges_dated: edge list carrying sender_era
		#		era_map_t: era map for era t
		#		era_t: integer era t
		#	Returns:
		#		data.frame(citing_community, out_citations_i), one row per era-t
		#		community including those that emit nothing
		#	Notes:
		#		This is E^C of the specification: the opportunity a community had to
		#		make any citation at all. It counts every citation the community's
		#		articles emit, not only those reaching bridging works -- the whole
		#		point of an exposure term is that it is the denominator of chances,
		#		not of successes.
		#	"""

		#	Era t edges
			e <- edges_dated[!is.na(edges_dated$sender_era) & edges_dated$sender_era == era_t, , drop = FALSE]

		#	Map to community and count
			citers <- data.frame(sender_id        = era_map_t$node_id,
								 citing_community = era_map_t$community,
								 stringsAsFactors = FALSE)
			citers <- citers[!duplicated(citers$sender_id), , drop = FALSE]
			e <- dplyr::left_join(e[c("sender_id")], citers, by = "sender_id", relationship = "many-to-one")
			e <- e[!is.na(e$citing_community), , drop = FALSE]
			cnt <- table(e$citing_community)

		#	Every community appears, including the silent ones
			all_comm <- sort(unique(era_map_t$community))
			out <- data.frame(citing_community = all_comm,
							  out_citations_i  = as.integer(cnt[as.character(all_comm)]),
							  stringsAsFactors = FALSE)
			out$out_citations_i[is.na(out$out_citations_i)] <- 0L

		#	Assemble result
			rownames(out) <- NULL
			return(out)
	}
