#	PathA_Granularity.R -- Measuring the Granularity of an Era Partition
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	27 October 2026
#
#	WHAT THIS DOES. Takes a partition of an era network -- a .clu file written by
#	the Julia community detection -- and measures how coarsely it divides the era.
#	One measurement serves three purposes:
#
#		1.	THE CALIBRATION CURVE. Citer-weighted k_eff at a given gamma, measured
#			rather than approximated by m/P. The construction inverts this curve to
#			choose a gamma per era.
#
#		2.	THE VALIDATION MEASUREMENTS. The five requirements of the design
#			document: monotonicity of k_eff in gamma, run-to-run variation, partition
#			agreement across seeds, selection among near-equivalent partitions, and
#			the tolerance those imply.
#
#		3.	THE CONSTRUCTION SUMMARY. Equal granularity is one way to improve
#			comparability across eras, not a sufficient one, so the size distribution
#			is reported alongside the scalar: threshold counts, the largest-community
#			share, a size Gini, the singleton share.
#
#	WHY GRANULARITY IS DEFINED AS IT IS. Communities on these networks vary
#	enormously in size, so the arithmetic mean size misstates the community a
#	typical article belongs to. Measured across all 84 partitions of the first run,
#	the median community holds exactly ONE citing article at every alpha and every
#	anchor, while the largest holds hundreds. With w_c the citing articles in
#	community c and W their total,
#
#		k_eff = W^2 / sum_c w_c^2          (inverse Simpson, Hill order 2)
#		g     = W / k_eff = sum_c w_c^2 / W
#
#	so g is the number of citing articles in the community of a randomly chosen
#	citing article. See granularity_design.md, section 5.
#
#	Because the modal community is degenerate as an analytical unit, the usable
#	community count is one of the n_comm_citers_ge_* columns and never
#	n_communities.
#
#	THE BRIDGING POPULATION. pa_bridging_works() in PathA_TieLayer.R defines
#	descent on WORKS, not on citing articles: a work bridges the pair (e, e+1) when
#	it belongs to era e's partition, some era e article cites it, and some era e+1
#	article cites it. Conditions two and three require being CITED in two
#	consecutive eras, so a citing article with no in-degree can never bridge. The
#	population that carries descent is therefore not the population granularity is
#	calibrated on, and this script measures both so the gap can be seen rather than
#	assumed away.
#
#	A consequence worth watching: era e's bridging population depends on how many
#	articles era e+1 contains. Era 18 spans only 2020-2021, so era 17's bridging
#	population is truncated -- 13.8% of its cited works against 33 to 48% in eras
#	11 to 16. n_citers_next_era is reported so that truncation is visible in the
#	table rather than discovered later.
#
#	INPUT
#		data/eras_7yr/era<NN>_nodes.Rda       era_nodes, ordered by node_id, with
#		                                      within-era in_, out_ and total_degree.
#		                                      Row position IS the Pajek vertex index.
#		data/eras_7yr/pa_edge_list.Rda        pa_edges, with sender_id, target_id and
#		                                      sender_era
#		pajek_files/champ_sweep_7yr/**/*.clu  the partitions to measure
#
#	OUTPUT, under data/granularity/
#		partition_granularity.csv       one row per .clu file
#		partition_agreement.csv         one row per pair of partitions in a group
#		granularity_tests.csv           the test table
#
#	PROVENANCE. Settings are parsed from the path below the scan root, because the
#	specification lives in the directory name -- alpha1.00_g120_e14 -- while the
#	file name repeats it. rel_dir carries that whole relative path, so two files
#	with the same base name under different run directories stay distinguishable.
#
#	The resolution is NOT recoverable from any file name now on disk. Nothing
#	written so far records it, so gamma is NA throughout and a partition cannot be
#	traced to the run that produced it. The partition writer should put gamma and
#	the seed in the name.
#
#	CONVENTIONS. dplyr for joins only, readr for CSV, data.table for the heavy
#	aggregation, base R otherwise.

################
#   PACKAGES   #
################

#	Load Packages
	library(data.table)

#####################
#   CONFIGURATION   #
#####################

#	Input Paths
	ERA_DIR    <- "/workspace/caffeine_citation/data/eras_7yr"
	SWEEP_ROOT <- "/workspace/caffeine_citation/pajek_files/champ_sweep_7yr"

#	Output Path
	GRAN_DIR <- "/workspace/caffeine_citation/data/granularity"

#	Era Range
	ANALYTIC_ERAS <- 11:17

#	Size Floors for the Threshold Counts
#	k_eff is a diversity equivalent and not a count of analytical units. The number
#	of rows available to a community-level model is the number of communities above
#	whatever floor that model needs, so the floors are reported rather than one of
#	them being chosen here.
#
#	Four is the working floor: group behaviour shifts between a triad and a group of
#	four, so a community of four citing articles is the smallest that behaves like a
#	group rather than like a dyad with an attachment. Three and five bracket it, and
#	ten is reported for outcomes that need a distribution rather than a count.
	CITER_FLOORS <- c(1L, 2L, 3L, 4L, 5L, 10L)

###############
#   HELPERS   #
###############

#	Read a Pajek Partition
	pa_read_clu <- function(path) {
		#	"""
		#	Args:
		#		path: path to a .clu file
		#	Returns:
		#		integer vector of community labels, one per vertex, in vertex order
		#	Notes:
		#		Header lines begin with an asterisk and are dropped; blank lines are
		#		dropped. The declared vertex count is checked against the number of
		#		labels read when a *Vertices header is present, because a truncated
		#		.clu would otherwise join silently against the wrong rows.
		#	"""

		#	Validation
			if (!file.exists(path)) stop("pa_read_clu: file not found: ", path)

		#	Read and Split
			lines <- trimws(readLines(path, warn = FALSE))
			head_ix <- grep("^\\*", lines)
			declared <- NA_integer_
			if (length(head_ix) > 0L) {
				v <- regmatches(lines[head_ix[1]], regexpr("[0-9]+", lines[head_ix[1]]))
				if (length(v) == 1L) declared <- as.integer(v)
			}

		#	Labels
			body <- if (length(head_ix) > 0L) lines[-head_ix] else lines
			body <- body[nzchar(body)]
			lab  <- suppressWarnings(as.integer(body))

		#	Validation
			if (anyNA(lab)) {
				stop("pa_read_clu: ", path, " holds ", sum(is.na(lab)),
					 " unparseable label(s)")
			}
			if (!is.na(declared) && declared != length(lab)) {
				stop("pa_read_clu: ", path, " declares ", declared,
					 " vertices but carries ", length(lab), " labels")
			}

		#	Assembling Result
			return(lab)
	}

#	Gini Coefficient
	pa_gini <- function(x) {
		#	"""
		#	Args:
		#		x: non-negative numeric vector, such as community sizes
		#	Returns:
		#		numeric Gini coefficient, 0 for a degenerate input
		#	Notes:
		#		Computed from the sorted values, which is exact for a finite sample and
		#		linear after the sort.
		#	"""

		#	Validation
			if (length(x) == 0L) return(0)
			if (any(x < 0, na.rm = TRUE)) stop("pa_gini: values must be non-negative")

		#	Early return
			total <- sum(x)
			if (!is.finite(total) || total == 0) return(0)

		#	Sorted Accumulation
			v <- sort(as.numeric(x))
			n <- length(v)
			acc <- sum((2 * seq_len(n) - n - 1) * v)

		#	Assembling Result
			return(acc / (n * total))
	}

#	Effective Count and Granularity from Per-Community Counts
	pa_effective <- function(w) {
		#	"""
		#	Args:
		#		w: counts per community, zeros allowed and ignored
		#	Returns:
		#		list(total, n_nonzero, k_eff, g)
		#	Notes:
		#		k_eff = W^2 / sum w^2 and g = W / k_eff. Communities holding none of the
		#		counted population contribute nothing to either, which is why n_nonzero
		#		is returned beside them -- the number of communities carrying any of the
		#		population is not the number detected.
		#	"""

		#	Drop Empties
			v <- as.numeric(w[!is.na(w) & w > 0])

		#	Early return
			if (length(v) == 0L) {
				return(list(total = 0, n_nonzero = 0L, k_eff = NA_real_, g = NA_real_))
			}

		#	Inverse Simpson
			total <- sum(v)
			ss    <- sum(v^2)

		#	Assembling Result
			return(list(total = total, n_nonzero = length(v),
						k_eff = total^2 / ss, g = ss / total))
	}

#	Load One Era's Node Table in Vertex Order
	pa_load_era_nodes <- function(era, era_dir = ERA_DIR) {
		#	"""
		#	Args:
		#		era: integer era number
		#		era_dir: directory holding era<NN>_nodes.Rda
		#	Returns:
		#		data.table of era nodes, ordered by node_id
		#	Notes:
		#		PathA_Revised_Eras.R writes the .net with the Pajek vertex index equal to
		#		the row position of era_nodes, which is ordered by node_id. The ordering
		#		is reasserted here rather than trusted, because a .clu joined by position
		#		to a differently ordered table would be wrong without erroring.
		#	"""

		#	Validation
			f <- file.path(era_dir, sprintf("era%02d_nodes.Rda", era))
			if (!file.exists(f)) stop("pa_load_era_nodes: file not found: ", f)

		#	Load
			env <- new.env(parent = emptyenv())
			load(f, envir = env)
			if (!exists("era_nodes", envir = env, inherits = FALSE)) {
				stop("pa_load_era_nodes: ", f, " holds no era_nodes object")
			}
			dt <- as.data.table(get("era_nodes", envir = env, inherits = FALSE))

		#	Validation
			need <- c("node_id", "out_degree", "in_degree")
			miss <- setdiff(need, names(dt))
			if (length(miss) > 0L) {
				stop("pa_load_era_nodes: era ", era, " is missing column(s): ",
					 paste(miss, collapse = ", "))
			}

		#	Reassert Vertex Order
			setorder(dt, node_id)

		#	Assembling Result
			return(dt)
	}

#	Works Eligible to Bridge an Era Pair
	pa_bridging_eligible <- function(era, edges, node_ids) {
		#	"""
		#	Args:
		#		era: integer era e, the EARLIER era of the pair (e, e+1)
		#		edges: full dated edge list with sender_era
		#		node_ids: the era's node ids, in vertex order
		#	Returns:
		#		logical vector over node_ids, TRUE where the work can bridge (e, e+1)
		#	Notes:
		#		Conditions (b) and (c) of pa_bridging_works() in PathA_TieLayer.R: some
		#		era e article cites the work, and some era e+1 article cites it.
		#		Condition (a), that the work belong to era e's partition, holds for every
		#		row of node_ids by construction.
		#
		#		The era e+1 citations are CROSS-ERA arcs and do not appear in era e's
		#		network, so this cannot be read off the era files and needs the full
		#		edge list.
		#	"""

		#	Validation
			need <- c("target_id", "sender_era")
			miss <- setdiff(need, names(edges))
			if (length(miss) > 0L) {
				stop("pa_bridging_eligible: edges is missing column(s): ",
					 paste(miss, collapse = ", "))
			}

		#	Targets Cited in Each Era of the Pair
			cited_e  <- unique(edges[!is.na(sender_era) & sender_era == era, target_id])
			cited_e1 <- unique(edges[!is.na(sender_era) & sender_era == (era + 1L),
									 target_id])

		#	Assembling Result
			return(node_ids %in% intersect(cited_e, cited_e1))
	}

#	Citing Articles in the Following Era
	pa_next_era_citers <- function(era, edges) {
		#	"""
		#	Args:
		#		era: integer era e, the EARLIER era of the pair (e, e+1)
		#		edges: full dated edge list with sender_id and sender_era
		#	Returns:
		#		integer count of distinct era e+1 articles that cite anything
		#	Notes:
		#		Era e's bridging population is bounded by how much citing era e+1 does,
		#		so this number explains a thin bridging population rather than leaving
		#		it to look like a property of era e. Era 18 spans two years, which is
		#		why era 17's bridging population is a third of era 16's.
		#	"""

		#	Validation
			need <- c("sender_id", "sender_era")
			miss <- setdiff(need, names(edges))
			if (length(miss) > 0L) {
				stop("pa_next_era_citers: edges is missing column(s): ",
					 paste(miss, collapse = ", "))
			}

		#	Assembling Result
			return(length(unique(edges[!is.na(sender_era) &
									   sender_era == (era + 1L), sender_id])))
	}

#	Contingency Table of Two Partitions
	pa_contingency <- function(a, b) {
		#	"""
		#	Args:
		#		a: integer labels, partition one
		#		b: integer labels, partition two, same vertices in the same order
		#	Returns:
		#		list(n, counts, marg_a, marg_b), counts holding only non-zero cells
		#	Notes:
		#		The agreement indices need the full marginals but only the occupied
		#		cells of the joint table, and the joint table of two partitions of the
		#		same vertex set is very sparse.
		#	"""

		#	Validation
			if (length(a) != length(b)) {
				stop("pa_contingency: partitions differ in length")
			}
			if (length(a) == 0L) stop("pa_contingency: empty partitions")

		#	Occupied Cells
			dt <- data.table(a = as.integer(a), b = as.integer(b))
			cells <- dt[, .(nij = .N), by = .(a, b)]

		#	Marginals
			ma <- dt[, .(ai = .N), by = a]
			mb <- dt[, .(bj = .N), by = b]

		#	Assembling Result
			return(list(n = length(a), counts = cells, marg_a = ma, marg_b = mb))
	}

#	Adjusted Rand Index
	pa_ari <- function(a, b) {
		#	"""
		#	Args:
		#		a: integer labels, partition one
		#		b: integer labels, partition two
		#	Returns:
		#		numeric ARI, 1 for identical partitions
		#	Notes:
		#		Pair-counting form, computed from the occupied cells and the marginals.
		#	"""

		#	Contingency
			ct <- pa_contingency(a, b)
			n  <- as.numeric(ct$n)

		#	Pair Sums
			pairs_of <- function(x) x * (x - 1) / 2
			sum_ij <- sum(pairs_of(as.numeric(ct$counts$nij)))
			sum_i  <- sum(pairs_of(as.numeric(ct$marg_a$ai)))
			sum_j  <- sum(pairs_of(as.numeric(ct$marg_b$bj)))
			total  <- pairs_of(n)

		#	Early return
			if (total == 0) return(1)

		#	Adjustment
			expected <- sum_i * sum_j / total
			maximum  <- (sum_i + sum_j) / 2
			if (maximum == expected) {
				return(if (isTRUE(all.equal(sum_ij, expected))) 1 else 0)
			}

		#	Assembling Result
			return((sum_ij - expected) / (maximum - expected))
	}

#	Expected Mutual Information for One Marginal Pair
	pa_emi_cell <- function(ai, bj, n, lgam) {
		#	"""
		#	Args:
		#		ai: size of a community in partition one
		#		bj: size of a community in partition two
		#		n: number of vertices
		#		lgam: precomputed lgamma(0:n + 1)
		#	Returns:
		#		numeric contribution of one (ai, bj) pair to E[MI]
		#	Notes:
		#		The Vinh, Epps and Bailey (2010) permutation-model expectation. The
		#		overlap k runs from max(1, ai + bj - n) to min(ai, bj), and the
		#		hypergeometric weight is evaluated in log space so the factorials do not
		#		overflow.
		#
		#		Called once per DISTINCT marginal pair rather than once per cell.
		#		Community sizes repeat heavily on these partitions, so memoizing on
		#		(ai, bj) reduces the work by orders of magnitude over the naive double
		#		loop.
		#	"""

		#	Overlap Range
			k_lo <- max(1L, ai + bj - n)
			k_hi <- min(ai, bj)

		#	Early return
			if (k_hi < k_lo) return(0)

		#	Hypergeometric Weight in Log Space
			k <- k_lo:k_hi
			log_w <- lgam[ai + 1L] + lgam[bj + 1L] +
					 lgam[n - ai + 1L] + lgam[n - bj + 1L] -
					 lgam[n + 1L] - lgam[k + 1L] -
					 lgam[ai - k + 1L] - lgam[bj - k + 1L] -
					 lgam[n - ai - bj + k + 1L]

		#	Assembling Result
			return(sum((k / n) * log((n * k) / (ai * bj)) * exp(log_w)))
	}

#	Adjusted Mutual Information
	pa_ami <- function(a, b) {
		#	"""
		#	Args:
		#		a: integer labels, partition one
		#		b: integer labels, partition two
		#	Returns:
		#		list(mi, emi, ami, nmi, h_a, h_b)
		#	Notes:
		#		AMI corrects mutual information for the agreement expected by chance
		#		under the permutation model, which matters here because the partitions
		#		compared have different community counts and because a partition with
		#		many small communities inflates raw MI.
		#
		#		Normalized by the mean of the two entropies:
		#		ami = (MI - E[MI]) / (mean(H_a, H_b) - E[MI]).
		#
		#		The marginal vectors are named size_a and size_b so they cannot be
		#		shadowed by the data.table columns ai and bj assigned below.
		#	"""

		#	Contingency
			ct <- pa_contingency(a, b)
			n  <- ct$n
			size_a <- setNames(ct$marg_a$ai, as.character(ct$marg_a$a))
			size_b <- setNames(ct$marg_b$bj, as.character(ct$marg_b$b))

		#	Mutual Information over Occupied Cells
			cells <- ct$counts
			cells[, ai := size_a[as.character(a)]]
			cells[, bj := size_b[as.character(b)]]
			mi <- sum((cells$nij / n) *
					  log((as.numeric(cells$nij) * n) /
						  (as.numeric(cells$ai) * as.numeric(cells$bj))))

		#	Entropies
			p_a <- as.numeric(size_a) / n
			p_b <- as.numeric(size_b) / n
			h_a <- -sum(p_a * log(p_a))
			h_b <- -sum(p_b * log(p_b))

		#	Expected Mutual Information, Memoized on Distinct Marginal Pairs
			lgam  <- lgamma(seq.int(0L, n) + 1L)
			sizes <- CJ(ai = sort(unique(as.integer(size_a))),
						bj = sort(unique(as.integer(size_b))))
			mult_a <- table(as.integer(size_a))
			mult_b <- table(as.integer(size_b))
			sizes[, mult := as.numeric(mult_a[as.character(ai)]) *
							as.numeric(mult_b[as.character(bj)])]
			sizes[, term := mapply(pa_emi_cell, ai, bj,
								   MoreArgs = list(n = n, lgam = lgam))]
			emi <- sum(sizes$mult * sizes$term)

		#	Normalize
			h_bar <- mean(c(h_a, h_b))
			ami <- if (abs(h_bar - emi) < 1e-12) NA_real_ else (mi - emi) / (h_bar - emi)
			nmi <- if (h_bar < 1e-12) NA_real_ else mi / h_bar

		#	Assembling Result
			return(list(mi = mi, emi = emi, ami = ami, nmi = nmi,
						h_a = h_a, h_b = h_b))
	}

#	Directory of a Partition, Relative to the Scan Root
	pa_rel_dir <- function(path, root = NULL) {
		#	"""
		#	Args:
		#		path: path to a .clu file
		#		root: scan root, or NULL
		#	Returns:
		#		character directory of path relative to root
		#	Notes:
		#		The whole relative path is kept, not just the immediate parent. Two
		#		partitions of the same era written by different runs can share a parent
		#		directory name -- alpha1.00_g120_e14 under run1 and under run2 -- and
		#		recording only the parent made them indistinguishable in the output.
		#	"""

		#	Directory
			d <- dirname(path)

		#	Early return
			if (is.null(root) || !nzchar(root)) return(basename(d))

		#	Strip the Root
			r <- sub("/+$", "", root)
			if (!startsWith(d, r)) return(basename(d))
			out <- substring(d, nchar(r) + 2L)

		#	Assembling Result
			return(if (nzchar(out)) out else ".")
	}

#	Parse Settings out of a Partition Path
	pa_parse_clu_name <- function(path, root = NULL) {
		#	"""
		#	Args:
		#		path: path to a .clu file
		#		root: scan root, so settings can be read from the whole relative path
		#	Returns:
		#		list(era, alpha, anchor, anchor_era, gamma, seed, rel_dir, tag)
		#	Notes:
		#		Settings are read from the directory and the base name together, because
		#		the specification lives in the directory -- alpha1.00_g120_e14 -- while
		#		the file name repeats it.
		#
		#		Every token is anchored to a separator so it cannot match inside a word.
		#		g120 is the ANCHOR, the target citing articles per effective community,
		#		and NOT a resolution: an earlier version matched g<number> as gamma and
		#		reported anchors of 120, 90, 160 and 60 in the gamma column.
		#
		#		gamma is read from gam<number> or gamma<number>. It is NA for every file
		#		now on disk, because nothing written so far records the resolution a
		#		partition was detected at. A grid directory such as gamma_0.01_1.0_n40
		#		names a RANGE and deliberately does not match.
		#	"""

		#	Haystack: Relative Directory and Base Name, Underscore Delimited
			base <- sub("\\.clu$", "", basename(path))
			rel  <- pa_rel_dir(path, root)
			hay  <- gsub("[^A-Za-z0-9.]+", "_", paste(rel, base, sep = "_"))

		#	Token Reader
			token <- function(pattern) {
				m <- regmatches(hay, regexec(pattern, hay, perl = TRUE))[[1]]
				if (length(m) >= 2L) m[2] else NA_character_
			}

		#	Settings
			era        <- token("(?:^|_)era([0-9]+)(?:_|$)")
			alpha      <- token("(?:^|_)alpha([0-9]+(?:\\.[0-9]+)?)(?:_|$)")
			anchor     <- token("(?:^|_)g([0-9]+(?:\\.[0-9]+)?)(?:_|$)")
			anchor_era <- token("(?:^|_)e([0-9]+)(?:_|$)")
			gamma      <- token("(?:^|_)gam(?:ma)?([0-9]+(?:\\.[0-9]+)?)(?:_|$)")
			seed       <- token("(?:^|_)s([0-9]+)(?:_|$)")

		#	Assembling Result
			return(list(era        = as.integer(era),
						alpha      = as.numeric(alpha),
						anchor     = as.numeric(anchor),
						anchor_era = as.integer(anchor_era),
						gamma      = as.numeric(gamma),
						seed       = as.integer(seed),
						rel_dir    = rel,
						tag        = sub("^era[0-9]+_?", "", base)))
	}

#	Record One Check
	pa_check <- function(name, passed, detail = "") {
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

#' @title pa_partition_profile
#' @description Measure the granularity and size distribution of one partition of an era network.
#'
#' @param membership Integer community labels, one per vertex, in .net vertex order.
#' @param era_nodes Era node table from pa_load_era_nodes, in vertex order.
#' @param bridge_eligible Logical vector over the same rows from pa_bridging_eligible, or NULL to omit the bridging measures.
#' @param floors Integer size floors for the threshold counts. Default CITER_FLOORS.
#'
#' @details
#' Granularity is measured on the active citing population -- vertices with
#' within-era out-degree of at least one -- because a cited-only work with
#' in-degree one is assigned to its citer's community by arithmetic, and counting
#' it would make granularity depend on how many references each community's
#' members happened to make inside the window.
#'
#' The bridging measures cover the population the descent relation uses, which is
#' works cited in two consecutive eras rather than citing articles. The two are
#' reported side by side because the calibration controls the first while the
#' primary outcome depends on the second.
#'
#' Equal granularity is one way to improve comparability across eras, not a
#' sufficient one: two partitions with the same g can differ in shape. The
#' largest-community share, the size Gini, the singleton share and the threshold
#' counts are returned for that reason. The threshold counts matter especially,
#' because the median community holds one citing article in every partition
#' measured so far, so n_communities overstates the usable units by an order of
#' magnitude.
#'
#' @return A one-row data.table of granularity and distribution measures.
#'
#' @examples
#' nodes <- data.table::data.table(node_id = 1:100,
#'                                 out_degree = rep(1L, 100),
#'                                 in_degree = rep(0L, 100))
#' pa_partition_profile(c(rep(1L, 90), rep(2L, 10)), nodes)$granularity
#'
#' @export
pa_partition_profile <- function(membership, era_nodes, bridge_eligible = NULL,
								 floors = CITER_FLOORS) {
	#	"""
	#	Args:
	#		membership: integer labels, one per vertex, in .net vertex order
	#		era_nodes: era node table in vertex order
	#		bridge_eligible: logical over the same rows, or NULL
	#		floors: integer size floors for the threshold counts
	#	Returns:
	#		one-row data.table
	#	Notes:
	#		The worked example of the design document is the test: 100 citing articles
	#		split 90 and 10 give g = 82 and k_eff = 100/82, not a mean size of 50.
	#	"""

	#	Validation
		if (length(membership) != nrow(era_nodes)) {
			stop("pa_partition_profile: membership has ", length(membership),
				 " labels but the era holds ", nrow(era_nodes), " vertices")
		}
		if (!is.null(bridge_eligible) && length(bridge_eligible) != nrow(era_nodes)) {
			stop("pa_partition_profile: bridge_eligible length does not match")
		}

	#	Per-Community Counts
		dt <- data.table(comm   = as.integer(membership),
						 citer  = as.integer(era_nodes$out_degree > 0L),
						 cited  = as.integer(era_nodes$in_degree > 0L),
						 bridge = if (is.null(bridge_eligible)) {
									 0L
								 } else {
									 as.integer(bridge_eligible)
								 })
		per <- dt[, .(n_vertices = .N,
					  n_citers   = sum(citer),
					  n_cited    = sum(cited),
					  n_bridge   = sum(bridge)), by = comm]

	#	Effective Counts
		eff_v <- pa_effective(per$n_vertices)
		eff_c <- pa_effective(per$n_citers)
		eff_b <- if (is.null(bridge_eligible)) {
					 list(total = NA_real_, n_nonzero = NA_integer_,
						  k_eff = NA_real_, g = NA_real_)
				 } else {
					 pa_effective(per$n_bridge)
				 }

	#	Threshold Counts over Citing Articles
		thr <- vapply(floors, function(f) sum(per$n_citers >= f), integer(1))
		names(thr) <- sprintf("n_comm_citers_ge_%d", floors)

	#	Assembling Result
		out <- data.table(
			n_communities         = nrow(per),
			n_vertices            = eff_v$total,
			n_citers              = eff_c$total,
			n_cited               = sum(per$n_cited),
			n_bridge_eligible     = eff_b$total,
			n_comm_with_citers    = eff_c$n_nonzero,
			n_comm_with_bridge    = eff_b$n_nonzero,
			k_eff_citers          = eff_c$k_eff,
			granularity           = eff_c$g,
			k_eff_vertices        = eff_v$k_eff,
			effective_size_vertex = eff_v$g,
			k_eff_bridge          = eff_b$k_eff,
			granularity_bridge    = eff_b$g,
			largest_citers        = max(per$n_citers),
			largest_citer_share   = if (eff_c$total > 0) {
										max(per$n_citers) / eff_c$total
									} else {
										NA_real_
									},
			median_citers_nonzero = if (eff_c$n_nonzero > 0L) {
										median(per$n_citers[per$n_citers > 0L])
									} else {
										NA_real_
									},
			largest_vertices      = max(per$n_vertices),
			largest_vertex_share  = max(per$n_vertices) / eff_v$total,
			citer_size_gini       = pa_gini(per$n_citers),
			vertex_size_gini      = pa_gini(per$n_vertices),
			singleton_share       = mean(per$n_vertices == 1L))
		return(cbind(out, as.data.table(as.list(thr)))[])
}

#' @title pa_profile_clu_file
#' @description Measure one partition file, taking its era and settings from its path.
#'
#' @param clu_path Path to a .clu file.
#' @param era_nodes Era node table in vertex order, or NULL to load it.
#' @param bridge_eligible Logical vector over the era's vertices, or NULL.
#' @param n_next_era_citers Count of citing articles in era e+1, or NA.
#' @param root Scan root, so settings are read from the whole relative path.
#' @param era_dir Directory holding era<NN>_nodes.Rda. Default ERA_DIR.
#'
#' @details
#' Loading the node table per file is wasteful across a scan, so
#' pa_scan_partitions passes it in along with the bridging eligibility and the
#' following era's citer count.
#'
#' @return A one-row data.table carrying provenance and the profile.
#'
#' @export
pa_profile_clu_file <- function(clu_path, era_nodes = NULL, bridge_eligible = NULL,
								n_next_era_citers = NA_integer_, root = NULL,
								era_dir = ERA_DIR) {
	#	"""
	#	Args:
	#		clu_path: path to a .clu file
	#		era_nodes: era node table in vertex order, or NULL to load
	#		bridge_eligible: logical over the era's vertices, or NULL
	#		n_next_era_citers: citing articles in era e+1, or NA
	#		root: scan root, or NULL
	#		era_dir: directory holding era<NN>_nodes.Rda
	#	Returns:
	#		one-row data.table
	#	"""

	#	Validation
		meta <- pa_parse_clu_name(clu_path, root)
		if (is.na(meta$era)) {
			stop("pa_profile_clu_file: no era in path: ", clu_path)
		}

	#	Era Inputs
		if (is.null(era_nodes)) era_nodes <- pa_load_era_nodes(meta$era, era_dir)

	#	Measure
		lab  <- pa_read_clu(clu_path)
		prof <- pa_partition_profile(lab, era_nodes, bridge_eligible)

	#	Assembling Result
		return(cbind(data.table(era               = meta$era,
								alpha             = meta$alpha,
								anchor            = meta$anchor,
								anchor_era        = meta$anchor_era,
								gamma             = meta$gamma,
								seed              = meta$seed,
								n_citers_next_era = as.integer(n_next_era_citers),
								rel_dir           = meta$rel_dir,
								tag               = meta$tag,
								clu_file          = basename(clu_path),
								clu_path          = clu_path),
					 prof))
}

#' @title pa_scan_partitions
#' @description Measure every partition under a directory tree, one row per .clu file.
#'
#' @param root Directory to scan recursively for .clu files. Default SWEEP_ROOT.
#' @param eras Integer eras to include. Default ANALYTIC_ERAS.
#' @param era_dir Directory holding era<NN>_nodes.Rda. Default ERA_DIR.
#' @param edges Full dated edge list, or NULL to skip the bridging measures.
#'
#' @details
#' Era node tables, bridging eligibility and the following era's citer count are
#' computed once per era and reused across every partition of that era, which is
#' what makes a scan of a hundred files cheap. A file whose era falls outside eras
#' is skipped rather than erroring, so the scan can be pointed at a directory
#' holding more than the analytic range. A file that cannot be measured warns and
#' is omitted, so one malformed partition does not halt the scan.
#'
#' @return A data.table, one row per partition file.
#'
#' @export
pa_scan_partitions <- function(root = SWEEP_ROOT, eras = ANALYTIC_ERAS,
							   era_dir = ERA_DIR, edges = NULL) {
	#	"""
	#	Args:
	#		root: directory to scan recursively
	#		eras: integer eras to include
	#		era_dir: directory holding era<NN>_nodes.Rda
	#		edges: full dated edge list, or NULL
	#	Returns:
	#		data.table, one row per .clu file
	#	Notes:
	#		The caches are keyed by era as a character string and tested with
	#		%in% names(), not by is.null() on [[, so a missing key cannot raise a
	#		subscript error.
	#	"""

	#	Validation
		if (!dir.exists(root)) stop("pa_scan_partitions: no such directory: ", root)

	#	Find the Partitions
		files <- list.files(root, pattern = "\\.clu$", recursive = TRUE,
							full.names = TRUE)
		if (length(files) == 0L) {
			warning("pa_scan_partitions: no .clu files under ", root)
			return(data.table())
		}

	#	Iterating Over Partitions
		rows <- vector("list", length(files))
		cache_nodes  <- list()
		cache_bridge <- list()
		cache_next   <- list()
		for (i in seq_along(files)) {
			meta <- pa_parse_clu_name(files[i], root)
			if (is.na(meta$era) || !(meta$era %in% eras)) next
			key <- as.character(meta$era)

			#	Node table
				if (!(key %in% names(cache_nodes))) {
					cache_nodes[[key]] <- pa_load_era_nodes(meta$era, era_dir)
				}
				era_nodes <- cache_nodes[[key]]

			#	Bridging eligibility and the following era's citers
				if (!is.null(edges) && !(key %in% names(cache_bridge))) {
					cache_bridge[[key]] <- pa_bridging_eligible(meta$era, edges,
																era_nodes$node_id)
					cache_next[[key]]   <- pa_next_era_citers(meta$era, edges)
				}
				be <- if (is.null(edges)) NULL else cache_bridge[[key]]
				nc <- if (is.null(edges)) NA_integer_ else cache_next[[key]]

			#	Measure
				rows[[i]] <- tryCatch(
					pa_profile_clu_file(files[i], era_nodes = era_nodes,
										bridge_eligible = be,
										n_next_era_citers = nc,
										root = root, era_dir = era_dir),
					error = function(e) {
						warning("pa_scan_partitions: ", basename(files[i]), ": ",
								conditionMessage(e))
						NULL
					})
		}

	#	Assembling Result
		out <- rbindlist(Filter(Negate(is.null), rows), use.names = TRUE, fill = TRUE)
		if (nrow(out) > 0L) setorder(out, era, rel_dir, clu_file)
		return(out[])
}

#' @title pa_partition_agreement
#' @description Compare partitions of the same era and specification, using the adjusted Rand index and adjusted mutual information.
#'
#' @param profiles Output of pa_scan_partitions.
#' @param by Columns identifying a group within which partitions should agree. Default era, alpha and anchor.
#' @param citers_only Restrict the comparison to citing articles. Default TRUE.
#' @param era_dir Directory holding era<NN>_nodes.Rda. Default ERA_DIR.
#'
#' @details
#' This answers validation requirement three of the design document. Two runs can
#' both hit a target effective count while assigning hundreds of citing articles
#' differently, and a calibration matching on a scalar would accept either. If
#' agreement within a group is poor, the partitions are not a stable foundation
#' under any scaling rule.
#'
#' Grouping is by SPECIFICATION rather than by directory -- era, alpha and anchor
#' -- because two partitions built to the same specification should agree whatever
#' run directory they were written into. Grouping on rel_dir would separate exactly
#' the replicates the requirement is about.
#'
#' Adjusted mutual information is reported alongside the adjusted Rand index
#' because the partitions compared may differ in community count, which AMI
#' handles better. k_eff_rel_diff is the relative difference in citer-weighted
#' k_eff between the pair, which is the measured floor on any calibration
#' tolerance: a tolerance below the run-to-run spread cannot distinguish a hit
#' from Leiden's own variation.
#'
#' Restricting to citing articles is the default because they are the population
#' the construction calibrates and the models analyse; cited-only vertices follow
#' their citer and would inflate agreement. Groups holding one partition produce
#' no rows.
#'
#' @return A data.table, one row per pair of partitions within a group.
#'
#' @export
pa_partition_agreement <- function(profiles, by = c("era", "alpha", "anchor"),
								   citers_only = TRUE, era_dir = ERA_DIR) {
	#	"""
	#	Args:
	#		profiles: output of pa_scan_partitions
	#		by: columns identifying a comparison group
	#		citers_only: restrict to vertices with within-era out-degree
	#		era_dir: directory holding era<NN>_nodes.Rda
	#	Returns:
	#		data.table, one row per within-group pair
	#	Notes:
	#		The grouping key is named grp_key rather than .grp, because dot-prefixed
	#		names are reserved by data.table for its own symbols.
	#	"""

	#	Early return
		if (nrow(profiles) == 0L) return(data.table())

	#	Validation
		miss <- setdiff(by, names(profiles))
		if (length(miss) > 0L) {
			stop("pa_partition_agreement: profiles lacks column(s): ",
				 paste(miss, collapse = ", "))
		}

	#	Group the Partitions
		p <- as.data.table(profiles)
		p[, grp_key := do.call(paste, c(.SD, sep = "|")), .SDcols = by]
		rows <- list()
		cache_nodes <- list()

	#	Iterating Over Groups
		for (g in unique(p$grp_key)) {
			sub <- p[grp_key == g]
			if (nrow(sub) < 2L) next

			#	Vertices to compare
				key <- as.character(sub$era[1])
				if (!(key %in% names(cache_nodes))) {
					cache_nodes[[key]] <- pa_load_era_nodes(sub$era[1], era_dir)
				}
				keep <- if (citers_only) {
							cache_nodes[[key]]$out_degree > 0L
						} else {
							rep(TRUE, nrow(cache_nodes[[key]]))
						}

			#	Every pair in the group
				for (i in 1:(nrow(sub) - 1L)) {
					for (j in (i + 1L):nrow(sub)) {
						a <- pa_read_clu(sub$clu_path[i])[keep]
						b <- pa_read_clu(sub$clu_path[j])[keep]
						agr <- pa_ami(a, b)
						ka  <- sub$k_eff_citers[i]
						kb  <- sub$k_eff_citers[j]
						rows[[length(rows) + 1L]] <- data.table(
							era            = sub$era[i],
							alpha          = sub$alpha[i],
							anchor         = sub$anchor[i],
							dir_a          = sub$rel_dir[i],
							dir_b          = sub$rel_dir[j],
							file_a         = sub$clu_file[i],
							file_b         = sub$clu_file[j],
							citers_only    = citers_only,
							n_compared     = length(a),
							k_a            = length(unique(a)),
							k_b            = length(unique(b)),
							k_eff_a        = ka,
							k_eff_b        = kb,
							k_eff_rel_diff = abs(ka - kb) / mean(c(ka, kb)),
							ari            = pa_ari(a, b),
							ami            = agr$ami,
							nmi            = agr$nmi,
							mi             = agr$mi,
							emi            = agr$emi)
					}
				}
		}

	#	Assembling Result
		out <- rbindlist(rows, use.names = TRUE, fill = TRUE)
		if (nrow(out) > 0L) setorder(out, era, alpha, anchor, dir_a, dir_b)
		return(out[])
}

#' @title pa_run_granularity
#' @description Measure every partition under a directory, compare partitions built to the same specification, write the tables, and run the checks.
#'
#' @param root Directory holding the partitions. Default SWEEP_ROOT.
#' @param eras Integer eras to include. Default ANALYTIC_ERAS.
#' @param era_dir Directory holding era<NN>_nodes.Rda. Default ERA_DIR.
#' @param output_dir Destination for the CSV outputs. Default GRAN_DIR.
#' @param with_bridging Compute the bridging-population measures. Default TRUE.
#' @param with_agreement Compute pairwise partition agreement. Default TRUE.
#'
#' @details
#' The bridging measures need the full dated edge list, because the era e+1
#' citations that make a work eligible to bridge are cross-era arcs and do not
#' appear in era e's network. If that file is absent the measures are skipped with
#' a warning rather than the run failing.
#'
#' @return Invisibly, list(profiles, agreement, tests).
#'
#' @examples
#' res <- pa_run_granularity()
#' res$profiles[, .(era, alpha, anchor, k_eff_citers, granularity, k_eff_bridge)]
#'
#' @export
pa_run_granularity <- function(root = SWEEP_ROOT, eras = ANALYTIC_ERAS,
							   era_dir = ERA_DIR, output_dir = GRAN_DIR,
							   with_bridging = TRUE, with_agreement = TRUE) {
	#	"""
	#	Args:
	#		root: directory holding the partitions
	#		eras: integer eras to include
	#		era_dir: directory holding era<NN>_nodes.Rda
	#		output_dir: destination for the CSVs
	#		with_bridging: compute the bridging-population measures
	#		with_agreement: compute pairwise agreement
	#	Returns:
	#		invisible list(profiles, agreement, tests)
	#	"""

	#	Output Directory
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

	#	Edge List for the Bridging Measures
		edges <- NULL
		if (with_bridging) {
			f <- file.path(era_dir, "pa_edge_list.Rda")
			if (!file.exists(f)) {
				warning("pa_run_granularity: ", f,
						" not found; skipping the bridging measures")
			} else {
				env <- new.env(parent = emptyenv())
				load(f, envir = env)
				edges <- as.data.table(get("pa_edges", envir = env, inherits = FALSE))
			}
		}

	#	Measure Every Partition
		message("Scanning partitions under ", root)
		profiles <- pa_scan_partitions(root, eras, era_dir, edges)
		message("  measured ", nrow(profiles), " partition(s)")

	#	Compare Partitions Built to the Same Specification
		agreement <- data.table()
		if (with_agreement && nrow(profiles) > 0L) {
			message("Comparing partitions within era, alpha and anchor")
			agreement <- pa_partition_agreement(profiles, era_dir = era_dir)
			message("  compared ", nrow(agreement), " pair(s)")
		}

	#	Write
		if (nrow(profiles) > 0L) {
			readr::write_csv(profiles[, !"clu_path"],
							 file.path(output_dir, "partition_granularity.csv"))
		}
		if (nrow(agreement) > 0L) {
			readr::write_csv(agreement,
							 file.path(output_dir, "partition_agreement.csv"))
		}

	#	Checks
		tests <- pa_run_granularity_tests(profiles, agreement, output_dir)

	#	Assembling Result
		return(invisible(list(profiles = profiles, agreement = agreement,
							  tests = tests)))
}

#############
#   TESTS   #
#############

#' @title pa_run_granularity_tests
#' @description Check a completed granularity measurement, print the results, and write them to CSV.
#'
#' @param profiles Output of pa_scan_partitions.
#' @param agreement Output of pa_partition_agreement. Default an empty table.
#' @param output_dir Destination for the test table, or NULL to skip writing. Default GRAN_DIR.
#'
#' @details
#' The identities are the point. The worked example of the design document must
#' reproduce exactly; k_eff must lie between one and the number of communities
#' carrying the population; g times k_eff must recover the population total; and a
#' partition compared with itself must score one on both agreement indices. A
#' failure in any of those is an arithmetic error rather than a finding about the
#' networks.
#'
#' Two checks are reports rather than pass or fail conditions. The run-to-run
#' spread in k_eff states the floor on any calibration tolerance. The bridging
#' truncation check flags an era whose following era is too small to support its
#' bridging population, which is the case for era 17 against the two-year era 18.
#'
#' @return A data.frame of checks.
#'
#' @export
pa_run_granularity_tests <- function(profiles, agreement = data.table(),
									 output_dir = GRAN_DIR) {
	#	"""
	#	Args:
	#		profiles: output of pa_scan_partitions
	#		agreement: output of pa_partition_agreement
	#		output_dir: destination for the test table, or NULL
	#	Returns:
	#		data.frame of checks
	#	"""

	#	Collect
		out <- list()

	#	The Worked Example: 100 citing articles split 90 and 10
		lab <- c(rep(1L, 90), rep(2L, 10))
		fake <- data.table(node_id = 1:100, out_degree = rep(1L, 100),
						   in_degree = rep(0L, 100))
		prof <- pa_partition_profile(lab, fake)
		out[[length(out) + 1L]] <- pa_check(
			"worked example: g = 82",
			isTRUE(all.equal(prof$granularity, 82)),
			sprintf("%.4f", prof$granularity))
		out[[length(out) + 1L]] <- pa_check(
			"worked example: k_eff = 100/82",
			isTRUE(all.equal(prof$k_eff_citers, 100 / 82)),
			sprintf("%.6f", prof$k_eff_citers))
		out[[length(out) + 1L]] <- pa_check(
			"worked example: mean size would have been 50",
			isTRUE(all.equal(100 / prof$n_communities, 50)),
			sprintf("%.1f", 100 / prof$n_communities))

	#	Gini of a Uniform Partition
		out[[length(out) + 1L]] <- pa_check(
			"gini of equal sizes is 0",
			isTRUE(all.equal(pa_gini(rep(7, 11)), 0)),
			sprintf("%.2e", pa_gini(rep(7, 11))))

	#	Agreement of a Partition with Itself
		self_ari <- pa_ari(lab, lab)
		self_ami <- pa_ami(lab, lab)$ami
		out[[length(out) + 1L]] <- pa_check(
			"ARI of a partition with itself is 1",
			isTRUE(all.equal(self_ari, 1)), sprintf("%.6f", self_ari))
		out[[length(out) + 1L]] <- pa_check(
			"AMI of a partition with itself is 1",
			isTRUE(all.equal(self_ami, 1, tolerance = 1e-6)),
			sprintf("%.6f", self_ami))

	#	Path Parsing
		demo <- "/root/alpha1.00_g120_e14/era11_alpha1.00_g120_e14.clu"
		pm <- pa_parse_clu_name(demo, "/root")
		out[[length(out) + 1L]] <- pa_check(
			"path parsing separates anchor from gamma",
			identical(pm$era, 11L) && isTRUE(all.equal(pm$alpha, 1.00)) &&
				isTRUE(all.equal(pm$anchor, 120)) && identical(pm$anchor_era, 14L) &&
				is.na(pm$gamma),
			sprintf("era %s alpha %s anchor %s anchor_era %s gamma %s",
					pm$era, pm$alpha, pm$anchor, pm$anchor_era, pm$gamma))
		gm <- pa_parse_clu_name("/root/gamma_0.01_1.0_n40/era11_local_gamma.clu", "/root")
		out[[length(out) + 1L]] <- pa_check(
			"a grid directory does not parse as an anchor or a gamma",
			identical(gm$era, 11L) && is.na(gm$anchor) && is.na(gm$gamma),
			sprintf("anchor %s gamma %s rel_dir %s", gm$anchor, gm$gamma, gm$rel_dir))

	#	Measured Partitions
		if (nrow(profiles) > 0L) {
			out[[length(out) + 1L]] <- pa_check(
				"every partition measured",
				all(is.finite(profiles$k_eff_citers)),
				sprintf("%d of %d finite", sum(is.finite(profiles$k_eff_citers)),
						nrow(profiles)))
			out[[length(out) + 1L]] <- pa_check(
				"k_eff <= communities carrying citers",
				all(profiles$k_eff_citers <= profiles$n_comm_with_citers + 1e-9),
				sprintf("max excess %.2e",
						max(profiles$k_eff_citers - profiles$n_comm_with_citers)))
			out[[length(out) + 1L]] <- pa_check(
				"k_eff >= 1",
				all(profiles$k_eff_citers >= 1 - 1e-9),
				sprintf("min %.4f", min(profiles$k_eff_citers)))
			out[[length(out) + 1L]] <- pa_check(
				"g * k_eff recovers the citer total",
				all(abs(profiles$granularity * profiles$k_eff_citers -
						profiles$n_citers) < 1e-6),
				sprintf("max residual %.2e",
						max(abs(profiles$granularity * profiles$k_eff_citers -
								profiles$n_citers))))
			out[[length(out) + 1L]] <- pa_check(
				"bridging population does not exceed the cited population",
				all(is.na(profiles$n_bridge_eligible) |
					profiles$n_bridge_eligible <= profiles$n_cited),
				sprintf("max bridge/cited %.3f",
						suppressWarnings(max(profiles$n_bridge_eligible /
											 profiles$n_cited, na.rm = TRUE))))
			out[[length(out) + 1L]] <- pa_check(
				"rel_dir distinguishes same-named files",
				!any(duplicated(profiles[, .(era, rel_dir, clu_file)])),
				sprintf("%d duplicate provenance key(s)",
						sum(duplicated(profiles[, .(era, rel_dir, clu_file)]))))

			#	Threshold counts, only if the default floors were used
				thr_cols <- sprintf("n_comm_citers_ge_%d", CITER_FLOORS)
				if (all(thr_cols %in% names(profiles)) && length(thr_cols) > 1L) {
					mono <- rep(TRUE, nrow(profiles))
					for (k in seq_len(length(thr_cols) - 1L)) {
						mono <- mono & (profiles[[thr_cols[k]]] >=
										profiles[[thr_cols[k + 1L]]])
					}
					out[[length(out) + 1L]] <- pa_check(
						"threshold counts fall as the floor rises",
						all(mono),
						sprintf("%d of %d rows monotone", sum(mono), nrow(profiles)))
				}

			#	Bridging truncation, over the pairs the design uses
			#	Era e is checked only when e+1 is also analytic, because the descent
			#	pair (e, e+1) is what needs era e+1 to be large enough. Era 17's
			#	following era is era 18, which spans two years and sits outside the
			#	analytic range, so the pair (17, 18) is not used and era 17's thin
			#	bridging population is not a defect. A check that fails on every run
			#	trains the reader to ignore the table, so the truncation is reported
			#	through the n_citers_next_era column instead.
				if ("n_citers_next_era" %in% names(profiles) &&
					any(!is.na(profiles$n_citers_next_era))) {
					nxt <- unique(profiles[!is.na(n_citers_next_era) &
										   (era + 1L) %in% ANALYTIC_ERAS,
										   .(era, n_citers_next_era, n_citers)])
					thin <- nxt[n_citers_next_era < 0.5 * n_citers]
					out[[length(out) + 1L]] <- pa_check(
						"following era supports bridging, over the analytic pairs",
						nrow(thin) == 0L,
						if (nrow(nxt) == 0L) {
							"no analytic pair to check"
						} else if (nrow(thin) == 0L) {
							sprintf("%d pair(s) checked, all adequate", nrow(nxt))
						} else {
							paste0("truncated: era(s) ",
								   paste(thin$era, collapse = ", "))
						})
				}
		}

	#	Agreement Table
		if (nrow(agreement) > 0L) {
			out[[length(out) + 1L]] <- pa_check(
				"ARI within [-1, 1]",
				all(agreement$ari >= -1 - 1e-9 & agreement$ari <= 1 + 1e-9),
				sprintf("range %.4f to %.4f", min(agreement$ari), max(agreement$ari)))
			out[[length(out) + 1L]] <- pa_check(
				"AMI at most 1",
				all(is.na(agreement$ami) | agreement$ami <= 1 + 1e-6),
				sprintf("max %.6f",
						suppressWarnings(max(agreement$ami, na.rm = TRUE))))
			#	Replicates only
			#	Rows whose alpha is NA are the sweep's own elbow partitions, grouped
			#	by era alone because they carry no specification. They sit at very
			#	different resolutions and are NOT replicates, so the stability
			#	threshold and the tolerance floor are computed without them.
				rep_rows <- agreement[!is.na(alpha)]
				out[[length(out) + 1L]] <- pa_check(
					"AMI at or above 0.60 among same-specification replicates",
					nrow(rep_rows) == 0L ||
						all(is.na(rep_rows$ami) | rep_rows$ami >= 0.60),
					if (nrow(rep_rows) == 0L) {
						"no replicate pairs to compare"
					} else {
						sprintf("min %.4f over %d pair(s)",
								suppressWarnings(min(rep_rows$ami, na.rm = TRUE)),
								nrow(rep_rows))
					})

			#	Run-to-run spread, a report
				if ("k_eff_rel_diff" %in% names(agreement) && nrow(rep_rows) > 0L) {
					worst <- suppressWarnings(max(rep_rows$k_eff_rel_diff,
												  na.rm = TRUE))
					out[[length(out) + 1L]] <- pa_check(
						"calibration tolerance floor from run-to-run spread",
						is.finite(worst),
						sprintf("max spread %.1f%% (era %s)", 100 * worst,
								rep_rows$era[which.max(rep_rows$k_eff_rel_diff)]))
				}
		}

	#	Assembling Result
		tests <- do.call(rbind, out)
		print(tests, row.names = FALSE)
		message("\n", sum(tests$result == "PASS"), " of ", nrow(tests),
				" checks passed")
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests, file.path(output_dir, "granularity_tests.csv"))
		}
		return(tests)
}

#############################
#   HOW TO RUN THIS STAGE   #
#############################

#	NO MAIN BLOCK, DELIBERATELY. PathA_Pipeline.R sources this file to load its
#	functions, and an auto-run here would measure every partition on disk before a
#	single stage had been declared. The working directory is likewise not set here:
#	setwd() inside a sourced library mutates global state, and the pipeline owns it.
#
#	Run as a pipeline stage:
#
#		source("scripts/Path_A/PathA_Pipeline.R")
#		res <- pa_pipeline(stages = "granularity")
#
#	Or directly:
#
#		res <- pa_run_granularity()
#		res$profiles[, .(era, alpha, anchor, k_eff_citers, granularity,
#		                 k_eff_bridge, granularity_bridge)]
#
#	HOW TO READ THE OUTPUT.
#
#		CALIBRATION    granularity near the anchor means the target was hit. This
#		               and granularity_bridge are the primary measures.
#		DESCENT        granularity_bridge covers the population descent uses.
#		USABLE UNITS   n_comm_citers_ge_4, never n_communities. Unequal counts
#		               across eras are expected: the field grew.
#		SHAPE          largest_citer_share and citer_size_gini are secondary.
#		TOLERANCE      k_eff_rel_diff is the floor on any calibration tolerance.
