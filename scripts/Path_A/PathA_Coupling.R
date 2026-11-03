#	PathA_Coupling.R -- Bibliographic Coupling Between Communities, and Its Exposure
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	3 November 2026

#	WHAT THIS BUILDS. Model I's outcome and its exposure (spec sec. 9, sec. 13.2). One
#	observation is an ordered community pair: community i in era t paired with
#	community j in era t-1. The outcome is n_coupling, the number of works i and j
#	cite in common. The exposure is stored as its three components rather than as a
#	single product, so the degree correction can be recomputed without rebuilding the
#	frame.

#	WHY CROSS-ERA. Coupling here is not within-era. Model II carries n_coupling as a
#	covariate on the pairing of a citing community in era t with a cited community in
#	era t-1, so Model I's outcome has to be measured on that same pairing or the two
#	models are not about the same object. Set intersection is symmetric, but the pair
#	is ordered because i and j come from different eras, so no pair is counted twice.

#	THE THREE DECISIONS, AND WHAT WAS CHOSEN. The spec states the quantities and not
#	the populations they are counted over. Each choice below is a configuration
#	constant, and the alternatives are measured alongside the adopted one so that
#	changing a decision does not require a rerun to find out what it costs.
#
#		1.	WHICH ROWS EXIST. A community with no references had no opportunity to
#			overlap with anything, so it carries no information about a connection
#			failing to form and is excluded (CP_MIN_REFS). Every pair among the
#			remaining communities exists, including the zeros. This is the same
#			exclusion the risk set makes on the citation side, for the same reason.
#
#		2.	THE DEGREE-CORRECTION POOL. kappa_p describes the skew of the citation
#			distribution over the works the two eras could have shared, so d_w counts
#			citations emitted by community-mapped articles of eras t-1 and t
#			(CP_KAPPA_POOL = "pair_mapped"). Two alternatives are computed and stored
#			on every pair: "pair_all" ignores whether the citing article mapped to a
#			community, and "corpus" uses the whole edge list. They differ by how much
#			of the long tail they admit, and the stored columns make that visible.
#
#		3.	R_i's WINDOW. Distinct works cited by i's articles within era t. Because
#			the pool in (2) is the union of both eras' references, every community's
#			reference set lies inside it by construction, so R_i needs no further
#			restriction and R_i, R_j and kappa_p are all on one population.
#
#	WHAT IS NOT WRITTEN. The full community-pair grid is 1.87 million rows across the
#	six analytic pairs, and five specifications would put nine million rows of mostly
#	zeros on disk. The zeros are reconstructible: a pair's grid is the cross product
#	of its two exposure tables. So this stage writes the nonzero coupling cells and
#	the two exposure tables, and pa_load_coupling_grid() expands a pair on demand.
#	Writing the product instead of the ingredients is the mistake the risk set made.
#
#	A NOTE ON ONE CHECK THAT DOES NOT HOLD. It is tempting to require that a descent
#	tie's bridging-work count not exceed the same pair's coupling count. It does not
#	follow. A bridging work h belongs to community j's partition and is cited by some
#	era t-1 article, but that citer need not be a member of j, so h need not be in
#	j's reference set. What does follow is that h is cited by community i, so the
#	bridging count is bounded by R_i, and that is the form the test takes.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, data.table and Matrix for
#	the heavy computation, base R for everything else.

################
#   PACKAGES   #
################

#	Libraries
	library(data.table)
	library(Matrix)

#####################
#   CONFIGURATION   #
#####################

#	Input Paths
	CP_ERA_DIR <- "/workspace/caffeine_citation/data/eras_7yr"

#	Output Path
	CP_OUT_DIR <- "/workspace/caffeine_citation/data/coupling"

#	Minimum References for a Community to Enter the Frame
#	Decision 1. A community whose articles cite nothing had no opportunity to share a
#	reference, so it is excluded rather than carried as a structural zero.
	CP_MIN_REFS <- 1L

#	The Pool kappa_p Is Computed Over
#	Decision 2. One of "pair_mapped", "pair_all" or "corpus". All three are measured
#	and stored on every pair; this constant says which one enters E^K.
	CP_KAPPA_POOL <- "pair_mapped"

#	Writing the Expanded Grid
#	FALSE, and it should stay FALSE. See the header: the grid is the cross product of
#	the two exposure tables and pa_load_coupling_grid() rebuilds it.
	CP_WRITE_GRID <- FALSE

#	Reporting Threshold for a Large Pair
#	A message, not a limit, so a grid an order of magnitude larger than expected says
#	so before it is built.
	CP_GRID_WARN_ROWS <- 2e6

#########################
#   INTERNAL HELPSERS   #
#########################

#	One Check Row
	pa_check_cp <- function(name, passed, detail = "") {
		#	"""
		#	Args:
		#		name: the check's name
		#		passed: logical, or NA when the check could not run
		#		detail: measured value behind the verdict
		#	Returns:
		#		data.frame(check, result, detail)
		#	"""

		#	Assembling Result
			return(data.frame(
				check  = name,
				result = if (is.na(passed)) "SKIP" else if (passed) "PASS" else "FAIL",
				detail = as.character(detail),
				stringsAsFactors = FALSE))
	}

#	Citation Counts Over a Set of Edges
	pa_cite_counts <- function(edges) {
		#	"""
		#	Args:
		#		edges: edge list carrying target_id
		#	Returns:
		#		numeric(2): sum of d_w and sum of d_w squared
		#	Notes:
		#		d_w is an arc count, not a distinct-citer count, which is what the
		#		spec's kappa_p is defined on. Returned as a pair of sums rather than
		#		a vector of degrees because only the two sums are ever used and the
		#		degree vector is large.
		#	"""

		#	Early return
			if (nrow(edges) == 0L) {
				return(c(0, 0))
			}

		#	Degrees
			dt <- as.data.table(edges)[, .(d = .N), by = target_id]

		#	Assembling Result
			return(c(sum(as.numeric(dt$d)), sum(as.numeric(dt$d)^2)))
	}

#	Arcs Emitted by Community-Mapped Articles of One Era
	refs_to_arcs <- function(edges_dt, era_map, era) {
		#	"""
		#	Args:
		#		edges_dt: edge list as a data.table, carrying sender_era
		#		era_map: era map for the era
		#		era: integer era
		#	Returns:
		#		integer vector of target ids, one element per arc
		#	Notes:
		#		Arcs, not distinct works, because kappa_p is defined on citation
		#		counts. This is deliberately not pa_community_refs, which
		#		de-duplicates within a community.
		#	"""

		#	Era Edges
			era_edges <- edges_dt[!is.na(sender_era) &
								  sender_era == as.integer(era),
								  .(sender_id = as.integer(sender_id),
									target_id = as.integer(target_id))]

		#	Early return
			if (nrow(era_edges) == 0L) {
				return(integer(0))
			}

		#	Keep Only Mapped Senders
			sender_map <- as.data.table(era_map)[,
							.(sender_id = as.integer(node_id))]
			sender_map <- unique(sender_map, by = "sender_id")
			era_edges <- era_edges[sender_map, on = "sender_id", nomatch = 0L]

		#	Assembling Result
			return(era_edges$target_id)
	}

#	Kappa From Two Sums
	pa_kappa_from_sums <- function(sums) {
		#	"""
		#	Args:
		#		sums: numeric(2) from pa_cite_counts
		#	Returns:
		#		numeric: kappa, or NA when the pool is empty
		#	Notes:
		#		If every work were cited exactly once kappa is 1/W for W works, which
		#		is the uniform case the exposure reduces to.
		#	"""

		#	Early return
			if (sums[1] <= 0) {
				return(NA_real_)
			}

		#	Assembling Result
			return(sums[2] / (sums[1]^2))
	}

#' @title pa_community_refs
#' @description The distinct works each community of one era cites.
#'
#' @param edges_dated Edge list carrying sender_era.
#' @param era_map Era map for the era, from pa_era_map.
#' @param era Integer era.
#'
#' @details
#' A sender enters only if it maps to a community in that era's partition. Era nodes
#' absent from the era network carry no community and are dropped, which is the same
#' attribution the tie layer makes -- era 17 has 25,739 such nodes -- so the two
#' stages agree on which citations belong to whom.
#'
#' @return A data.table with community and work_id, one row per distinct pairing.
#'
#' @examples
#' refs <- pa_community_refs(edges_dated, maps[["17"]], 17L)
#'
#' @export
pa_community_refs <- function(edges_dated, era_map, era) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		era_map: era map for the era
	#		era: integer era
	#	Returns:
	#		data.table(community, work_id)
	#	"""

	#	Validation
		need <- c("sender_id", "target_id", "sender_era")
		missing <- setdiff(need, names(edges_dated))
		if (length(missing) > 0L) {
			stop("pa_community_refs: edge list is missing: ",
				 paste(missing, collapse = ", "))
		}
		if (!all(c("node_id", "community") %in% names(era_map))) {
			stop("pa_community_refs: era map needs node_id and community")
		}

	#	Era Edges
		era_edges <- as.data.table(edges_dated)
		era_edges <- era_edges[!is.na(sender_era) & sender_era == as.integer(era),
							   .(sender_id = as.integer(sender_id),
								 target_id = as.integer(target_id))]

	#	Early return
		if (nrow(era_edges) == 0L) {
			return(data.table(community = integer(0), work_id = integer(0)))
		}

	#	Sender to Community, One Row per Node
		sender_map <- as.data.table(era_map)[, .(sender_id = as.integer(node_id),
												 community = as.integer(community))]
		sender_map <- unique(sender_map, by = "sender_id")

	#	Attach and Drop the Unmapped
		era_edges[sender_map, on = "sender_id", community := i.community]
		era_edges <- era_edges[!is.na(community)]

	#	Assembling Result
		refs <- unique(era_edges[, .(community, work_id = target_id)])
		setorder(refs, community, work_id)
		return(refs)
}

#' @title pa_ref_incidence
#' @description A sparse community-by-work incidence matrix over a shared vocabulary.
#'
#' @param refs A data.table with community and work_id, from pa_community_refs.
#' @param communities Integer vector of communities in scope, in the row order wanted.
#' @param vocabulary Integer vector of work ids in scope, in the column order wanted.
#'
#' @details
#' Both sides of a pair must be built over the same vocabulary or the matrix product
#' is not defined, which is why the vocabulary is an argument rather than derived
#' here. Entries are binary: a community either cites a work or does not, and a
#' community citing a work twice does not make the work more shared.
#'
#' @return A sparse matrix with one row per community and one column per work.
#'
#' @examples
#' refs <- data.table::data.table(community = c(1L, 1L, 2L),
#'                                work_id   = c(11L, 12L, 12L))
#' pa_ref_incidence(refs, communities = 1:2, vocabulary = 11:12)
#'
#' @export
pa_ref_incidence <- function(refs, communities, vocabulary) {
	#	"""
	#	Args:
	#		refs: data.table(community, work_id)
	#		communities: integer vector, the row space
	#		vocabulary: integer vector, the column space
	#	Returns:
	#		sparseMatrix, length(communities) x length(vocabulary), binary
	#	"""

	#	Index Maps
		row_index <- match(refs$community, communities)
		col_index <- match(refs$work_id, vocabulary)

	#	Validation
		if (anyNA(row_index) || anyNA(col_index)) {
			stop("pa_ref_incidence: refs contain a community or work outside the ",
				 "declared row or column space")
		}

	#	Assembling Result
		return(sparseMatrix(i = row_index, j = col_index, x = 1,
							dims = c(length(communities), length(vocabulary))))
}

#' @title pa_coupling_pair
#' @description Build one era pair's coupling counts and exposure components.
#'
#' @param edges_dated Edge list carrying sender_era.
#' @param era_map_prior Era map for era t-1.
#' @param era_map_t Era map for era t.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#' @param min_refs Minimum references for a community to enter. Default CP_MIN_REFS.
#' @param kappa_pool Which pool enters E^K. Default CP_KAPPA_POOL.
#' @param warn_rows Grid size above which the size is reported. Default CP_GRID_WARN_ROWS.
#'
#' @details
#' The whole computation is one sparse matrix product. Build a binary incidence
#' matrix for each era over the union of their referenced works, and
#' \code{B_t \%*\% t(B_prior)} gives every pair's shared-work count at once. Doing it
#' as a product rather than pair by pair is not an optimization but the only tractable
#' form: the (16,17) pair has 839,800 community pairs, and a loop over them would
#' intersect two reference sets 839,800 times.
#'
#' Only the nonzero cells are returned. The zeros are the cross product of the two
#' exposure tables minus these cells, which pa_load_coupling_grid() reconstructs.
#'
#' @return A list with coupling, exposure_t, exposure_prior and summary.
#'
#' @examples
#' res <- pa_coupling_pair(edges_dated, maps[["16"]], maps[["17"]], 16L, 17L)
#'
#' @export
pa_coupling_pair <- function(edges_dated, era_map_prior, era_map_t,
							 era_prior, era_t, min_refs = CP_MIN_REFS,
							 kappa_pool = CP_KAPPA_POOL,
							 warn_rows = CP_GRID_WARN_ROWS) {
	#	"""
	#	Args:
	#		edges_dated: edge list carrying sender_era
	#		era_map_prior: era map for era t-1
	#		era_map_t: era map for era t
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#		min_refs: minimum references to enter the frame
	#		kappa_pool: "pair_mapped", "pair_all" or "corpus"
	#		warn_rows: grid size above which to report
	#	Returns:
	#		list(coupling, exposure_t, exposure_prior, summary)
	#	"""

	#	Validation
		pools <- c("pair_mapped", "pair_all", "corpus")
		if (!(kappa_pool %in% pools)) {
			stop("pa_coupling_pair: kappa_pool must be one of ",
				 paste(pools, collapse = ", "))
		}

	#	Reference Sets
		refs_t <- pa_community_refs(edges_dated, era_map_t, era_t)
		refs_p <- pa_community_refs(edges_dated, era_map_prior, era_prior)

	#	Exposure, Before the Floor
		exp_t <- refs_t[, .(R = .N), by = community]
		exp_p <- refs_p[, .(R = .N), by = community]
		setorder(exp_t, community)
		setorder(exp_p, community)

	#	Apply the Floor
		exp_t <- exp_t[R >= as.integer(min_refs)]
		exp_p <- exp_p[R >= as.integer(min_refs)]
		refs_t <- refs_t[community %in% exp_t$community]
		refs_p <- refs_p[community %in% exp_p$community]

	#	Early return
		if (nrow(exp_t) == 0L || nrow(exp_p) == 0L) {
			return(list(
				coupling = data.table(community_i = integer(0),
									  community_j = integer(0),
									  n_coupling = integer(0)),
				exposure_t     = exp_t,
				exposure_prior = exp_p,
				summary = data.frame(era_prior = as.integer(era_prior),
									 era_t     = as.integer(era_t),
									 stringsAsFactors = FALSE)))
		}

	#	The Shared Vocabulary
		vocabulary <- sort(union(refs_t$work_id, refs_p$work_id))

	#	Report an Unexpectedly Large Grid
		n_grid <- as.numeric(nrow(exp_t)) * as.numeric(nrow(exp_p))
		if (n_grid > warn_rows) {
			message("  coupling grid: ", nrow(exp_t), " x ", nrow(exp_p), " = ",
					format(n_grid, big.mark = ",", scientific = FALSE), " pairs")
		}

	#	Incidence Matrices
	#	One row per community, one column per work, binary. Both sides share the
	#	vocabulary so that the product below is defined.
		incidence_t     <- pa_ref_incidence(refs_t, exp_t$community, vocabulary)
		incidence_prior <- pa_ref_incidence(refs_p, exp_p$community, vocabulary)

	#	Coupling, in One Product
	#	This is K in the spec's notation: K_ij counts the works i and j both cite.
		coupling_matrix <- incidence_t %*% Matrix::t(incidence_prior)

	#	The Nonzero Cells
		triplet <- as(coupling_matrix, "TsparseMatrix")
		coupling <- data.table(
			community_i = exp_t$community[triplet@i + 1L],
			community_j = exp_p$community[triplet@j + 1L],
			n_coupling  = as.integer(triplet@x))
		coupling <- coupling[n_coupling > 0L]
		setorder(coupling, community_i, community_j)

	#	The Three Degree Corrections
	#	Decision 2. One kappa per pool, all three carried on the summary row so a
	#	change of pool is a column swap rather than a rerun.
		edges_dt <- as.data.table(edges_dated)
		pair_arcs <- edges_dt[!is.na(sender_era) &
							  sender_era %in% c(as.integer(era_prior),
												as.integer(era_t))]
		mapped_arcs <- c(refs_to_arcs(edges_dt, era_map_t, era_t),
						 refs_to_arcs(edges_dt, era_map_prior, era_prior))
		sums_mapped <- pa_cite_counts(data.table(target_id = mapped_arcs))
		sums_all    <- pa_cite_counts(pair_arcs)
		sums_corpus <- pa_cite_counts(edges_dt)

		kappa_values <- c(pair_mapped = pa_kappa_from_sums(sums_mapped),
						  pair_all    = pa_kappa_from_sums(sums_all),
						  corpus      = pa_kappa_from_sums(sums_corpus))

	#	The Pool's Distinct Works
	#	Carried so that the Cauchy-Schwarz floor on kappa can be checked: with n
	#	distinct works, sum(d^2) >= sum(d)^2 / n, so kappa cannot fall below 1/n.
		n_pool_works <- length(unique(mapped_arcs))

	#	Assembling Result
		summary <- data.frame(
			era_prior            = as.integer(era_prior),
			era_t                = as.integer(era_t),
			n_communities_t      = nrow(exp_t),
			n_communities_prior  = nrow(exp_p),
			n_grid               = n_grid,
			n_nonzero            = nrow(coupling),
			density              = if (n_grid > 0) nrow(coupling) / n_grid else NA_real_,
			total_coupling       = sum(as.numeric(coupling$n_coupling)),
			max_coupling         = if (nrow(coupling) == 0L) 0L else
										max(coupling$n_coupling),
			n_vocabulary         = length(vocabulary),
			n_pool_works         = n_pool_works,
			R_t_min              = min(exp_t$R),
			R_t_median           = as.numeric(stats::median(exp_t$R)),
			R_t_max              = max(exp_t$R),
			R_prior_median       = as.numeric(stats::median(exp_p$R)),
			kappa_pair_mapped    = kappa_values[["pair_mapped"]],
			kappa_pair_all       = kappa_values[["pair_all"]],
			kappa_corpus         = kappa_values[["corpus"]],
			kappa_used           = kappa_values[[kappa_pool]],
			kappa_pool           = kappa_pool,
			stringsAsFactors     = FALSE)

		return(list(coupling = coupling, exposure_t = exp_t,
					exposure_prior = exp_p, summary = summary))
}

#' @title pa_load_coupling_grid
#' @description Rebuild one pair's full community-pair grid, zeros included.
#'
#' @param spec_tag Specification tag, as pa_spec_tag returns it.
#' @param era_prior Integer era t-1.
#' @param era_t Integer era t.
#' @param output_dir Coupling output root. Default CP_OUT_DIR.
#'
#' @details
#' The grid is the cross product of the two exposure tables, with the nonzero cells
#' joined on and everything else zero. It is never stored: 1.87 million rows across
#' the six pairs and five specifications would be nine million rows of mostly zeros,
#' against three small tables per pair that determine them.
#'
#' Model I is fit on what this returns. The exposure is given as its components, so
#' E^K = R_i * R_j * kappa can be formed, varied or replaced at fit time.
#'
#' @return A data.table with community_i, community_j, n_coupling, R_i, R_j and kappa.
#'
#' @examples
#' grid <- pa_load_coupling_grid("alpha1.00_g120_e14", 16, 17)
#'
#' @export
pa_load_coupling_grid <- function(spec_tag, era_prior, era_t,
								  output_dir = CP_OUT_DIR) {
	#	"""
	#	Args:
	#		spec_tag: specification tag
	#		era_prior: integer era t-1
	#		era_t: integer era t
	#		output_dir: coupling output root
	#	Returns:
	#		data.table(community_i, community_j, n_coupling, R_i, R_j, kappa)
	#	"""

	#	Paths
		stem <- file.path(output_dir, spec_tag,
						  sprintf("pair_%02d_%02d", era_prior, era_t))
		need <- c(coupling = paste0(stem, "_coupling.csv"),
				  exp_t    = paste0(stem, "_exposure_t.csv"),
				  exp_p    = paste0(stem, "_exposure_prior.csv"),
				  summary  = paste0(stem, "_summary.csv"))

	#	Validation
		missing <- need[!file.exists(need)]
		if (length(missing) > 0L) {
			stop("pa_load_coupling_grid: not found: ",
				 paste(basename(missing), collapse = ", "),
				 ". Run pa_run_coupling() for specification ", spec_tag, " first.")
		}

	#	Read
		cp <- as.data.table(readr::read_csv(need[["coupling"]],
											show_col_types = FALSE, progress = FALSE))
		et <- as.data.table(readr::read_csv(need[["exp_t"]],
											show_col_types = FALSE, progress = FALSE))
		ep <- as.data.table(readr::read_csv(need[["exp_p"]],
											show_col_types = FALSE, progress = FALSE))
		sm <- readr::read_csv(need[["summary"]], show_col_types = FALSE,
							  progress = FALSE)

	#	Expand
		grid <- CJ(community_i = sort(unique(et$community)),
				   community_j = sort(unique(ep$community)))
		if (nrow(cp) > 0L) {
			grid[cp, on = .(community_i, community_j),
				 n_coupling := i.n_coupling]
		} else {
			grid[, n_coupling := NA_integer_]
		}
		grid[is.na(n_coupling), n_coupling := 0L]

	#	Attach Exposure
		grid[et, on = c(community_i = "community"), R_i := i.R]
		grid[ep, on = c(community_j = "community"), R_j := i.R]
		grid[, kappa := as.numeric(sm$kappa_used[1])]

	#	Assembling Result
		setcolorder(grid, c("community_i", "community_j", "n_coupling",
							"R_i", "R_j", "kappa"))
		return(grid)
}

#' @title pa_run_coupling
#' @description Build coupling and exposure for every specification and era pair.
#'
#' @param specs Specification table. Default TL_SPECS.
#' @param eras Integer eras in scope. Default PA_ANALYTIC_ERAS.
#' @param era_dir Directory holding the node and edge lists. Default CP_ERA_DIR.
#' @param output_dir Destination root. Default CP_OUT_DIR.
#' @param verbose Report progress.
#'
#' @details
#' The structure mirrors pa_run_tie_layer deliberately: one specification at a time,
#' era maps built once per specification, pairs walked in order, each pair's small
#' tables written before the next is built. Nothing is held across specifications,
#' so a run can be interrupted and resumed a specification at a time.
#'
#' @return Invisibly, a list with summary and the output paths.
#'
#' @examples
#' res <- pa_run_coupling()
#' res$summary
#'
#' @export
pa_run_coupling <- function(specs = NULL, eras = NULL,
							era_dir = CP_ERA_DIR, output_dir = CP_OUT_DIR,
							verbose = TRUE) {
	#	"""
	#	Args:
	#		specs: specification table, or NULL for TL_SPECS
	#		eras: integer eras, or NULL for PA_ANALYTIC_ERAS
	#		era_dir: directory holding the node and edge lists
	#		output_dir: destination root
	#		verbose: report progress
	#	Returns:
	#		invisible list(summary, paths)
	#	"""

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- TL_SPECS
		if (is.null(eras))  eras  <- PA_ANALYTIC_ERAS

	#	Validation
		need <- c("alpha", "anchor", "anchor_era", "grid")
		missing <- setdiff(need, names(specs))
		if (length(missing) > 0L) {
			stop("pa_run_coupling: specs is missing: ",
				 paste(missing, collapse = ", "))
		}

	#	Load the Corpus
	#	This block is pa_run_tie_layer's, deliberately unchanged. Three details in it
	#	are load-bearing and none of them is obvious. The objects are fetched by NAME
	#	-- pa_nodes and pa_edges -- because load() returns whatever names the file
	#	happens to carry and indexing that is a lottery. They are loaded into a
	#	dedicated environment so nothing in the caller's frame is shadowed. And
	#	as.data.frame() is required, not cosmetic: the saved objects are data.tables,
	#	and pa_dedupe_edges selects columns with edges[c("sender_id", "target_id")],
	#	which a data.table reads as an i-join and rejects.
		nf <- file.path(era_dir, "pa_node_list.Rda")
		ef <- file.path(era_dir, "pa_edge_list.Rda")
		for (f in c(nf, ef)) {
			if (!file.exists(f)) stop("pa_run_coupling: file not found: ", f)
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

	#	Date and De-duplicate the Edges
	#	pa_dedupe_edges returns list(edges, n_before, n_after, n_removed), not a
	#	frame, so the frame has to be taken off it before it is dated.
		ded <- pa_dedupe_edges(edge_list, verbose = verbose)
		edges <- pa_attach_sender_era(ded$edges, node_list, verbose = verbose)

	#	Pairs
		pairs <- pa_era_pairs(eras)

	#	Collect
		rows  <- list()
		paths <- character(0)

	#	Iterating Over Specifications
		for (s in seq_len(nrow(specs))) {
			tag <- pa_spec_tag(specs$alpha[s], specs$anchor[s], specs$anchor_era[s])
			if (verbose) cat("\nSpecification ", tag, "\n", sep = "")

			#	Era Maps for This Specification
				maps <- pa_build_era_maps(node_list, eras = eras,
										  grid = specs$grid[s],
										  alpha = specs$alpha[s],
										  anchor = specs$anchor[s],
										  anchor_era = specs$anchor_era[s],
										  verbose = verbose)

			#	Destination
				spec_dir <- file.path(output_dir, tag)
				dir.create(spec_dir, recursive = TRUE, showWarnings = FALSE)

			#	Iterating Over Pairs
				for (p in seq_len(nrow(pairs))) {
					ep <- pairs$era_prior[p]
					et <- pairs$era_t[p]
					if (verbose) cat("  pair ", ep, " -> ", et, "\n", sep = "")

					res <- pa_coupling_pair(
						edges,
						era_map_prior = maps[[as.character(ep)]],
						era_map_t     = maps[[as.character(et)]],
						era_prior     = ep,
						era_t         = et)

					#	Write the Small Tables
					#	These three determine the grid, which is why the grid is not
					#	written. See pa_load_coupling_grid().
						stem <- file.path(spec_dir,
										  sprintf("pair_%02d_%02d", ep, et))
						readr::write_csv(res$coupling,
										 paste0(stem, "_coupling.csv"))
						readr::write_csv(res$exposure_t,
										 paste0(stem, "_exposure_t.csv"))
						readr::write_csv(res$exposure_prior,
										 paste0(stem, "_exposure_prior.csv"))
						readr::write_csv(res$summary,
										 paste0(stem, "_summary.csv"))
						paths <- c(paths, paste0(stem, "_coupling.csv"))

					#	The Expanded Grid Is Not Written
						if (isTRUE(CP_WRITE_GRID)) {
							grid <- pa_load_coupling_grid(tag, ep, et, output_dir)
							readr::write_csv(grid, paste0(stem, "_grid.csv"))
						}

					#	Collect the Summary
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
		readr::write_csv(summary, file.path(output_dir, "coupling_summary.csv"))

	#	Checks
		tests <- pa_run_coupling_tests(summary, edges, node_list,
									   specs = specs, eras = eras,
									   output_dir = output_dir)
		if (verbose) {
			cat("\n")
			print(tests, row.names = FALSE)
			cat("\n", sum(tests$result == "PASS"), " of ", nrow(tests),
				" checks passed\n", sep = "")
		}

	#	Assembling Result
		return(invisible(list(summary = summary, tests = tests, paths = paths)))
}

#' @title pa_run_coupling_tests
#' @description Verify the coupling stage's output against quantities it did not use.
#'
#' @param summary Output of pa_run_coupling.
#' @param edges_dated Edge list carrying sender_era.
#' @param specs Specification table. Default TL_SPECS.
#' @param eras Integer eras in scope. Default PA_ANALYTIC_ERAS.
#' @param output_dir Destination for the test table, or NULL to skip writing.
#'
#' @details
#' The checks that matter are the ones a different code path can confirm. Coupling is
#' bounded above by the smaller of the two reference counts, which the matrix product
#' never consults; a pair's grid rebuilt from the exposure tables must have exactly
#' the row count the summary recorded; and the descent ties of the tie layer must
#' rest on works the citing community actually cites, which bounds the bridging count
#' by R_i.
#'
#' @return A data.frame of checks.
#'
#' @export
pa_run_coupling_tests <- function(summary, edges_dated, node_list, specs = NULL,
								  eras = NULL, output_dir = CP_OUT_DIR) {
	#	"""
	#	Args:
	#		summary: output of pa_run_coupling
	#		edges_dated: edge list carrying sender_era
	#		node_list: rebuilt citation node list, for the determinism check
	#		specs: specification table, or NULL for TL_SPECS
	#		eras: integer eras, or NULL for PA_ANALYTIC_ERAS
	#		output_dir: destination for the test table, or NULL
	#	Returns:
	#		data.frame of checks
	#	"""

	#	Resolve the Deferred Defaults
		if (is.null(specs)) specs <- TL_SPECS
		if (is.null(eras))  eras  <- PA_ANALYTIC_ERAS

	#	Collect
		out <- list()

	#	Coverage
		if (nrow(summary) > 0L) {
			out[[length(out) + 1L]] <- pa_check_cp(
				"every specification produced every pair",
				all(table(summary$spec_tag) == nrow(pa_era_pairs(eras))),
				sprintf("%d specification(s) x %d pair(s)",
						length(unique(summary$spec_tag)), nrow(pa_era_pairs(eras))))
			out[[length(out) + 1L]] <- pa_check_cp(
				"every pair produced coupling",
				all(summary$n_nonzero > 0L),
				sprintf("min %d nonzero cell(s)", min(summary$n_nonzero)))
			out[[length(out) + 1L]] <- pa_check_cp(
				"kappa lies in (0, 1]",
				all(summary$kappa_used > 0 & summary$kappa_used <= 1),
				sprintf("%.3e to %.3e", min(summary$kappa_used),
						max(summary$kappa_used)))
			out[[length(out) + 1L]] <- pa_check_cp(
				"kappa clears its Cauchy-Schwarz floor",
				all(summary$kappa_pair_mapped >= 1 / summary$n_pool_works),
				sprintf("min ratio to 1/n %.2f",
						min(summary$kappa_pair_mapped * summary$n_pool_works)))
			out[[length(out) + 1L]] <- pa_check_cp(
				"density is a share",
				all(summary$density > 0 & summary$density <= 1),
				sprintf("%.1f%% to %.1f%%", 100 * min(summary$density),
						100 * max(summary$density)))
		}

	#	The Upper Bound on Coupling
	#	Two communities cannot share more works than the smaller of them cites. The
	#	matrix product never looks at R, so this is an independent confirmation.
		bound_ok <- NA
		bound_detail <- "not run"
		if (nrow(summary) > 0L) {
			#	The First Specification's First Pair
				first_tag  <- summary$spec_tag[1]
				first_spec <- summary[summary$spec_tag == first_tag, ,
									  drop = FALSE]

			#	Rebuild That Pair's Grid
				grid <- tryCatch(
					pa_load_coupling_grid(first_tag, first_spec$era_prior[1],
										  first_spec$era_t[1], output_dir),
					error = function(e) NULL)

			#	Compare
				if (!is.null(grid)) {
					excess <- grid$n_coupling - pmin(grid$R_i, grid$R_j)
					bound_ok <- all(excess <= 0L)
					bound_detail <- sprintf("max excess %d over %d cell(s)",
											max(excess), nrow(grid))
				}
		}
		out[[length(out) + 1L]] <- pa_check_cp(
			"coupling does not exceed the smaller reference set",
			bound_ok, bound_detail)

	#	The Grid Rebuilds to Its Recorded Size
	#	The summary records n_grid and n_nonzero from the matrix product. Rebuilding
	#	from the exposure tables has to agree, or the zeros are not reconstructible
	#	and the decision not to store them is unsafe.
		size_ok <- NA
		size_detail <- "not run"
		if (nrow(summary) > 0L) {
			#	The First Specification's First Pair
				first_tag  <- summary$spec_tag[1]
				first_spec <- summary[summary$spec_tag == first_tag, ,
									  drop = FALSE]

			#	Rebuild That Pair's Grid
				grid <- tryCatch(
					pa_load_coupling_grid(first_tag, first_spec$era_prior[1],
										  first_spec$era_t[1], output_dir),
					error = function(e) NULL)

			#	Compare
				if (!is.null(grid)) {
					size_ok <- nrow(grid) == first_spec$n_grid[1] &&
							   sum(grid$n_coupling > 0L) == first_spec$n_nonzero[1]
					size_detail <- sprintf(
						"%d rows, %d nonzero against %.0f and %d",
						nrow(grid), sum(grid$n_coupling > 0L),
						first_spec$n_grid[1], first_spec$n_nonzero[1])
				}
		}
		out[[length(out) + 1L]] <- pa_check_cp(
			"the grid rebuilds to its recorded size", size_ok, size_detail)

	#	Determinism: Shuffling the Edge List Changes Nothing
	#	Built on the smallest pair, since the point is invariance rather than scale.
		det_ok <- NA
		det_detail <- "not run"
		if (nrow(specs) > 0L && length(eras) >= 2L &&
			exists("pa_build_era_maps", mode = "function")) {
			#	The Smallest Pair
				era_prior <- eras[1]
				era_t     <- eras[2]

			#	Era Maps for It
				maps <- tryCatch(
					pa_build_era_maps(node_list, eras = c(era_prior, era_t),
									  grid = specs$grid[1],
									  alpha = specs$alpha[1],
									  anchor = specs$anchor[1],
									  anchor_era = specs$anchor_era[1],
									  verbose = FALSE),
					error = function(e) NULL)

			#	Build It Twice, Once From a Shuffled Edge List
				if (!is.null(maps)) {
					ordered_run <- pa_coupling_pair(
						edges_dated, maps[[as.character(era_prior)]],
						maps[[as.character(era_t)]], era_prior, era_t)
					set.seed(7L)
					shuffled <- edges_dated[sample.int(nrow(edges_dated)), ,
											drop = FALSE]
					shuffled_run <- pa_coupling_pair(
						shuffled, maps[[as.character(era_prior)]],
						maps[[as.character(era_t)]], era_prior, era_t)

				#	Compare
					det_ok <- identical(nrow(ordered_run$coupling),
										nrow(shuffled_run$coupling)) &&
							  isTRUE(all.equal(
								  sum(ordered_run$coupling$n_coupling),
								  sum(shuffled_run$coupling$n_coupling)))
					det_detail <- sprintf("pair %d -> %d, %d cell(s)",
										  era_prior, era_t,
										  nrow(ordered_run$coupling))
				}
		}
		out[[length(out) + 1L]] <- pa_check_cp(
			"shuffling the edge list changes nothing", det_ok, det_detail)

	#	Assembling Result
		tests <- do.call("rbind", out)
		rownames(tests) <- NULL
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests, file.path(output_dir, "coupling_tests.csv"))
		}
		return(tests)
}

#######################
#   EXECUTING TESTS   #
#######################

#	Run the Lane
#	This runs when the file is EXECUTED as a script and does not run when the file is
#	SOURCED -- by PathA_Pipeline.R, by a test suite, or from the console. The guard is
#	R's equivalent of Python's __name__ == "__main__": sys.nframe() is 1 at the top of
#	a script under Rscript and 5 or more inside source(), because source() is itself a
#	function call several frames deep.
#
#		Rscript scripts/Path_A/PathA_Coupling.R     runs the lane
#		source("scripts/Path_A/PathA_Coupling.R")   defines the functions only
#
#	The guard is what makes an executable block safe here. This file is fifth in the
#	pipeline's load order, so an unguarded block would build the whole coupling stage
#	as a side effect of loading the pipeline, before the preflight had checked a
#	single input. A commented-out block would be safe too, and useless: it documents
#	a command instead of providing one.
	if (sys.nframe() <= 1L) {

		#	Dependencies
		#	Sourced explicitly because a script run has no pipeline to have loaded
		#	them. Each of these files carries its own guard, so sourcing them here
		#	defines their functions without running their lanes.
			PA_ROOT    <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")
			PA_SCRIPTS <- file.path(PA_ROOT, "scripts")
			deps <- c("RPajekFunctions_30April2023.r",
					  "Path_A/PathA_PajekReaders.R",
					  "Path_A/PathA_EraResolution.R",
					  "Path_A/PathA_TieLayer.R")

			cat("PathA_Coupling.R -- the coupling lane\n")
			cat("=====================================\n\n")
			cat("Sourcing dependencies...\n")
			for (dep in deps) {
				path <- file.path(PA_SCRIPTS, dep)
				if (!file.exists(path)) {
					stop("PathA_Coupling.R: dependency not found: ", path)
				}
				source(path)
				cat("  loaded ", basename(dep), "\n", sep = "")
			}

		#	Validation
		#	The lane's own preflight. Four things it cannot run without, checked
		#	before the first sparse product rather than after. Built as data so the
		#	block carries no inline function of its own.
			cat("\nPreflight...\n")
			dir.create(CP_OUT_DIR, recursive = TRUE, showWarnings = FALSE)

			have_specs <- exists("TL_SPECS") && nrow(TL_SPECS) > 0L
			have_eras  <- exists("PA_ANALYTIC_ERAS") &&
						  length(PA_ANALYTIC_ERAS) >= 2L
			have_lists <- file.exists(file.path(CP_ERA_DIR, "pa_node_list.Rda")) &&
						  file.exists(file.path(CP_ERA_DIR, "pa_edge_list.Rda"))
			can_write  <- dir.exists(CP_OUT_DIR) &&
						  file.access(CP_OUT_DIR, 2L) == 0L

			pre_tab <- data.frame(
				check = c("the specification table is loaded",
						  "the analytic eras are declared",
						  "the node and edge lists exist",
						  "the output directory is writable"),
				ok    = c(have_specs, have_eras, have_lists, can_write),
				detail = c(
					if (have_specs) {
						sprintf("%d specification(s)", nrow(TL_SPECS))
					} else {
						"TL_SPECS absent or empty"
					},
					if (have_eras) {
						paste(range(PA_ANALYTIC_ERAS), collapse = " to ")
					} else {
						"PA_ANALYTIC_ERAS absent or too short"
					},
					CP_ERA_DIR,
					CP_OUT_DIR),
				stringsAsFactors = FALSE)
			print(pre_tab, row.names = FALSE)
			if (!all(pre_tab$ok)) {
				cat("\nPreflight FAILED; nothing was run\n")
				quit(status = 1L)
			}

		#	Run
			cat("\nBuilding coupling...\n")
			t0 <- Sys.time()
			cp <- pa_run_coupling()
			secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

		#	Report
			cat("\n=====================================\n")
			print(cp$summary[, c("spec_tag", "era_prior", "era_t", "n_grid",
								 "n_nonzero", "density", "total_coupling",
								 "kappa_used")],
				  row.names = FALSE)
			cat("\nBuilt in ", round(secs, 1), "s\n", sep = "")
			cat("Output: ", CP_OUT_DIR, "\n", sep = "")

		#	Exit Status
		#	Non-zero when a check failed, so a shell or a scheduler learns about it.
		#	This is the same contract Test_PathA_TieLayer.R honours.
			n_fail <- sum(cp$tests$result == "FAIL")
			if (n_fail > 0L) {
				cat("FAILED: ", n_fail, " check(s)\n", sep = "")
				quit(status = 1L)
			}
			cat("OK\n")
	}
