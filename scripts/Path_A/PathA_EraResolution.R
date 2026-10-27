#	PathA_EraResolution.R -- Era Windows, Era Pairs, and Mapping Vertices to Nodes
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	27 October 2026
#
#	WHERE THIS SITS. Stages 1 and 2 of the pipeline order of operations
#	(specification S14.1). Requires PathA_PajekReaders.R. Feeds PathA_TieLayer.R,
#	which needs one era map per era before it can attribute any citation.
#	Tested by tests/Test_PathA_TieLayer.R.
#
#	WHAT CHANGED FROM THE SEPTEMBER VERSION, AND WHY.
#
#	1.	ERAS ARE SEVEN-YEAR WINDOWS, NOT SINGLE YEARS. The September file mapped
#		eras 12 to 23 onto the publication years 2009 to 2020. The rebuilt
#		construction tiles 1901 to 2021 in seven-year windows, so era 12 is 1978-84
#		and the analytic range is eras 11 to 17 -- seven eras and six consecutive
#		pairs, where the annual design had twelve and eleven. pa_era_year() is gone;
#		pa_era_table() and pa_era_window() replace it.
#
#	2.	TIME IS CARRIED AS pair_index. Fixed tiling makes era midpoints an affine
#		function of the era number -- midpoint = 1904 + 7(era - 1) -- so a linear
#		term in pair_index and one in midpoints fit identically and differ only by a
#		factor of seven in the coefficient. pair_index is therefore the trend, and
#		the window bounds are carried as labels. pa_attach_pair_sizes() adds each
#		era's citing-article count so that log field size, which is NOT collinear
#		with pair_index because the growth is uneven, remains available as an
#		alternative clock without rebuilding anything.
#
#	3.	ONE LAYOUT, ONE PARTITION CONVENTION, AND A SPECIFICATION. The September
#		resolver searched three directory layouts and two partition naming schemes
#		in a documented priority order, because the tree had grown that way. The
#		rebuilt tree has one layout. What varies instead is the SPECIFICATION: the
#		scaling rule and anchor the partition was calibrated to (S14.8). So the
#		resolver takes a specification and finds exactly one file, and an ambiguity
#		is an error rather than a priority decision.
#
#			network      <pajek_dir>/era<NN>.net                     (two digits)
#			partition    <sweep_root>/<grid>/specifications/<tag>/era<NN>_<tag>.clu
#			tag          alpha<A.AA>_g<anchor>_e<anchor era>
#
#	4.	THE NODE LIST RENAMED TWO COLUMNS. The rebuilt node list carries node_key
#		and the flags is_article and is_cited where the 2024 list carried node_label
#		and role. pa_era_map() reads the new names, derives role from the flags so
#		downstream code that expects it still works, and emits node_label as an
#		alias of node_key so PathA_TieLayer.R runs unmodified. The alias is
#		deprecated and should go once the tie layer is revised.
#
#	Resolution stays deterministic: one candidate per input, recorded in the
#	manifest, so a layout change shows up as a diff rather than as a silent switch
#	of input file.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

#####################
#   CONFIGURATION   #
#####################

#	Era Tiling
#	Mirrors pa_build_era_table() in PathA_Revised_Eras.R. Kept as parameters rather
#	than a hard-coded table so a width change propagates instead of silently
#	disagreeing with the construction.
	PA_ERA_WIDTH <- 7L
	PA_YEAR_MIN  <- 1901L
	PA_YEAR_MAX  <- 2021L

#	Analytic Range
#	Mean degree reaches its plateau at era 11 and era 18 spans only 2020-2021, so
#	the analytic range is 11 to 17: seven eras, six consecutive pairs.
	PA_ANALYTIC_ERAS <- 11:17

#	Default Input Roots
	PA_PAJEK_DIR  <- "/workspace/caffeine_citation/pajek_files/eras_7yr"
	PA_SWEEP_ROOT <- "/workspace/caffeine_citation/pajek_files/champ_sweep_7yr"
	PA_ERA_DIR    <- "/workspace/caffeine_citation/data/eras_7yr"

#	Adopted Specification
#	alpha = 1 holds granularity approximately constant; the anchor sets its level
#	at era 14. See S14.8 for why these values and not others.
	PA_GRID        <- "gamma_0.02_6.0_n64"
	PA_ALPHA       <- 1.00
	PA_ANCHOR      <- 120
	PA_ANCHOR_ERA  <- 14L

###############
#   HELPERS   #
###############

#	The Era Window Table
	pa_era_table <- function(width = PA_ERA_WIDTH, year_min = PA_YEAR_MIN,
							 year_max = PA_YEAR_MAX) {
		#	"""
		#	Args:
		#		width: era width in years (default PA_ERA_WIDTH)
		#		year_min: first year of era 1 (default PA_YEAR_MIN)
		#		year_max: last year covered; truncates the final era (default PA_YEAR_MAX)
		#	Returns:
		#		data.frame(era, year_start, year_end, n_years, midpoint)
		#	Notes:
		#		Era n runs from year_min + width*(n-1) for width years, truncated at
		#		year_max. Era 11 is 1971-77 and era 17 is 2013-19 under the defaults;
		#		era 18 is 2020-2021 and is short, which is why it sits outside the
		#		analytic range.
		#
		#		midpoint is reported because it is the obvious alternative time value
		#		and because stating it makes its redundancy visible: under fixed
		#		tiling it is 1904 + 7(era - 1), an affine function of the era number,
		#		so it carries exactly the information pair_index carries.
		#	"""

		#	Validation
			if (width < 1L) stop("pa_era_table: width must be at least 1")
			if (year_max < year_min) stop("pa_era_table: year_max precedes year_min")

		#	Tile the Range
			n_era <- as.integer(ceiling((year_max - year_min + 1L) / width))
			era   <- seq_len(n_era)
			start <- year_min + width * (era - 1L)
			end   <- pmin(start + width - 1L, year_max)

		#	Assembling Result
			return(data.frame(era        = as.integer(era),
							  year_start = as.integer(start),
							  year_end   = as.integer(end),
							  n_years    = as.integer(end - start + 1L),
							  midpoint   = (start + end) / 2,
							  stringsAsFactors = FALSE))
	}

#	The Window of One Era
	pa_era_window <- function(era, ...) {
		#	"""
		#	Args:
		#		era: integer vector of era numbers
		#		...: passed to pa_era_table()
		#	Returns:
		#		data.frame(era, year_start, year_end, n_years, midpoint), one row per
		#		requested era, NA where the era is outside the tiling
		#	Notes:
		#		Replaces pa_era_year(), which returned a single year and cannot under
		#		a seven-year tiling.
		#	"""

		#	Look Up
			tab <- pa_era_table(...)
			out <- tab[match(as.integer(era), tab$era), , drop = FALSE]
			out$era <- as.integer(era)

		#	Assembling Result
			rownames(out) <- NULL
			return(out)
	}

#	The Specification Tag
	pa_spec_tag <- function(alpha = PA_ALPHA, anchor = PA_ANCHOR,
							anchor_era = PA_ANCHOR_ERA) {
		#	"""
		#	Args:
		#		alpha: scaling exponent (default PA_ALPHA)
		#		anchor: target citing articles per effective community (default PA_ANCHOR)
		#		anchor_era: era the anchor is set at (default PA_ANCHOR_ERA)
		#	Returns:
		#		character tag, e.g. "alpha1.00_g120_e14"
		#	Notes:
		#		The tag is the directory name the partition writer uses and the suffix
		#		on the file itself, so it is built in one place rather than pasted at
		#		each call site. alpha carries two decimals; the anchor drops trailing
		#		zeros so 120 renders as "120" rather than "120.0".
		#	"""

		#	Assembling Result
			return(sprintf("alpha%.2f_g%s_e%d", as.numeric(alpha),
						   formatC(as.numeric(anchor), format = "fg",
								   drop0trailing = TRUE),
						   as.integer(anchor_era)))
	}

#	Declared Vertex Count from a Network Header
	pa_net_vertices <- function(net_path) {
		#	"""
		#	Args:
		#		net_path: path to a .net file
		#	Returns:
		#		integer vertex count from the *Vertices header, NA when absent
		#	Notes:
		#		Reads the first few lines only. The point is to check a partition's
		#		length against its network in the preflight, before read_net() parses
		#		seventy thousand vertices to discover the same mismatch.
		#	"""

		#	Read the Header
			ln <- readLines(net_path, n = 5L, warn = FALSE)
			hit <- grep("^\\s*\\*[Vv]ertices", ln)

		#	Early return
			if (length(hit) == 0L) return(NA_integer_)

		#	Assembling Result
			return(suppressWarnings(as.integer(
				sub("^\\s*\\*[Vv]ertices\\s+([0-9]+).*$", "\\1", ln[hit[1L]]))))
	}

#	Citing Articles in One Era
	pa_era_citers <- function(era, era_dir = PA_ERA_DIR) {
		#	"""
		#	Args:
		#		era: integer era number
		#		era_dir: directory holding era<NN>_nodes.Rda (default PA_ERA_DIR)
		#	Returns:
		#		integer count of vertices with within-era out-degree of at least one,
		#		or NA when the file is absent
		#	Notes:
		#		Read from the era node table rather than from the granularity output,
		#		so the pair table does not depend on a granularity run having
		#		happened. This is the W of S14.8.
		#	"""

		#	Early return
			f <- file.path(era_dir, sprintf("era%02d_nodes.Rda", era))
			if (!file.exists(f)) return(NA_integer_)

		#	Load and Count
			env <- new.env(parent = emptyenv())
			load(f, envir = env)
			if (!exists("era_nodes", envir = env, inherits = FALSE)) return(NA_integer_)
			nodes <- get("era_nodes", envir = env, inherits = FALSE)
			if (!("out_degree" %in% names(nodes))) return(NA_integer_)

		#	Assembling Result
			return(as.integer(sum(nodes$out_degree > 0L)))
	}

#########################
#   PRIMARY FUNCTIONS   #
#########################

#' @title pa_era_pairs
#' @description Build the consecutive era pairs of the analytic range, with pair_index as the time trend.
#'
#' @param eras Integer vector of eras, contiguous and ascending. Default PA_ANALYTIC_ERAS.
#' @param ... Passed to pa_era_table for the window bounds.
#'
#' @details
#' Six pairs from seven eras under the default range, where the annual design had
#' eleven from twelve. pair_index runs 1 to 6 and is the column the design
#' matrices of S13 carry as the time trend.
#'
#' pair_index rather than a calendar value, because fixed seven-year tiling makes
#' era midpoints an affine function of the era number: midpoint = 1904 + 7(era - 1).
#' A linear term in midpoints and one in pair_index therefore fit identically and
#' differ only by a factor of seven in the coefficient. The window bounds and
#' midpoints are carried as labels, and would start carrying information of their
#' own only if the window became sliding or variable in width.
#'
#' @return A data.frame with pair_index, era_prior, era_t, the window bounds and
#'   midpoints of both eras, and pair_id.
#'
#' @examples
#' pa_era_pairs()
#'
#' @export
pa_era_pairs <- function(eras = PA_ANALYTIC_ERAS, ...) {
	#	"""
	#	Args:
	#		eras: integer vector of eras, contiguous and ascending
	#		...: passed to pa_era_table()
	#	Returns:
	#		data.frame, one row per consecutive pair
	#	"""

	#	Validation
		eras <- sort(unique(as.integer(eras)))
		if (length(eras) < 2L) stop("pa_era_pairs: need at least two eras to form a pair")
		if (!all(diff(eras) == 1L)) {
			stop("pa_era_pairs: era sequence is not contiguous: ",
				 paste(eras, collapse = ", "))
		}

	#	Pair the Eras
		prior <- eras[-length(eras)]
		curr  <- eras[-1L]
		w_prior <- pa_era_window(prior, ...)
		w_t     <- pa_era_window(curr, ...)

	#	Assembling Result
		return(data.frame(pair_index       = seq_along(prior),
						  era_prior        = prior,
						  era_t            = curr,
						  year_prior_start = w_prior$year_start,
						  year_prior_end   = w_prior$year_end,
						  year_t_start     = w_t$year_start,
						  year_t_end       = w_t$year_end,
						  midpoint_prior   = w_prior$midpoint,
						  midpoint_t       = w_t$midpoint,
						  pair_id          = paste0(prior, "_", curr),
						  stringsAsFactors = FALSE))
}

#' @title pa_attach_pair_sizes
#' @description Attach each era's citing-article count to the pair table, so log field size is available as an alternative time value.
#'
#' @param pairs Output of pa_era_pairs.
#' @param era_dir Directory holding era<NN>_nodes.Rda. Default PA_ERA_DIR.
#'
#' @details
#' pair_index is the primary trend and is sufficient under fixed tiling. Log field
#' size is the one alternative that is not collinear with it: across the six pairs
#' the prior-era citing counts are 518, 897, 1,627, 4,244, 4,626 and 5,891, whose
#' log increments are 0.55, 0.60, 0.96, 0.09 and 0.24 -- a large step at pair three
#' and almost none at pair four. The two clocks therefore say different things,
#' pair_index that time passed evenly and log size that the field grew unevenly,
#' and which is the right one depends on whether the drift being modelled is about
#' elapsed time or about the field maturing.
#'
#' This function exists so that choice stays open at modelling time rather than
#' being foreclosed at construction. It does not express a preference.
#'
#' @return The pair table with n_citers_prior, n_citers_t, and their logs.
#'
#' @export
pa_attach_pair_sizes <- function(pairs, era_dir = PA_ERA_DIR) {
	#	"""
	#	Args:
	#		pairs: output of pa_era_pairs()
	#		era_dir: directory holding era<NN>_nodes.Rda
	#	Returns:
	#		data.frame, pairs plus n_citers_prior, n_citers_t, log_citers_prior,
	#		log_citers_t
	#	Notes:
	#		An era whose node table is absent yields NA rather than an error, so a
	#		partial construction still produces a usable pair table.
	#	"""

	#	Validation
		need <- c("era_prior", "era_t")
		miss <- setdiff(need, names(pairs))
		if (length(miss) > 0L) {
			stop("pa_attach_pair_sizes: pairs is missing column(s): ",
				 paste(miss, collapse = ", "))
		}

	#	Count Once per Era
		eras <- sort(unique(c(pairs$era_prior, pairs$era_t)))
		counts <- vapply(eras, pa_era_citers, integer(1), era_dir = era_dir)
		names(counts) <- as.character(eras)

	#	Attach
		out <- pairs
		out$n_citers_prior <- unname(counts[as.character(out$era_prior)])
		out$n_citers_t     <- unname(counts[as.character(out$era_t)])
		out$log_citers_prior <- ifelse(is.na(out$n_citers_prior) |
									   out$n_citers_prior <= 0L,
									   NA_real_, log(out$n_citers_prior))
		out$log_citers_t     <- ifelse(is.na(out$n_citers_t) |
									   out$n_citers_t <= 0L,
									   NA_real_, log(out$n_citers_t))

	#	Assembling Result
		rownames(out) <- NULL
		return(out)
}

#' @title pa_resolve_era
#' @description Find one era's network and its partition for a given specification.
#'
#' @param era Integer era number.
#' @param alpha Scaling exponent of the specification. Default PA_ALPHA.
#' @param anchor Anchor granularity of the specification. Default PA_ANCHOR.
#' @param anchor_era Era the anchor is set at. Default PA_ANCHOR_ERA.
#' @param grid Sweep grid directory the partition was written under. Default PA_GRID.
#' @param pajek_dir Directory holding era<NN>.net. Default PA_PAJEK_DIR.
#' @param sweep_root Root of the partition tree. Default PA_SWEEP_ROOT.
#'
#' @details
#' One candidate per input. The September resolver searched three directory layouts
#' and two partition naming conventions in a documented priority order, because the
#' tree had grown that way; the rebuilt tree has one layout, and what varies is the
#' specification instead. An absent file is therefore an error naming the exact path
#' that was expected, not a fallback.
#'
#' Era numbers are zero-padded to two digits, as the construction writes them, so
#' a lexical listing sorts correctly.
#'
#' @return A list with era, net, clu, spec_tag, grid, and the declared vertex count
#'   from the network header.
#'
#' @examples
#' pa_resolve_era(14)
#'
#' @export
pa_resolve_era <- function(era, alpha = PA_ALPHA, anchor = PA_ANCHOR,
						   anchor_era = PA_ANCHOR_ERA, grid = PA_GRID,
						   pajek_dir = PA_PAJEK_DIR, sweep_root = PA_SWEEP_ROOT) {
	#	"""
	#	Args:
	#		era: integer era number
	#		alpha, anchor, anchor_era: the specification
	#		grid: sweep grid directory
	#		pajek_dir: directory holding era<NN>.net
	#		sweep_root: root of the partition tree
	#	Returns:
	#		list(era, net, clu, spec_tag, grid, net_vertices)
	#	"""

	#	Build the Paths
		tag <- pa_spec_tag(alpha, anchor, anchor_era)
		net <- file.path(pajek_dir, sprintf("era%02d.net", era))
		clu <- file.path(sweep_root, grid, "specifications", tag,
						 sprintf("era%02d_%s.clu", era, tag))

	#	Validation
		if (!file.exists(net)) {
			stop("pa_resolve_era: no network for era ", era, ". Expected:\n  ", net)
		}
		if (!file.exists(clu)) {
			stop("pa_resolve_era: no partition for era ", era,
				 " at specification ", tag, ". Expected:\n  ", clu)
		}

	#	Assembling Result
		return(list(era          = as.integer(era),
					net          = net,
					clu          = clu,
					spec_tag     = tag,
					grid         = as.character(grid),
					net_vertices = pa_net_vertices(net)))
}

#' @title pa_era_manifest
#' @description Resolve every era in scope and record what was found, as a preflight.
#'
#' @param eras Integer vector of eras to resolve. Default PA_ANALYTIC_ERAS.
#' @param alpha Scaling exponent of the specification. Default PA_ALPHA.
#' @param anchor Anchor granularity. Default PA_ANCHOR.
#' @param anchor_era Era the anchor is set at. Default PA_ANCHOR_ERA.
#' @param grid Sweep grid directory. Default PA_GRID.
#' @param pajek_dir Directory holding era<NN>.net. Default PA_PAJEK_DIR.
#' @param sweep_root Root of the partition tree. Default PA_SWEEP_ROOT.
#'
#' @details
#' Runs before any worker starts, so a missing input fails the whole run in under a
#' second rather than twenty minutes into a parallel job. The partition length is
#' checked against the network's declared vertex count here, from the header alone,
#' rather than after read_net has parsed seventy thousand vertices to discover the
#' same mismatch. Write it out with the run and a later reader can see exactly which
#' files produced the results.
#'
#' Requires PathA_PajekReaders.R for pa_read_clu.
#'
#' @return A data.frame, one row per era.
#'
#' @export
pa_era_manifest <- function(eras = PA_ANALYTIC_ERAS, alpha = PA_ALPHA,
							anchor = PA_ANCHOR, anchor_era = PA_ANCHOR_ERA,
							grid = PA_GRID, pajek_dir = PA_PAJEK_DIR,
							sweep_root = PA_SWEEP_ROOT) {
	#	"""
	#	Args:
	#		eras: integer vector of eras to resolve
	#		alpha, anchor, anchor_era: the specification
	#		grid: sweep grid directory
	#		pajek_dir, sweep_root: input roots
	#	Returns:
	#		data.frame, one row per era
	#	"""

	#	Validation
		if (!exists("pa_read_clu", mode = "function")) {
			stop("pa_era_manifest: pa_read_clu() not found; source PathA_PajekReaders.R first")
		}

	#	Iterating Over Eras
		win <- pa_era_window(eras)
		rows <- lapply(seq_along(eras), function(i) {
			e <- eras[i]
			r <- pa_resolve_era(e, alpha, anchor, anchor_era, grid, pajek_dir,
								sweep_root)
			part <- pa_read_clu(r$clu)
			data.frame(era            = r$era,
					   year_start     = win$year_start[i],
					   year_end       = win$year_end[i],
					   spec_tag       = r$spec_tag,
					   grid           = r$grid,
					   net            = r$net,
					   clu            = r$clu,
					   net_vertices   = r$net_vertices,
					   clu_length     = length(part),
					   n_communities  = length(unique(part)),
					   lengths_agree  = isTRUE(r$net_vertices == length(part)),
					   net_bytes      = file.size(r$net),
					   clu_bytes      = file.size(r$clu),
					   stringsAsFactors = FALSE)
		})

	#	Assembling Result
		out <- do.call("rbind", rows)
		rownames(out) <- NULL
		return(out)
}

#' @title pa_manifest_warnings
#' @description Report anything amiss in a manifest, without stopping the run.
#'
#' @param manifest The data.frame returned by pa_era_manifest.
#'
#' @details
#' The September version warned that partition naming was not uniform and that the
#' partitions were development fixtures pending CHAMP at a pooled gamma star. Both
#' are obsolete: there is one naming convention, and no pooled gamma is selected
#' (S14.8). What matters now is that every era came from the same specification,
#' that every partition matches its network in length, and that nothing is empty.
#'
#' @return A character vector of warnings, empty when nothing is amiss.
#'
#' @export
pa_manifest_warnings <- function(manifest) {
	#	"""
	#	Args:
	#		manifest: output of pa_era_manifest()
	#	Returns:
	#		character vector of warnings, empty when clean
	#	"""

	#	Collect
		msgs <- character(0)

	#	One specification across the run
		tags <- unique(manifest$spec_tag)
		if (length(tags) > 1L) {
			msgs <- c(msgs, paste0(
				"Partitions come from more than one specification: ",
				paste(tags, collapse = ", "),
				". Community granularity is not comparable across specifications; ",
				"a run should use one."))
		}
		grids <- unique(manifest$grid)
		if (length(grids) > 1L) {
			msgs <- c(msgs, paste0(
				"Partitions come from more than one sweep grid: ",
				paste(grids, collapse = ", "), "."))
		}

	#	Partition length against the network
		bad <- manifest[!is.na(manifest$net_vertices) & !manifest$lengths_agree, ,
						drop = FALSE]
		if (nrow(bad) > 0L) {
			msgs <- c(msgs, paste0(
				"Partition length does not match the network for era(s) ",
				paste(sprintf("%d (%d vertices, %d assignments)", bad$era,
							  bad$net_vertices, bad$clu_length), collapse = "; "),
				". Community assignments would be misaligned."))
		}

	#	Empty inputs
		empty <- manifest[manifest$net_bytes == 0L | manifest$clu_bytes == 0L, ,
						  drop = FALSE]
		if (nrow(empty) > 0L) {
			msgs <- c(msgs, paste0("Zero-byte input for era(s) ",
								   paste(empty$era, collapse = ", "), "."))
		}

	#	Assembling Result
		return(msgs)
}

#' @title pa_era_map
#' @description Map one era's network vertices onto the node list and its community assignments.
#'
#' @param node_list The rebuilt citation node list, carrying node_id, node_key, era, is_article and is_cited.
#' @param vertices Vertex table from pa_read_net.
#' @param partition Integer vector from pa_read_clu.
#' @param era Integer era number.
#' @param legacy_names Emit node_label as an alias of node_key, and role derived from the flags. Default TRUE.
#'
#' @details
#' THE JOIN KEY IS node_id, NOT node_key. The Label column of a Pajek vertex line
#' in this project holds the node_id as a quoted string, not the DOI or
#' author_year_journal string:
#'
#' \preformatted{
#'   *Vertices 2629
#'        1 "8112"    0.0000 0.0000 0.5000 ic Blue bc White
#' }
#'
#' The rebuilt construction writes the label the same way the 2024 files did, so
#' this holds across both. An earlier draft misread it as the canonical key and
#' failed on every era; the validation below names that failure explicitly.
#'
#' The rebuilt node list renamed two columns: node_key where the 2024 list had
#' node_label, and the flags is_article and is_cited where it had role. Both new
#' names are read here. With legacy_names TRUE, role is derived from the flags and
#' node_label is emitted as an alias of node_key, so PathA_TieLayer.R runs
#' unmodified; the alias is deprecated and should be dropped once that file is
#' revised.
#'
#' Vertices are joined to the node list rather than the reverse, so an unmappable
#' vertex is an error rather than a quiet row of NA. Vertices carry the community
#' assignments, and a vertex we cannot identify is a community we cannot attribute.
#' The two vertex sets are compared in both directions and the result returned as
#' the attribute "vertex_set", so the input test of S14.4 can assert on it.
#'
#' @return A data.frame with pajek_id, node_id, node_key, community, era,
#'   is_article, is_cited, and under legacy_names also node_label and role. Carries
#'   the attribute "vertex_set".
#'
#' @export
pa_era_map <- function(node_list, vertices, partition, era, legacy_names = TRUE) {
	#	"""
	#	Args:
	#		node_list: rebuilt citation node list
	#		vertices: vertex table from pa_read_net()
	#		partition: integer vector from pa_read_clu()
	#		era: integer era number
	#		legacy_names: emit node_label and role for the unrevised tie layer
	#	Returns:
	#		data.frame with attribute "vertex_set"
	#	Notes:
	#		The node-list check distinguishes the rebuilt list from the 2024 one by
	#		name, because both are on disk and handing over the wrong one would
	#		otherwise fail deep inside a join.
	#	"""

	#	Validation
		if (nrow(vertices) != length(partition)) {
			stop("pa_era_map: era ", era, ": ", nrow(vertices), " vertices but ",
				 length(partition), " assignments")
		}
		need <- c("node_id", "node_key", "era", "is_article", "is_cited")
		miss <- setdiff(need, names(node_list))
		if (length(miss) > 0L) {
			extra <- if (all(c("node_label", "role") %in% names(node_list))) {
						 " This looks like the 2024 node list, which carries node_label and role; the rebuilt list carries node_key, is_article and is_cited."
					 } else {
						 ""
					 }
			stop("pa_era_map: node_list is missing column(s): ",
				 paste(miss, collapse = ", "), ".", extra)
		}

	#	The Label Column Carries node_id
		raw <- as.character(vertices[["Label"]])
		node_id <- suppressWarnings(as.integer(raw))
		if (anyNA(node_id)) {
			bad <- raw[is.na(node_id)][[1L]]
			stop("pa_era_map: era ", era, ": ", sum(is.na(node_id)), " of ",
				 length(raw), " vertex labels are not integer node ids. ",
				 "First offender: \"", bad, "\". This project's .net files label ",
				 "vertices by node_id; a file labelled by DOI or author_year ",
				 "string was probably exported by a different route.")
		}

	#	Vertex Table, Named Rather than Positional
		v <- data.frame(pajek_id  = as.integer(vertices[["ID"]]),
						node_id   = node_id,
						community = as.integer(partition),
						stringsAsFactors = FALSE)

	#	Node List Side, Reduced to This Era
		nl <- as.data.frame(node_list)[node_list$era == era,
									   c("node_id", "node_key", "is_article",
										 "is_cited"), drop = FALSE]
		nl <- nl[!duplicated(nl$node_id), , drop = FALSE]

	#	Join on the Node Id
		out <- dplyr::left_join(v, nl, by = "node_id", relationship = "many-to-one")

	#	Every Vertex Must Resolve
		unmatched <- sum(is.na(out$node_key))
		if (unmatched > 0L) {
			stop("pa_era_map: era ", era, ": ", unmatched, " of ", nrow(out),
				 " network vertices carry a node id absent from era ", era,
				 " of the node list. First unmatched id: ",
				 out$node_id[is.na(out$node_key)][[1L]])
		}

	#	Compare the Two Vertex Sets in Both Directions
		absent <- length(setdiff(nl$node_id, v$node_id))
		vertex_set <- list(n_vertices                  = nrow(v),
						   n_era_nodes                 = nrow(nl),
						   n_nodes_absent_from_network = absent,
						   exact_match                 = identical(absent, 0L))

	#	Legacy Names for the Unrevised Tie Layer
		out$era <- as.integer(era)
		keep <- c("pajek_id", "node_id", "node_key", "community", "era",
				  "is_article", "is_cited")
		if (legacy_names) {
			out$node_label <- out$node_key
			out$role <- ifelse(out$is_article & out$is_cited, "both",
							   ifelse(out$is_article, "article", "cited"))
			keep <- c(keep, "node_label", "role")
		}

	#	Assembling Result
		out <- out[keep]
		attr(out, "vertex_set") <- vertex_set
		rownames(out) <- NULL
		return(out)
}
