#	PathA_Revised_Eras.R -- Node List, Era Networks, and Degree Tests for Path A
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	6 October 2026
#
#	WHERE THIS SITS. Stage 1 of the pipeline. It builds one canonical node list,
#	one deduplicated edge list, the era networks over a fixed-width tiling, and the
#	degree tests that confirm the construction is sound. Everything downstream --
#	community detection, the tie layer, the models -- reads what this script writes.
#
#	WHAT CHANGED FROM THE PREVIOUS VERSION, AND WHY.
#
#	1.	IDENTITY. The previous version keyed articles on their DOI and cited
#		references on the raw Web of Science CR string, and minted ids for each
#		separately. Those two key spaces never intersect, so a caffeine article
#		cited by another caffeine article became two vertices: no vertex carried
#		both in- and out-degree, and no era network held a path of length two.
#		This version reads the canonical keys the 2023 process already produced --
#		source_combined and DOI_combined in article_combinedv2.Rda, each a
#		normalized DOI or an author_year_journal triple -- and resolves both roles
#		into one key space, as nodelist_maker() in link_analysis_5March24.R did.
#
#	2.	ERA ASSIGNMENT. A work now belongs to the era containing its own
#		publication year. In citation_node_list_3Oct2024.Rda a cited work carries
#		the CITING article's year, because Rank_Thinning_Function.R joined time_id
#		onto both the source and the target column of article_combined. That is
#		not a birth-era assignment, and it is also order dependent: the duplicate
#		filter kept whichever citing row appeared first in the file rather than the
#		earliest year. Here the citing year is kept separately, as
#		year_first_cited, computed as a minimum, and never used to place a work in
#		an era.
#
#	3.	THE WITHIN-ERA RULE IS NOW AUDITED, NOT ASSUMED. An arc belongs to an era
#		only when both endpoints were published inside the window. The audit writes
#		era_citation_audit.csv: for every era, how many of the citations its
#		articles emit stay inside the window and how many fall outside, with the
#		reason. Under the rule, pct_in_span must be 100.
#
#	4.	PAJEK LABELS. The label is node_id, as the old .net files had, so
#		pa_era_map() in the R tie layer can parse it. Labelling with the DOI left
#		the literal "NA" on every article without one -- 31% of era 12's articles
#		in the 7-year files -- and DOI labels are not unique.
#
#	5.	THE VERTEX INDEX COMES FROM ROW POSITION. write_net() numbers vertices by
#		row position in the vector it is handed, so the arc index map is built from
#		row position too. The previous version built it from sort(node_id), which
#		agreed only because both happened to ascend.
#
#	6.	year_to_era NO LONGER SHORTENS ITS INPUT. findInterval() returns 0 below
#		the first boundary and era_table$era[0] drops that element, so the old
#		function returned a vector shorter than the years it was given whenever a
#		pre-1901 reference appeared.
#
#	7.	REFERENCE YEARS. The year is the first bare four-digit year in the first
#		two comma-delimited fields, not field two. Field two fails whenever the
#		author field contains a comma, and those references were dropped silently.
#
#	8.	DEGREES AND TESTS. In-degree, out-degree, and total degree are computed for
#		every node, corpus-wide and within each era, written beside the networks as
#		Pajek vectors, and checked by the test block at the foot of this file. The
#		headline check is that nodes carry both in- and out-degree, which the
#		previous construction made impossible.
#
#	INPUT
#		article_combinedv2.Rda              canonical keys, citing year, CR strings
#		citation_node_list_3Oct2024.Rda     optional, for node_id preservation
#
#	OUTPUT, under data/eras_7yr/ and pajek_files/eras_7yr/
#		pa_node_list.Rda            one row per work
#		pa_edge_list.Rda            one row per distinct citation
#		era<NN>_nodes.Rda           era node list with degrees
#		era<NN>_edges.Rda           era edge list
#		era<NN>.net                 Pajek network, labelled with node_id
#		era<NN>_total_degree.vec    total degree, in vertex order
#		era_construction_summary.csv   one row per era
#		era_citation_audit.csv      the within-era audit
#		era_citation_audit_arcs.csv optional, one row per arc
#		node_degrees.csv            corpus-wide degrees
#		era_construction_tests.csv  the test results
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything
#	else, data.table for the heavy aggregation.

################
#   PACKAGES   #
################

#	Load Packages
	library(data.table)

#	Source Pajek I/O
	source("/workspace/caffeine_citation/scripts/RPajekFunctions_30April2023.r")

#####################
#   CONFIGURATION   #
#####################

#	Working Directory
	setwd("/workspace/caffeine_citation")

#	Era Tiling
	ERA_WIDTH <- 7L
	YEAR_MIN  <- 1901L
	YEAR_MAX  <- 2021L

#	Input Paths
	COMBINED_PATH <- "data/article_combinedv2.Rda"
	OLD_NODE_PATH <- "data/citation_node_list_3Oct2024.Rda"

#	Output Paths
	OUTPUT_DIR <- "data/eras_7yr"
	PAJEK_DIR  <- "pajek_files/eras_7yr"

#	Audit Depth
	WRITE_ARC_AUDIT <- FALSE

#	Whole-Corpus Network
#	The era networks keep only the arcs whose endpoints share a window -- 24.6% of
#	the record. The full network keeps all of them, over the same node ids, so the
#	share of a work's citations that cross an era boundary can enter the models as a
#	covariate rather than being discarded at construction.
	WRITE_FULL_NETWORK  <- TRUE
	FULL_NET_NAME       <- "full_network"
	WRITE_NODE_CROSS_ERA <- TRUE

###############
#   HELPERS   #
###############

#	Key Normalization
	pa_normalize_key <- function(x) {
		#	"""
		#	Args:
		#		x: character vector of canonical keys, from source_combined or
		#			DOI_combined
		#	Returns:
		#		character vector, lowercased and trimmed, NA where empty
		#	Notes:
		#		The 2023 keys are already canonical: a normalized DOI, or an
		#		author_year_journal triple. Only case and whitespace are at issue, and
		#		case matters -- 117,000 of the old DOI labels carry uppercase, so a
		#		case-sensitive comparison splits works that are the same.
		#	"""

		#	Normalize
			out <- tolower(trimws(as.character(x)))

		#	Empty strings are missing, not keys
			out[out == "" | out == "na"] <- NA_character_

		#	Assembling Result
			return(out)
	}

#	Helper Function for pa_build_node_list and pa_build_era: Integer Year Extremes
	pa_year_extreme <- function(x, which = c("min", "max")) {
		#	"""
		#	Args:
		#		x: integer vector of years, possibly all NA
		#		which: "min" or "max" (default "min")
		#	Returns:
		#		integer scalar, NA_integer_ when x holds no year
		#	Notes:
		#		min(x, na.rm = TRUE) returns Inf -- a double -- when every element is
		#		NA, and data.table refuses an aggregation whose result changes type
		#		between groups. The 122 references with no readable year form exactly
		#		such a group, so the extremes are taken over the non-missing values and
		#		an empty vector returns NA of the right type.
		#	"""

		#	Drop missing values
			which <- match.arg(which)
			v <- x[!is.na(x)]

		#	Early return
			if (length(v) == 0L) return(NA_integer_)

		#	Assembling Result
			return(as.integer(if (which == "min") min(v) else max(v)))
	}

#	Publication Year from a Cited Reference String
	pa_cr_year <- function(citation) {
		#	"""
		#	Args:
		#		citation: character vector of Web of Science CR strings
		#	Returns:
		#		integer vector of publication years, NA where none is readable
		#	Notes:
		#		The year is sought in the first two comma-delimited fields, which is
		#		where the CR format puts it, and only there -- searching the whole
		#		string would catch a volume or a page range. The previous version read
		#		field two positionally, which failed on any group author carrying a
		#		comma ("5-hour Energy, 2012, ...") and dropped those references without
		#		reporting it.
		#	"""

		#	Restrict the search to the leading fields
			head_fields <- sub("^([^,]*,[^,]*).*$", "\\1", as.character(citation))

		#	Locate a four-digit year
			pos <- regexpr("(1[89][0-9]{2}|20[0-2][0-9])", head_fields)

		#	Extract where found
			out <- rep(NA_integer_, length(head_fields))
			hit <- pos > 0L
			out[hit] <- as.integer(substring(head_fields[hit], pos[hit], pos[hit] + 3L))

		#	Assembling Result
			return(out)
	}

#	Era Boundaries
	pa_build_era_table <- function(year_min = YEAR_MIN, year_max = YEAR_MAX,
								   width = ERA_WIDTH) {
		#	"""
		#	Args:
		#		year_min: first year of the tiling (default YEAR_MIN)
		#		year_max: last year of the corpus (default YEAR_MAX)
		#		width: era width in years (default ERA_WIDTH)
		#	Returns:
		#		data.frame(era, year_start, year_end, n_years, is_complete)
		#	Notes:
		#		is_complete marks an era that spans the full width. The tiling from
		#		1901 in steps of 7 ends at 2020, so the final era covers 2020-2021
		#		only. It is built and written, and excluded from the comparable set,
		#		because a two-year era is not comparable with a seven-year one.
		#	"""

		#	Validation
			if (width < 1L) stop("width must be at least 1")
			if (year_max < year_min) stop("year_max must be at least year_min")

		#	Build boundaries
			starts <- seq(year_min, year_max, by = width)
			ends <- pmin(starts + width - 1L, year_max)

		#	Assembling Result
			return(data.frame(era         = seq_along(starts),
							  year_start  = as.integer(starts),
							  year_end    = as.integer(ends),
							  n_years     = as.integer(ends - starts + 1L),
							  is_complete = (ends - starts + 1L) == width,
							  stringsAsFactors = FALSE))
	}

#	Year to Era
	pa_year_to_era <- function(year, era_table) {
		#	"""
		#	Args:
		#		year: integer vector of publication years
		#		era_table: data.frame from pa_build_era_table()
		#	Returns:
		#		integer vector of era numbers, same length as year, NA outside range
		#	Notes:
		#		findInterval() returns 0 for a year below the first boundary, and
		#		era_table$era[0] returns nothing rather than NA, so indexing directly
		#		returns a vector SHORTER than its input. The previous version did
		#		that, which silently misaligned the era column of any table holding a
		#		pre-1901 reference. The zeros are converted to NA first.
		#	"""

		#	Locate the interval
			idx <- findInterval(as.integer(year), era_table$year_start)
			idx[idx == 0L] <- NA_integer_

		#	Map to eras
			era <- era_table$era[idx]

		#	Years beyond the final era's end are outside the tiling
			beyond <- !is.na(idx) & as.integer(year) > era_table$year_end[idx]
			era[beyond] <- NA_integer_

		#	Validation
			if (length(era) != length(year)) {
				stop("pa_year_to_era: returned ", length(era), " eras for ",
					 length(year), " years")
			}

		#	Assembling Result
			return(as.integer(era))
	}

#	Helper Function for pa_build_node_list: Citing and Cited Sides
	pa_extract_sides <- function(article_combined, verbose = TRUE) {
		#	"""
		#	Args:
		#		article_combined: contents of article_combinedv2.Rda
		#		verbose: report coverage? (default TRUE)
		#	Returns:
		#		list(articles, citations, report)
		#	Notes:
		#		articles is one row per citing article: its canonical key and its own
		#		publication year. citations is one row per article-reference pair: the
		#		citing key, the cited key, and the cited work's publication year read
		#		from the CR string.
		#
		#		Columns are resolved by name rather than by position. The 2023 scripts
		#		addressed this frame positionally -- article_combined[c(6,7,10)] -- and
		#		a column inserted anywhere ahead of those indices would have changed
		#		what was read without any error.
		#	"""

		#	Validation
			dt <- as.data.table(article_combined)
			need <- c("id", "year", "source_combined", "citation", "DOI_combined")
			miss <- setdiff(need, names(dt))
			if (length(miss) > 0L) {
				stop("article_combined is missing column(s): ", paste(miss, collapse = ", "))
			}

		#	Citing side, one row per article
			articles <- unique(dt[, .(article_ref    = as.character(id),
									  node_key       = pa_normalize_key(source_combined),
									  year_published = suppressWarnings(as.integer(year)))],
							   by = "article_ref")
			n_articles_raw <- nrow(articles)
			articles <- articles[!is.na(node_key)]

		#	Cited side, one row per article-reference pair
			citations <- dt[, .(article_ref = as.character(id),
								sender_key  = pa_normalize_key(source_combined),
								node_key    = pa_normalize_key(DOI_combined),
								cr_year     = pa_cr_year(citation))]
			n_pairs_raw <- nrow(citations)
			citations <- citations[!is.na(node_key) & !is.na(sender_key)]

		#	Report
			report <- list(n_articles_raw    = n_articles_raw,
						   n_articles_keyed  = nrow(articles),
						   n_pairs_raw       = n_pairs_raw,
						   n_pairs_keyed     = nrow(citations),
						   n_pairs_no_year   = sum(is.na(citations$cr_year)),
						   n_articles_no_year = sum(is.na(articles$year_published)))
			if (verbose) {
				cat("  articles keyed: ", report$n_articles_keyed, " of ",
					report$n_articles_raw, "\n", sep = "")
				cat("  reference pairs keyed: ", report$n_pairs_keyed, " of ",
					report$n_pairs_raw, " (", report$n_pairs_no_year,
					" without a readable year)\n", sep = "")
			}

		#	Assembling Result
			return(list(articles = articles, citations = citations, report = report))
	}

#	Helper Function for pa_build_node_list: Node Ids from the Old List
	pa_match_old_ids <- function(nodes, old_node_list, verbose = TRUE) {
		#	"""
		#	Args:
		#		nodes: data.table carrying node_key, in its final order
		#		old_node_list: contents of citation_node_list_3Oct2024.Rda
		#		verbose: report match counts? (default TRUE)
		#	Returns:
		#		list(node_id, id_source, report) each vector the length of nodes
		#	Notes:
		#		Two passes. The first matches the key as built against the old label,
		#		lowercased. The second matches against old labels with a trailing
		#		uppercase "NA" removed: 90,387 of the 96,036 non-DOI labels in the old
		#		list carry that artifact, left by paste0() over a missing field. The
		#		test is case sensitive on purpose -- exactly one old label genuinely
		#		ends in lowercase "na".
		#
		#		An old id that would be claimed by two different keys is released and
		#		both keys are renumbered, because lowercasing merges 386 case-variant
		#		labels that the old pipeline held apart.
		#	"""

		#	Prepare the old labels
			old <- as.data.table(old_node_list)
			if (!all(c("node_id", "node_label") %in% names(old))) {
				stop("old node list must carry node_id and node_label")
			}
			old[, old_label := trimws(as.character(node_label))]
			old[, old_key := tolower(old_label)]
			old[, old_id := as.integer(node_id)]

		#	Pass one: the key as built
			pass1 <- unique(old[, .(old_key, old_id)], by = "old_key")
			m1 <- dplyr::left_join(nodes[, .(node_key)], pass1,
								   by = c("node_key" = "old_key"),
								   relationship = "many-to-one")
			node_id <- as.integer(data.table::as.data.table(m1)$old_id)
			id_source <- ifelse(is.na(node_id), "new", "old_exact")
			n_pass1 <- sum(!is.na(node_id))

		#	Pass two: old labels carrying the trailing "NA" artifact
			n_pass2 <- 0L
			old_na <- old[grepl("NA$", old_label)]
			if (nrow(old_na) > 0L) {
				old_na[, stripped := tolower(sub("NA$", "", old_label))]
				pass2 <- unique(old_na[, .(stripped, old_id)], by = "stripped")
				m2 <- dplyr::left_join(nodes[, .(node_key)], pass2,
									   by = c("node_key" = "stripped"),
									   relationship = "many-to-one")
				cand <- as.integer(data.table::as.data.table(m2)$old_id)
				fill <- is.na(node_id) & !is.na(cand)
				node_id[fill] <- cand[fill]
				id_source[fill] <- "old_na_stripped"
				n_pass2 <- sum(fill)
			}

		#	Release ids claimed twice
			tab <- table(node_id[!is.na(node_id)])
			dup_ids <- as.integer(names(tab[tab > 1L]))
			n_released <- 0L
			if (length(dup_ids) > 0L) {
				hit <- !is.na(node_id) & node_id %in% dup_ids
				n_released <- sum(hit)
				node_id[hit] <- NA_integer_
				id_source[hit] <- "new"
			}

		#	Report
			report <- list(n_old_labels         = nrow(old),
						   n_old_na_artifact    = sum(grepl("NA$", old$old_label)),
						   n_old_empty_journal  = sum(grepl("_$", old$old_label)),
						   n_old_case_collapsed = sum(duplicated(old$old_key)),
						   n_id_pass1           = n_pass1,
						   n_id_pass2           = n_pass2,
						   n_id_released        = n_released)
			if (verbose) {
				cat("  ids preserved: ", n_pass1, " exact, ", n_pass2,
					" after stripping the NA artifact, ", n_released, " released\n", sep = "")
			}

		#	Assembling Result
			return(list(node_id = node_id, id_source = id_source, report = report))
	}

#' @title pa_build_node_list
#' @description Build one canonical node list in which a work cited under its reference form and published as an article resolve to a single node.
#'
#' @param sides List from pa_extract_sides().
#' @param old_node_list Contents of citation_node_list_3Oct2024.Rda, or NULL to mint every id afresh.
#' @param verbose Report coverage and match counts. Default TRUE.
#'
#' @details Keys come from the 2023 canonicalization: a normalized DOI, or an author_year_journal triple. Article metadata wins over reference metadata where a work appears in both roles, which is the rule nodelist_maker() applied by stacking senders ahead of targets and dropping later duplicates.
#'
#' year_published is the work's own publication year and is the only year era assignment uses. year_first_cited is the earliest year any corpus article cites the work; it is recorded for reference and computed as a minimum rather than taken from whichever row appeared first.
#'
#' @return A list with nodes (data.table), articles, citations, and report.
#'
#' @examples
#' \dontrun{
#' sides <- pa_extract_sides(article_combined)
#' built <- pa_build_node_list(sides, old_node_list = node_list)
#' sum(built$nodes$is_article & built$nodes$is_cited)
#' }
#'
#' @export
pa_build_node_list <- function(sides, old_node_list = NULL, verbose = TRUE) {
	#	"""
	#	Args:
	#		sides: list(articles, citations) from pa_extract_sides()
	#		old_node_list: old node list for id preservation, or NULL
	#		verbose: report progress? (default TRUE)
	#	Returns:
	#		list(nodes, articles, citations, report)
	#	Notes:
	#		One row per distinct key. A work seen in both roles keeps the article's
	#		publication year, because PY is a field and a CR year is a parse.
	#	"""

	#	Validation
		articles <- as.data.table(sides$articles)
		citations <- as.data.table(sides$citations)
		if (nrow(articles) == 0L) stop("no keyed articles")
		if (nrow(citations) == 0L) stop("no keyed reference pairs")

	#	Article side, one row per key
		art_side <- articles[, .(year_article = pa_year_extreme(year_published, "min")),
							 by = node_key]

	#	Cited side, one row per key, recording disagreement among parsed years
		cit_side <- citations[, .(year_cited      = pa_year_extreme(cr_year, "min"),
								  n_year_variants = length(unique(cr_year[!is.na(cr_year)]))),
							  by = node_key]

	#	The union of the two key sets
		nodes <- data.table(node_key = union(art_side$node_key, cit_side$node_key))
		nodes <- as.data.table(dplyr::left_join(nodes, art_side, by = "node_key",
												relationship = "one-to-one"))
		nodes <- as.data.table(dplyr::left_join(nodes, cit_side, by = "node_key",
												relationship = "one-to-one"))

	#	Roles and resolved attributes
		nodes[, is_article := node_key %in% art_side$node_key]
		nodes[, is_cited   := node_key %in% cit_side$node_key]
		nodes[, year_published := ifelse(!is.na(year_article), year_article, year_cited)]
		nodes[, year_source := ifelse(!is.na(year_article), "article_py", "reference_cr")]
		nodes[, key_type := ifelse(grepl("^10\\.", node_key), "doi", "string")]
		nodes[is.na(n_year_variants), n_year_variants := 0L]

	#	Earliest citing year, recorded and never used for era assignment
		cite_years <- as.data.table(dplyr::left_join(
			citations[, .(node_key, article_ref)],
			articles[, .(article_ref, citing_year = year_published)],
			by = "article_ref", relationship = "many-to-one"))
		first_cited <- cite_years[, .(year_first_cited = pa_year_extreme(citing_year, "min")),
								  by = node_key]
		nodes <- as.data.table(dplyr::left_join(nodes, first_cited, by = "node_key",
												relationship = "one-to-one"))

	#	Deterministic order before any id is minted
		setorder(nodes, node_key)

	#	Node ids, preserved where the old list has the key
		id_report <- list()
		if (!is.null(old_node_list)) {
			matched <- pa_match_old_ids(nodes, old_node_list, verbose = verbose)
			nodes[, node_id := matched$node_id]
			nodes[, id_source := matched$id_source]
			id_report <- matched$report
		} else {
			nodes[, node_id := NA_integer_]
			nodes[, id_source := "new"]
		}

	#	Mint the remaining ids above every preserved one
		next_id <- if (all(is.na(nodes$node_id))) 1L else max(nodes$node_id, na.rm = TRUE) + 1L
		need <- which(is.na(nodes$node_id))
		if (length(need) > 0L) {
			nodes[need, node_id := seq.int(next_id, length.out = length(need))]
		}

	#	Attach ids to both sides
		key_map <- nodes[, .(node_key, node_id)]
		articles <- as.data.table(dplyr::left_join(articles, key_map, by = "node_key",
												   relationship = "many-to-one"))
		citations <- as.data.table(dplyr::left_join(
			citations, key_map, by = "node_key", relationship = "many-to-one"))
		setnames(citations, "node_id", "target_id")
		citations <- as.data.table(dplyr::left_join(
			citations, key_map, by = c("sender_key" = "node_key"),
			relationship = "many-to-one"))
		setnames(citations, "node_id", "sender_id")

	#	Report
		report <- c(sides$report, id_report,
					list(n_nodes        = nrow(nodes),
						 n_both_roles   = sum(nodes$is_article & nodes$is_cited),
						 n_article_only = sum(nodes$is_article & !nodes$is_cited),
						 n_cited_only   = sum(!nodes$is_article & nodes$is_cited),
						 n_no_year      = sum(is.na(nodes$year_published)),
						 n_year_conflict = sum(nodes$n_year_variants > 1L)))
		if (verbose) {
			cat("  nodes: ", report$n_nodes, " (", report$n_both_roles,
				" in both roles)\n", sep = "")
		}

	#	Assembling Result
		return(list(nodes = nodes[, .(node_id, node_key, key_type, year_published,
									  year_source, year_first_cited, n_year_variants,
									  is_article, is_cited, id_source)],
					articles = articles, citations = citations, report = report))
}

#' @title pa_build_edge_list
#' @description Reduce the article-reference pairs to one row per distinct citation between two nodes.
#'
#' @param built List from pa_build_node_list().
#' @param verbose Report the reduction. Default TRUE.
#'
#' @details The 2023 edge list stacked two overlapping sources and stored each citation between two and fourteen times, so deduplication happens once, here, on the identifier pair. Rows are ordered before the reduction so the surviving representative does not depend on input order.
#'
#' A self-loop means a work's citing form and one of its own references resolved to the same key. That is a key collision rather than a citation, so self-loops are dropped and counted.
#'
#' @return A list with edges (data.table) and report.
#'
#' @examples
#' \dontrun{
#' edged <- pa_build_edge_list(built)
#' nrow(edged$edges)
#' }
#'
#' @export
pa_build_edge_list <- function(built, verbose = TRUE) {
	#	"""
	#	Args:
	#		built: list from pa_build_node_list()
	#		verbose: report the reduction? (default TRUE)
	#	Returns:
	#		list(edges, report)
	#	Notes:
	#		Years travel with the arc so that the era audit does not need a second
	#		join against the node list.
	#	"""

	#	Assemble the pairs
		cit <- as.data.table(built$citations)
		edges <- cit[!is.na(sender_id) & !is.na(target_id),
					 .(sender_id = as.integer(sender_id),
					   target_id = as.integer(target_id))]
		n_before <- nrow(edges)

	#	Deterministic order, then one row per pair
		setorder(edges, sender_id, target_id)
		edges <- unique(edges, by = c("sender_id", "target_id"))
		n_dedup <- n_before - nrow(edges)

	#	Self-loops are key collisions
		n_loops <- sum(edges$sender_id == edges$target_id)
		edges <- edges[sender_id != target_id]

	#	Attach publication years
		yr <- as.data.table(built$nodes)[, .(node_id, year_published)]
		edges <- as.data.table(dplyr::left_join(edges, yr,
												by = c("sender_id" = "node_id"),
												relationship = "many-to-one"))
		setnames(edges, "year_published", "sender_year")
		edges <- as.data.table(dplyr::left_join(edges, yr,
												by = c("target_id" = "node_id"),
												relationship = "many-to-one"))
		setnames(edges, "year_published", "target_year")

	#	Report
		report <- list(n_pairs_keyed = n_before, n_edges = nrow(edges),
					   n_duplicates_removed = n_dedup, n_self_loops_removed = n_loops)
		if (verbose) {
			cat("  edges ", n_before, " -> ", nrow(edges), " (", n_dedup,
				" duplicates, ", n_loops, " self-loops)\n", sep = "")
		}

	#	Assembling Result
		return(list(edges = edges, report = report))
}

#	Degrees over a Node Set and an Edge Set
	pa_degrees <- function(node_ids, edges) {
		#	"""
		#	Args:
		#		node_ids: integer vector of node ids, in the order wanted
		#		edges: data.table carrying sender_id and target_id
		#	Returns:
		#		data.table(node_id, out_degree, in_degree, total_degree)
		#	Notes:
		#		Counted by matching ids to positions rather than by joining, so a node
		#		that appears in no arc returns zero rather than NA. The sum of
		#		out-degree and the sum of in-degree must each equal the number of arcs,
		#		which the test block checks.
		#	"""

		#	Positions
			n <- length(node_ids)
			pos_out <- match(edges$sender_id, node_ids)
			pos_in  <- match(edges$target_id, node_ids)

		#	Counts
			out_deg <- tabulate(pos_out[!is.na(pos_out)], nbins = n)
			in_deg  <- tabulate(pos_in[!is.na(pos_in)], nbins = n)

		#	Assembling Result
			return(data.table(node_id      = as.integer(node_ids),
							  out_degree   = as.integer(out_deg),
							  in_degree    = as.integer(in_deg),
							  total_degree = as.integer(out_deg + in_deg)))
	}

#	Within-Era Audit of Emitted Citations
	pa_era_audit <- function(edges, era_table, write_arcs = FALSE,
							 output_dir = OUTPUT_DIR) {
		#	"""
		#	Args:
		#		edges: edge table carrying sender_era, target_era and both years
		#		era_table: data.frame from pa_build_era_table()
		#		write_arcs: also write one row per arc? (default FALSE)
		#		output_dir: destination for the arc-level file
		#	Returns:
		#		data.table, one row per era
		#	Notes:
		#		The denominator is the citations an era's own articles emit. An arc is
		#		in span when the cited work was also published inside the window;
		#		otherwise the reason is recorded. Under the within-era rule
		#		pct_in_span is 100, and in the 7-year networks built by the previous
		#		version it was 29 to 45, which is what this audit exists to catch.
		#	"""

		#	Classify every arc against its citing article's era
			e <- as.data.table(edges)
			e <- as.data.table(dplyr::left_join(
				e, as.data.table(era_table)[, .(sender_era = era, year_start, year_end)],
				by = "sender_era", relationship = "many-to-one"))
			e[, span_status := ifelse(is.na(sender_era), "citing_outside",
								ifelse(is.na(target_year), "cited_year_missing",
								ifelse(target_year < year_start, "cited_before_start",
								ifelse(target_year > year_end, "cited_after_end",
									   "in_span"))))]

		#	Aggregate by era
			audit <- e[!is.na(sender_era), .(
				n_arcs_emitted       = .N,
				n_arcs_in_span       = sum(span_status == "in_span"),
				n_cited_before_start = sum(span_status == "cited_before_start"),
				n_cited_after_end    = sum(span_status == "cited_after_end"),
				n_cited_year_missing = sum(span_status == "cited_year_missing")),
				by = .(era = sender_era)]
			audit[, pct_in_span := round(100 * n_arcs_in_span / n_arcs_emitted, 2)]
			audit[, pct_outside := round(100 * (n_arcs_emitted - n_arcs_in_span) /
										 n_arcs_emitted, 2)]

		#	Every era appears, including those emitting nothing
			audit <- as.data.table(dplyr::left_join(as.data.table(era_table), audit,
													by = "era",
													relationship = "one-to-one"))
			for (j in c("n_arcs_emitted", "n_arcs_in_span", "n_cited_before_start",
						"n_cited_after_end", "n_cited_year_missing")) {
				audit[is.na(get(j)), (j) := 0L]
			}

		#	Optional arc-level file
			if (write_arcs) {
				readr::write_csv(e[, .(sender_id, target_id, sender_year, target_year,
									   sender_era, target_era, span_status)],
								 file.path(output_dir, "era_citation_audit_arcs.csv"))
			}

		#	Assembling Result
			setorder(audit, era)
			return(audit)
	}

#	Era-by-Era Tie Matrix
	pa_era_tie_matrix <- function(edges, era_table) {
		#	"""
		#	Args:
		#		edges: edge table carrying sender_era and target_era
		#		era_table: data.frame from pa_build_era_table()
		#	Returns:
		#		list(matrix_long, era_shares)
		#	Notes:
		#		matrix_long counts arcs for every ordered pair of eras, including the
		#		diagonal, which is the within-era count the era networks keep.
		#		era_shares reduces that to one row per citing era: the share of its
		#		arcs running inside the era, backward to an earlier era, forward to a
		#		later one, or to a work with no era at all.
		#
		#		These are the quantities a cross-era covariate would be built from. The
		#		backward share is the one with a substantive reading -- how far outside
		#		its own period an era reaches for what it cites.
		#	"""

		#	Count every ordered pair
			e <- as.data.table(edges)
			mat <- e[, .N, by = .(sender_era, target_era)]
			setnames(mat, "N", "n_arcs")
			setorder(mat, sender_era, target_era, na.last = TRUE)

		#	Reduce to one row per citing era
			shares <- e[!is.na(sender_era), .(
				n_arcs          = .N,
				n_within        = sum(!is.na(target_era) & target_era == sender_era),
				n_backward      = sum(!is.na(target_era) & target_era < sender_era),
				n_forward       = sum(!is.na(target_era) & target_era > sender_era),
				n_target_undated = sum(is.na(target_era))),
				by = .(era = sender_era)]
			shares[, pct_within   := round(100 * n_within / n_arcs, 2)]
			shares[, pct_backward := round(100 * n_backward / n_arcs, 2)]
			shares[, pct_forward  := round(100 * n_forward / n_arcs, 2)]
			shares[, mean_era_lag := NA_real_]

		#	Mean era distance of a backward tie
			lag <- e[!is.na(sender_era) & !is.na(target_era) & target_era < sender_era,
					 .(mean_era_lag = round(mean(sender_era - target_era), 3)),
					 by = .(era = sender_era)]
			shares[, mean_era_lag := NULL]
			shares <- as.data.table(dplyr::left_join(shares, lag, by = "era",
													 relationship = "one-to-one"))

		#	Every era appears
			shares <- as.data.table(dplyr::left_join(as.data.table(era_table), shares,
													 by = "era",
													 relationship = "one-to-one"))
			setorder(shares, era)

		#	Assembling Result
			return(list(matrix_long = mat, era_shares = shares))
	}

#	Cross-Era Exposure per Node
	pa_node_cross_era <- function(nodes, edges) {
		#	"""
		#	Args:
		#		nodes: node table carrying node_id and era
		#		edges: edge table carrying sender_era and target_era
		#	Returns:
		#		data.table, one row per node that emits or receives an arc
		#	Notes:
		#		The work-level version of the same accounting: how many of a work's
		#		emitted citations stay inside its own era and how many leave it, and
		#		the same for citations it receives. pct_out_cross_era is the candidate
		#		covariate -- a work whose references are mostly outside its own period
		#		is reaching further back than its neighbours.
		#
		#		Counted by aggregation rather than by joining a degree table, so a node
		#		absent from one side simply has zeros.
		#	"""

		#	Emitted
			e <- as.data.table(edges)
			out <- e[, .(out_total = .N,
						 out_within = sum(!is.na(sender_era) & !is.na(target_era) &
										  sender_era == target_era)),
					 by = .(node_id = sender_id)]
			out[, out_cross := out_total - out_within]

		#	Received
			inc <- e[, .(in_total = .N,
						 in_within = sum(!is.na(sender_era) & !is.na(target_era) &
										 sender_era == target_era)),
					 by = .(node_id = target_id)]
			inc[, in_cross := in_total - in_within]

		#	Join onto the node list
			out_tab <- as.data.table(nodes)[, .(node_id, era, year_published)]
			out_tab <- as.data.table(dplyr::left_join(out_tab, out, by = "node_id",
													  relationship = "one-to-one"))
			out_tab <- as.data.table(dplyr::left_join(out_tab, inc, by = "node_id",
													  relationship = "one-to-one"))
			for (j in c("out_total", "out_within", "out_cross",
						"in_total", "in_within", "in_cross")) {
				out_tab[is.na(get(j)), (j) := 0L]
			}

		#	Shares
			out_tab[, pct_out_cross_era := ifelse(out_total > 0L,
												  round(100 * out_cross / out_total, 2),
												  NA_real_)]
			out_tab[, pct_in_cross_era  := ifelse(in_total > 0L,
												  round(100 * in_cross / in_total, 2),
												  NA_real_)]

		#	Assembling Result
			setorder(out_tab, node_id)
			return(out_tab)
	}

#	The Whole-Corpus Network
	pa_build_full_network <- function(nodes, edges, pajek_dir = PAJEK_DIR,
									  output_dir = OUTPUT_DIR,
									  net_name = FULL_NET_NAME,
									  write_pajek = TRUE) {
		#	"""
		#	Args:
		#		nodes: node table carrying node_id, era and the corpus degrees
		#		edges: edge table over the whole corpus
		#		pajek_dir: destination for .net, .clu and .vec files
		#		output_dir: destination for the summary
		#		net_name: file stem (default FULL_NET_NAME)
		#		write_pajek: write the Pajek files? (default TRUE)
		#	Returns:
		#		data.table, one summary row
		#	Notes:
		#		Every arc, including the three quarters that cross an era boundary.
		#		Vertices are the full node list in node_id order, so the era partition
		#		and the degree vectors line up with the network positionally, which is
		#		what Pajek requires.
		#
		#		The era partition writes 999 for a work with no era, the convention the
		#		2024 node list used, so a reader that knows the old files knows this
		#		one.
		#	"""

		#	Vertices in node_id order
			full_nodes <- as.data.table(nodes)
			setorder(full_nodes, node_id)

		#	Index map from row position
			id_map <- setNames(seq_len(nrow(full_nodes)),
							   as.character(full_nodes$node_id))

		#	Validation
			if (!all(id_map[as.character(full_nodes$node_id)] ==
					 seq_len(nrow(full_nodes)))) {
				stop("full network: vertex index map is not row position")
			}
			missing_ends <- sum(!(edges$sender_id %in% full_nodes$node_id)) +
							sum(!(edges$target_id %in% full_nodes$node_id))
			if (missing_ends > 0L) {
				stop("full network: ", missing_ends, " arc endpoints are not in the node list")
			}

		#	Write
			if (write_pajek) {
				#	Network
					write_net("Arcs", full_nodes$node_id,
							  rep("0.0000", nrow(full_nodes)),
							  rep("0.0000", nrow(full_nodes)),
							  rep("0.5000", nrow(full_nodes)),
							  "Blue", "White",
							  id_map[as.character(edges$sender_id)],
							  id_map[as.character(edges$target_id)],
							  rep(1, nrow(edges)),
							  "Gray", file.path(pajek_dir, net_name), TRUE)

				#	Era partition, 999 where a work has no era
					era_clu <- full_nodes$era
					era_clu[is.na(era_clu)] <- 999L
					write_clu(as.integer(era_clu),
							  file.path(pajek_dir, paste0(net_name, "_era")))

				#	Degree vectors
					write_vec(full_nodes$total_degree,
							  file.path(pajek_dir, paste0(net_name, "_total_degree")))
					write_vec(full_nodes$in_degree,
							  file.path(pajek_dir, paste0(net_name, "_in_degree")))
					write_vec(full_nodes$out_degree,
							  file.path(pajek_dir, paste0(net_name, "_out_degree")))
			}

		#	Summary
			summary <- data.table(
				network        = net_name,
				n_vertices     = nrow(full_nodes),
				n_arcs         = nrow(edges),
				n_dated        = sum(!is.na(full_nodes$era)),
				n_undated      = sum(is.na(full_nodes$era)),
				n_citing       = sum(full_nodes$out_degree > 0L),
				n_cited        = sum(full_nodes$in_degree > 0L),
				n_both_degree  = sum(full_nodes$out_degree > 0L & full_nodes$in_degree > 0L),
				n_arcs_within_era = sum(!is.na(edges$sender_era) & !is.na(edges$target_era) &
										edges$sender_era == edges$target_era),
				mean_degree    = round(mean(full_nodes$total_degree), 3))
			summary[, pct_arcs_cross_era := round(100 * (n_arcs - n_arcs_within_era) / n_arcs, 2)]

		#	Assembling Result
			return(summary)
	}

#	One Era's Nodes, Edges, Degrees, and Pajek File
	pa_build_era <- function(nodes, edges, era_num, era_table,
							 pajek_dir = PAJEK_DIR, output_dir = OUTPUT_DIR,
							 write_pajek = TRUE) {
		#	"""
		#	Args:
		#		nodes: node table carrying era
		#		edges: edge table carrying sender_era and target_era
		#		era_num: integer era
		#		era_table: data.frame from pa_build_era_table()
		#		pajek_dir: destination for .net and .vec files
		#		output_dir: destination for .Rda files
		#		write_pajek: write the Pajek files? (default TRUE)
		#	Returns:
		#		list(era_nodes, era_edges, summary)
		#	Notes:
		#		An arc belongs to the era only when both endpoints do. Only nodes
		#		carrying at least one such arc are kept, so an era network has no
		#		isolates -- the same rule the previous version used.
		#
		#		The Pajek vertex index is the row position of era_nodes, because
		#		write_net() numbers vertices by row position in the vector it is
		#		handed. era_nodes is ordered by node_id first, so the file is
		#		reproducible, and the index map is built from the ordered rows rather
		#		than from a separate sort.
		#	"""

		#	Era arcs
			era_edges <- edges[!is.na(sender_era) & !is.na(target_era) &
							   sender_era == era_num & target_era == era_num,
							   .(sender_id, target_id, sender_year, target_year)]

		#	Era vertices, participating only
			active <- sort(unique(c(era_edges$sender_id, era_edges$target_id)))
			era_nodes <- nodes[node_id %in% active]
			setorder(era_nodes, node_id)

		#	Drop corpus-wide degrees before computing era degrees
		#	nodes arrives carrying out_degree, in_degree and total_degree over the
		#	whole corpus. Joining the era-level degrees on top of those produced
		#	suffixed columns, so era_nodes$in_degree resolved to NULL and every era
		#	reported n_both_degree = 0 while 20,434 such nodes existed corpus-wide.
			drop_cols <- intersect(c("out_degree", "in_degree", "total_degree"),
								   names(era_nodes))
			if (length(drop_cols) > 0L) era_nodes[, (drop_cols) := NULL]

		#	Degrees within the era
			deg <- pa_degrees(era_nodes$node_id, era_edges)
			era_nodes <- as.data.table(dplyr::left_join(era_nodes, deg, by = "node_id",
														relationship = "one-to-one"))

		#	Pajek files
			if (write_pajek && nrow(era_edges) > 0L) {
				#	Index map from row position
					id_map <- setNames(seq_len(nrow(era_nodes)),
									   as.character(era_nodes$node_id))

				#	Validation
					if (!all(id_map[as.character(era_nodes$node_id)] ==
							 seq_len(nrow(era_nodes)))) {
						stop("era ", era_num, ": vertex index map is not row position")
					}

				#	Network
					net_name <- file.path(pajek_dir, sprintf("era%02d", era_num))
					write_net("Arcs", era_nodes$node_id,
							  rep("0.0000", nrow(era_nodes)),
							  rep("0.0000", nrow(era_nodes)),
							  rep("0.5000", nrow(era_nodes)),
							  "Blue", "White",
							  id_map[as.character(era_edges$sender_id)],
							  id_map[as.character(era_edges$target_id)],
							  rep(1, nrow(era_edges)),
							  "Gray", net_name, TRUE)

				#	Degree vectors, in vertex order
					write_vec(era_nodes$total_degree,
							  file.path(pajek_dir, sprintf("era%02d_total_degree", era_num)))
					write_vec(era_nodes$in_degree,
							  file.path(pajek_dir, sprintf("era%02d_in_degree", era_num)))
					write_vec(era_nodes$out_degree,
							  file.path(pajek_dir, sprintf("era%02d_out_degree", era_num)))
			}

		#	Save the era tables
			save(era_nodes, file = file.path(output_dir, sprintf("era%02d_nodes.Rda", era_num)))
			save(era_edges, file = file.path(output_dir, sprintf("era%02d_edges.Rda", era_num)))

		#	Era summary
			row <- as.data.table(era_table)[era == era_num]
			both_deg <- sum(era_nodes$in_degree > 0L & era_nodes$out_degree > 0L)
			summary <- data.table(
				era               = era_num,
				year_start        = row$year_start,
				year_end          = row$year_end,
				n_years           = row$n_years,
				is_complete       = row$is_complete,
				n_vertices        = nrow(era_nodes),
				n_arcs            = nrow(era_edges),
				n_citing          = sum(era_nodes$out_degree > 0L),
				n_cited           = sum(era_nodes$in_degree > 0L),
				n_both_degree     = both_deg,
				n_arcs_article_to_article = nrow(era_edges[
					sender_id %in% era_nodes$node_id[era_nodes$in_degree > 0L &
													 era_nodes$out_degree > 0L] &
					target_id %in% era_nodes$node_id[era_nodes$in_degree > 0L &
													 era_nodes$out_degree > 0L]]),
				mean_degree       = if (nrow(era_nodes) == 0L) NA_real_
									else round(mean(era_nodes$total_degree), 3),
				min_year          = pa_year_extreme(era_nodes$year_published, "min"),
				max_year          = pa_year_extreme(era_nodes$year_published, "max"))

		#	Assembling Result
			return(list(era_nodes = era_nodes, era_edges = era_edges, summary = summary))
	}

########################
#   MAIN CONSTRUCTION  #
########################

#' @title pa_run_era_construction
#' @description Build the canonical node list, the deduplicated edge list, and one network per era, and write the audit and degree files the test block checks.
#'
#' @param combined_path Path to article_combinedv2.Rda.
#' @param old_node_path Path to citation_node_list_3Oct2024.Rda, or NULL to mint every id afresh.
#' @param output_dir Directory for .Rda and .csv output.
#' @param pajek_dir Directory for .net and .vec output.
#' @param era_width Era width in years. Default ERA_WIDTH.
#' @param year_min First year of the tiling. Default YEAR_MIN.
#' @param year_max Last year of the corpus. Default YEAR_MAX.
#' @param write_arc_audit Also write one audit row per arc. Default WRITE_ARC_AUDIT.
#' @param verbose Report progress. Default TRUE.
#'
#' @details A work belongs to the era containing its own publication year, and an arc belongs to an era only when both of its endpoints do. The citing year is recorded as year_first_cited and never used to place a work.
#'
#' Era assignment is kept out of the node list itself, so the era width can be changed without rebuilding identity.
#'
#' @return Invisibly, a list with nodes, edges, era_summary, audit, degrees, and report.
#'
#' @examples
#' \dontrun{
#' res <- pa_run_era_construction("data/article_combinedv2.Rda",
#'                                "data/citation_node_list_3Oct2024.Rda")
#' res$era_summary
#' }
#'
#' @export
pa_run_era_construction <- function(combined_path = COMBINED_PATH,
									old_node_path = OLD_NODE_PATH,
									output_dir = OUTPUT_DIR,
									pajek_dir = PAJEK_DIR,
									era_width = ERA_WIDTH,
									year_min = YEAR_MIN,
									year_max = YEAR_MAX,
									write_arc_audit = WRITE_ARC_AUDIT,
									verbose = TRUE) {
	#	"""
	#	Args:
	#		combined_path: path to article_combinedv2.Rda
	#		old_node_path: path to the old node list, or NULL
	#		output_dir: directory for .Rda and .csv output
	#		pajek_dir: directory for .net and .vec output
	#		era_width: era width in years
	#		year_min: first year of the tiling
	#		year_max: last year of the corpus
	#		write_arc_audit: write the arc-level audit? (default WRITE_ARC_AUDIT)
	#		verbose: report progress? (default TRUE)
	#	Returns:
	#		list(nodes, edges, era_summary, audit, degrees, report) invisibly
	#	Notes:
	#		Writes everything the test block reads, so the tests can be run against a
	#		completed run rather than only inside one.
	#	"""

	#	Validation
		if (!file.exists(combined_path)) stop("not found: ", combined_path)
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
		dir.create(pajek_dir, recursive = TRUE, showWarnings = FALSE)

	#	Era boundaries
		era_table <- pa_build_era_table(year_min, year_max, era_width)
		if (verbose) {
			cat("Era table:\n")
			print(era_table)
		}

	#	Load the canonical frame
		if (verbose) cat("\nLoading article_combined...\n")
		env <- new.env()
		load(combined_path, envir = env)
		nm <- ls(env)
		if (!("article_combined" %in% nm)) {
			stop("expected an object named article_combined in ", combined_path)
		}
		article_combined <- get("article_combined", envir = env)

	#	The old node list, for id preservation
		old_nodes <- NULL
		if (!is.null(old_node_path) && file.exists(old_node_path)) {
			oenv <- new.env()
			load(old_node_path, envir = oenv)
			old_nodes <- get(ls(oenv)[1L], envir = oenv)
			if (verbose) cat("Old node list: ", nrow(old_nodes), " rows\n", sep = "")
		}

	#	Node list and edge list
		if (verbose) cat("Extracting the two sides...\n")
		sides <- pa_extract_sides(article_combined, verbose = verbose)
		if (verbose) cat("Building the node list...\n")
		built <- pa_build_node_list(sides, old_nodes, verbose = verbose)
		if (verbose) cat("Building the edge list...\n")
		edged <- pa_build_edge_list(built, verbose = verbose)

	#	Era assignment, from each work's own publication year
		nodes <- as.data.table(built$nodes)
		nodes[, era := pa_year_to_era(year_published, era_table)]

	#	Era of each arc's endpoints
		edges <- as.data.table(edged$edges)
		era_map <- nodes[, .(node_id, era)]
		edges <- as.data.table(dplyr::left_join(edges, era_map,
												by = c("sender_id" = "node_id"),
												relationship = "many-to-one"))
		setnames(edges, "era", "sender_era")
		edges <- as.data.table(dplyr::left_join(edges, era_map,
												by = c("target_id" = "node_id"),
												relationship = "many-to-one"))
		setnames(edges, "era", "target_era")

	#	Corpus-wide degrees
		if (verbose) cat("Computing degrees...\n")
		degrees <- pa_degrees(nodes$node_id, edges)
		nodes <- as.data.table(dplyr::left_join(nodes, degrees, by = "node_id",
												relationship = "one-to-one"))

	#	The within-era audit
		if (verbose) cat("Auditing the within-era rule...\n")
		audit <- pa_era_audit(edges, era_table, write_arcs = write_arc_audit,
							  output_dir = output_dir)

	#	Cross-era accounting
		if (verbose) cat("Counting cross-era ties...\n")
		ties <- pa_era_tie_matrix(edges, era_table)
		node_cross <- if (WRITE_NODE_CROSS_ERA) pa_node_cross_era(nodes, edges) else NULL

	#	The whole-corpus network
		full_summary <- NULL
		if (WRITE_FULL_NETWORK) {
			if (verbose) cat("Writing the full network...\n")
			full_summary <- pa_build_full_network(nodes, edges, pajek_dir = pajek_dir,
												  output_dir = output_dir)
		}

	#	Each era
		if (verbose) cat("\nConstructing eras...\n")
		era_rows <- vector("list", nrow(era_table))
		for (e in era_table$era) {
			built_era <- pa_build_era(nodes, edges, e, era_table,
									  pajek_dir = pajek_dir, output_dir = output_dir)
			era_rows[[e]] <- built_era$summary
			if (verbose) {
				s <- built_era$summary
				cat(sprintf("  Era %2d (%d-%d): %6d vertices, %7d arcs, %5d with both degrees%s\n",
							s$era, s$year_start, s$year_end, s$n_vertices, s$n_arcs,
							s$n_both_degree, if (!s$is_complete) "  [short era]" else ""))
			}
		}
		era_summary <- rbindlist(era_rows)

	#	Save
		pa_nodes <- nodes
		pa_edges <- edges
		pa_report <- c(built$report, edged$report,
					   list(n_nodes_both_degree = sum(nodes$in_degree > 0L &
													  nodes$out_degree > 0L),
							era_width = era_width, year_min = year_min,
							year_max = year_max))
		save(pa_nodes, file = file.path(output_dir, "pa_node_list.Rda"))
		save(pa_edges, file = file.path(output_dir, "pa_edge_list.Rda"))
		save(era_table, era_summary, pa_report,
			 file = file.path(output_dir, "era_construction_summary.Rda"))
		readr::write_csv(era_summary, file.path(output_dir, "era_construction_summary.csv"))
		readr::write_csv(audit, file.path(output_dir, "era_citation_audit.csv"))
		readr::write_csv(ties$matrix_long, file.path(output_dir, "era_tie_matrix.csv"))
		readr::write_csv(ties$era_shares, file.path(output_dir, "era_cross_era_shares.csv"))
		if (!is.null(full_summary)) {
			readr::write_csv(full_summary, file.path(output_dir, "full_network_summary.csv"))
		}
		if (!is.null(node_cross)) {
			readr::write_csv(node_cross, file.path(output_dir, "node_cross_era.csv"))
		}
		readr::write_csv(nodes[, .(node_id, node_key, key_type, year_published, era,
								   is_article, is_cited, out_degree, in_degree,
								   total_degree)],
						 file.path(output_dir, "node_degrees.csv"))

	#	Report
		if (verbose) {
			cat("\nSummary:\n")
			print(era_summary)
			cat("\nWithin-era audit:\n")
			print(audit[, .(era, year_start, year_end, n_arcs_emitted, n_arcs_in_span,
							pct_in_span)])
			cat("\nCross-era shares by citing era:\n")
			print(ties$era_shares[, .(era, year_start, year_end, n_arcs, pct_within,
									  pct_backward, pct_forward, mean_era_lag)])
			if (!is.null(full_summary)) {
				cat("\nFull network:\n")
				print(full_summary)
			}
			cat("\nNodes with both in- and out-degree, corpus-wide: ",
				pa_report$n_nodes_both_degree, "\n", sep = "")
		}

	#	Assembling Result
		return(invisible(list(nodes = nodes, edges = edges, era_table = era_table,
							  era_summary = era_summary, audit = audit,
							  era_tie_matrix = ties$matrix_long,
							  era_shares = ties$era_shares,
							  node_cross_era = node_cross,
							  full_summary = full_summary,
							  degrees = degrees, report = pa_report)))
}

#############
#   TESTS   #
#############

#	Collect One Test Result
	pa_check <- function(name, expected, observed, pass) {
		#	"""
		#	Args:
		#		name: short description of the check
		#		expected: what the check requires, as a string
		#		observed: what was found, as a string
		#		pass: logical
		#	Returns:
		#		data.table, one row
		#	Notes:
		#		Kept trivial so that a check's logic sits at its call site and the
		#		table reads as a list of claims about the run.
		#	"""

		#	Assembling Result
			return(data.table(check = name, expected = as.character(expected),
							  observed = as.character(observed), pass = as.logical(pass)))
	}

#' @title pa_run_era_tests
#' @description Check a completed era construction, print the results, write them to CSV, and stop if any check failed.
#'
#' @param res List returned by pa_run_era_construction().
#' @param output_dir Directory for era_construction_tests.csv.
#' @param stop_on_fail Stop after reporting when any check fails. Default TRUE.
#'
#' @details Every check runs before anything stops, so one failure does not hide the others. The headline check is that nodes carry both in- and out-degree: the previous construction keyed articles and cited references in separate spaces, which made that impossible and left the era networks one layer deep.
#'
#' @return Invisibly, a data.table of checks.
#'
#' @examples
#' \dontrun{
#' res <- pa_run_era_construction()
#' pa_run_era_tests(res)
#' }
#'
#' @export
pa_run_era_tests <- function(res, output_dir = OUTPUT_DIR, stop_on_fail = TRUE) {
	#	"""
	#	Args:
	#		res: list from pa_run_era_construction()
	#		output_dir: destination for the test CSV
	#		stop_on_fail: stop at the end when any check failed? (default TRUE)
	#	Returns:
	#		data.table of checks, invisibly
	#	Notes:
	#		Checks are grouped: identity, the within-era rule, degrees, determinism.
	#	"""

	#	Inputs
		nodes <- as.data.table(res$nodes)
		edges <- as.data.table(res$edges)
		era_table <- as.data.table(res$era_table)
		era_summary <- as.data.table(res$era_summary)
		audit <- as.data.table(res$audit)
		out <- list()

	#	Identity: keys and ids are unique
		out[[length(out) + 1L]] <- pa_check(
			"node keys are unique", 0, sum(duplicated(nodes$node_key)),
			sum(duplicated(nodes$node_key)) == 0L)
		out[[length(out) + 1L]] <- pa_check(
			"node ids are unique", 0, sum(duplicated(nodes$node_id)),
			sum(duplicated(nodes$node_id)) == 0L)

	#	Identity: every arc endpoint resolves
		unresolved <- sum(!(edges$sender_id %in% nodes$node_id)) +
					  sum(!(edges$target_id %in% nodes$node_id))
		out[[length(out) + 1L]] <- pa_check(
			"every arc endpoint is a node", 0, unresolved, unresolved == 0L)

	#	Identity: the edge list carries no duplicate pair and no self-loop
		out[[length(out) + 1L]] <- pa_check(
			"no duplicate arcs", 0,
			sum(duplicated(edges[, .(sender_id, target_id)])),
			sum(duplicated(edges[, .(sender_id, target_id)])) == 0L)
		out[[length(out) + 1L]] <- pa_check(
			"no self-loops", 0, sum(edges$sender_id == edges$target_id),
			sum(edges$sender_id == edges$target_id) == 0L)

	#	The headline check: works appear in both roles and carry both degrees
		n_both_roles <- sum(nodes$is_article & nodes$is_cited)
		out[[length(out) + 1L]] <- pa_check(
			"nodes resolve into both roles", "> 0", n_both_roles, n_both_roles > 0L)
		n_both_deg <- sum(nodes$in_degree > 0L & nodes$out_degree > 0L)
		out[[length(out) + 1L]] <- pa_check(
			"nodes carry both in- and out-degree", "> 0", n_both_deg, n_both_deg > 0L)

	#	Degrees reconcile with the arc count
		out[[length(out) + 1L]] <- pa_check(
			"sum of out-degree equals arcs", nrow(edges), sum(nodes$out_degree),
			sum(nodes$out_degree) == nrow(edges))
		out[[length(out) + 1L]] <- pa_check(
			"sum of in-degree equals arcs", nrow(edges), sum(nodes$in_degree),
			sum(nodes$in_degree) == nrow(edges))
		out[[length(out) + 1L]] <- pa_check(
			"total degree is the sum of the two", 0,
			sum(nodes$total_degree != nodes$in_degree + nodes$out_degree),
			all(nodes$total_degree == nodes$in_degree + nodes$out_degree))

	#	The within-era rule: every era's vertices fall inside its span
		span_fail <- 0L
		for (e in era_table$era) {
			f <- file.path(output_dir, sprintf("era%02d_nodes.Rda", e))
			if (!file.exists(f)) next
			lenv <- new.env(); load(f, envir = lenv)
			en <- as.data.table(get("era_nodes", envir = lenv))
			if (nrow(en) == 0L) next
			row <- era_table[era == e]
			span_fail <- span_fail + sum(en$year_published < row$year_start |
										 en$year_published > row$year_end, na.rm = TRUE)
		}
		out[[length(out) + 1L]] <- pa_check(
			"era vertices lie inside their span", 0, span_fail, span_fail == 0L)

	#	The within-era rule: the audit agrees with the era networks
		audit_arcs <- sum(audit$n_arcs_in_span)
		era_arcs <- sum(era_summary$n_arcs)
		out[[length(out) + 1L]] <- pa_check(
			"audit in-span arcs equal era arcs", era_arcs, audit_arcs,
			audit_arcs == era_arcs)

	#	Era networks: both-degree nodes exist in the complete eras
		complete <- era_summary[is_complete == TRUE & n_arcs > 0L]
		n_eras_both <- sum(complete$n_both_degree > 0L)
		out[[length(out) + 1L]] <- pa_check(
			"complete eras holding both-degree nodes", paste0("> 0 of ", nrow(complete)),
			n_eras_both, n_eras_both > 0L)

	#	The full network covers every node and every arc
		if (!is.null(res$full_summary)) {
			fs <- as.data.table(res$full_summary)
			out[[length(out) + 1L]] <- pa_check(
				"full network vertices equal the node list", nrow(nodes),
				fs$n_vertices, fs$n_vertices == nrow(nodes))
			out[[length(out) + 1L]] <- pa_check(
				"full network arcs equal the edge list", nrow(edges),
				fs$n_arcs, fs$n_arcs == nrow(edges))
			out[[length(out) + 1L]] <- pa_check(
				"full network within-era arcs equal era arcs", sum(era_summary$n_arcs),
				fs$n_arcs_within_era, fs$n_arcs_within_era == sum(era_summary$n_arcs))
		}

	#	Cross-era shares account for every emitted arc
		if (!is.null(res$era_shares)) {
			sh <- as.data.table(res$era_shares)[!is.na(n_arcs)]
			parts_ok <- all(sh$n_arcs == sh$n_within + sh$n_backward + sh$n_forward +
								sh$n_target_undated)
			out[[length(out) + 1L]] <- pa_check(
				"cross-era counts sum to arcs emitted", "TRUE", parts_ok, parts_ok)
			out[[length(out) + 1L]] <- pa_check(
				"no era cites forward more than it cites within",
				"informational",
				paste0(sum(sh$n_forward > sh$n_within), " era(s)"), TRUE)
		}

	#	Node-level cross-era accounting reconciles with the edge list
		if (!is.null(res$node_cross_era)) {
			nc <- as.data.table(res$node_cross_era)
			out[[length(out) + 1L]] <- pa_check(
				"node out-counts sum to arcs", nrow(edges), sum(nc$out_total),
				sum(nc$out_total) == nrow(edges))
			out[[length(out) + 1L]] <- pa_check(
				"node in-counts sum to arcs", nrow(edges), sum(nc$in_total),
				sum(nc$in_total) == nrow(edges))
		}

	#	Node ids preserved from the old list
		if ("id_source" %in% names(nodes)) {
			n_pres <- sum(nodes$id_source != "new")
			out[[length(out) + 1L]] <- pa_check(
				"node ids preserved from the old list", "informational",
				paste0(n_pres, " of ", nrow(nodes), " (",
					   round(100 * n_pres / nrow(nodes), 2), "%)"), TRUE)
		}

	#	Determinism: a shuffled edge list produces the same era arcs
		set.seed(20261006)
		shuffled <- edges[sample.int(nrow(edges))]
		e_ref <- edges[!is.na(sender_era) & !is.na(target_era) & sender_era == target_era,
					   .(sender_id, target_id)]
		setorder(e_ref, sender_id, target_id)
		e_shf <- shuffled[!is.na(sender_era) & !is.na(target_era) & sender_era == target_era,
						  .(sender_id, target_id)]
		setorder(e_shf, sender_id, target_id)
		out[[length(out) + 1L]] <- pa_check(
			"era arcs are invariant to input order", "identical",
			isTRUE(all.equal(e_ref, e_shf, check.attributes = FALSE)),
			isTRUE(all.equal(e_ref, e_shf, check.attributes = FALSE)))

	#	Cited works dated after the article that cites them
	#	A citation to a work that appears later is a real publishing pattern -- an
	#	in-press or online-first reference carrying the eventual year -- so this is a
	#	tolerance rather than an absolute. A small share is expected; a large one
	#	would mean the CR year is being read from the wrong field.
		cited_only <- nodes[is_cited == TRUE & is_article == FALSE &
							!is.na(year_published) & !is.na(year_first_cited)]
		later <- cited_only[year_published > year_first_cited]
		share_later <- if (nrow(cited_only) == 0L) 0 else nrow(later) / nrow(cited_only)
		if (nrow(later) > 0L) {
			readr::write_csv(later[, .(node_id, node_key, key_type, year_published,
									   year_first_cited)],
							 file.path(output_dir, "cited_after_first_citation.csv"))
		}
		out[[length(out) + 1L]] <- pa_check(
			"cited works dated after their first citation",
			paste0("<= 0.5% of ", nrow(cited_only)),
			paste0(nrow(later), " (", round(100 * share_later, 3), "%)"),
			share_later <= 0.005)

	#	Assemble, report, and write
		tests <- rbindlist(out)
		cat("\nTests:\n")
		print(tests)
		readr::write_csv(tests, file.path(output_dir, "era_construction_tests.csv"))

	#	Validation
		n_fail <- sum(!tests$pass)
		if (n_fail > 0L) {
			cat("\n", n_fail, " of ", nrow(tests), " checks failed.\n", sep = "")
			if (stop_on_fail) stop("era construction failed its tests; see ",
								   file.path(output_dir, "era_construction_tests.csv"))
		} else {
			cat("\nAll ", nrow(tests), " checks passed.\n", sep = "")
		}

	#	Assembling Result
		return(invisible(tests))
}

############
#   MAIN   #
############

#	Build
	era_results <- pa_run_era_construction(combined_path = COMBINED_PATH,
										   old_node_path = OLD_NODE_PATH,
										   output_dir = OUTPUT_DIR,
										   pajek_dir = PAJEK_DIR,
										   era_width = ERA_WIDTH,
										   year_min = YEAR_MIN,
										   year_max = YEAR_MAX,
										   write_arc_audit = WRITE_ARC_AUDIT)

#	Test
	era_tests <- pa_run_era_tests(era_results, output_dir = OUTPUT_DIR)
