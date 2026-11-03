#	PathA_NodeMetadata.R -- Article Metadata Keyed to node_id
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	3 November 2026

#	WHY THIS EXISTS. Five of the six outstanding Model I covariates need article
#	metadata that the rebuilt node list does not carry. pa_node_list.Rda holds
#	identifiers, years, degrees and era, and nothing else: no author, journal, title,
#	abstract or keyword. So venue overlap, disciplinary distance, both cognitive
#	measures and topical proximity all wait on one artifact, and this builds it --
#	node_id joined to whatever metadata the corpus can supply, with a coverage report
#	saying how much of each field actually arrives.
#
#	THREE SOURCES, IN PRECEDENCE ORDER.
#
#		1.	THE WEB OF SCIENCE DOWNLOAD (NM_WOS_FILE). One row per downloaded
#			article, carrying the fields only it has: abstract, title, keywords,
#			full author list, full journal name. Joined on DOI. This is the only
#			source of text, so topical proximity and cognitive subject matter
#			cannot exist without it.
#
#		2.	THE PARSED REFERENCE STRINGS (NM_EDGE_FILE). article_combined is
#			edge-level, and its citation field is a Web of Science CR string:
#			"Abdolmaleky HM, 2004, SCHIZOPHR RES, V67, P53, DOI 10.1016/...".
#			First author, year and journal abbreviation parse out of it cleanly,
#			for any work that someone cited with a DOI. Used to fill journal and
#			first author where the download does not reach.
#
#		3.	THE LEGACY NODE LIST (NM_LEGACY_FILE). citation_node_list_3Oct2024.Rda
#			carries author_id, which the rebuilt list dropped. It matches the
#			rebuilt keys at 100% over 589,784 nodes, and 322,712 distinct author
#			ids make it usable for author overlap on its own \meas{}.

#	WHY THE DOWNLOAD IS NOT OPTIONAL, AND WHAT THE REFERENCE STRINGS ACTUALLY MISS.
#	To be an article in this corpus is to be a record in the Web of Science download,
#	and a journal article record carries its journal in SO. So an article's journal is
#	never missing from the download; it is only missing from the reference-string
#	route, which can see a work only if somebody cited it with a DOI. Recent articles
#	have not been cited yet, so they have no reference string to parse.
#
#	That is what the following measures. It is the recoverability of a journal from
#	the reference strings alone, not the article's possession of one \meas{}:
#
#		era     articles reached by CR     cited-only works reached by CR
#		11              56.5%                           68.0%
#		12              57.7%                           67.6%
#		13              61.1%                           68.9%
#		14              57.2%                           73.7%
#		15              64.1%                           87.1%
#		16              69.0%                           94.4%
#		17              57.9%                           95.5%
#
#	Two conclusions follow, and they are different for the two populations.

#	For ARTICLES the download closes the gap. Journal, authors, title and abstract all
#	arrive per record, so a community measure computed on its articles has no coverage
#	problem. The residue is the small share of records that are books or series items
#	rather than journal articles -- 2.2% of the edge table's rows are PT B or PT S
#	\meas{} -- and those genuinely have no journal.
#
#	For CITED-ONLY WORKS the gap does not close, because most of them were never in
#	the download. Their journals come only from reference strings, at 68.0% rising to
#	95.5% across the window. A measure computed over ALL of a community's members
#	therefore carries an era gradient of its own into models that have era terms.
#
#	So the rule this table exists to support: compute journal-based covariates on a
#	community's ARTICLES, which is also the population granularity is measured on, and
#	not over its full membership. The coverage report splits the two for exactly this
#	reason, and the journal_cr column is kept beside journal so the difference between
#	what the download supplied and what the reference strings filled stays visible.
#
#	WHAT THIS FILE DOES NOT DO. It builds no covariate. Author overlap, venue
#	overlap, disciplinary distance and the cognitive measures are community-level
#	quantities and belong in their own lane; this one stops at the node.
#
#	SCHEMA DISCOVERY. The download's column names are not assumed. Each field is
#	resolved against a list of candidates -- the Web of Science two-letter tags and
#	the obvious renamings -- and the run reports which name matched and which fields
#	were not found at all. A field that cannot be resolved is omitted and reported,
#	not guessed at.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, data.table for the heavy
#	computation, base R for everything else.

#################
#   LIBRARIES   #
#################

#	Libraries
	library(data.table)

#####################
#   CONFIGURATION   #
#####################

#	Project Root
	NM_ROOT <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")

#	Inputs
	NM_NODE_FILE   <- file.path(NM_ROOT, "data/eras_7yr/pa_node_list.Rda")
	NM_WOS_FILE    <- file.path(NM_ROOT, "data/caffeine_articles_20April2021.Rda")
	NM_EDGE_FILE   <- file.path(NM_ROOT, "data/article_combinedv2.Rda")
	NM_LEGACY_FILE <- file.path(NM_ROOT, "data/citation_node_list_3Oct2024.Rda")

#	Output
	NM_OUT_DIR <- file.path(NM_ROOT, "data/node_metadata")

#	Analytic Eras, for the Coverage Report
	NM_ERAS <- 11:17

#	Candidate Column Names in the Download
#	First match wins, case-insensitive. Web of Science tags come first because the
#	download is a WoS export; the spelled-out names follow because an earlier
#	cleaning pass may have renamed them.
	NM_FIELDS <- list(
		doi             = c("DI", "DOI", "DOI_combined", "doi"),
		journal         = c("SO", "journal", "source", "source_title"),
		journal_abbrev  = c("J9", "JI", "journal_abbrev", "source_abbrev"),
		title           = c("TI", "title", "article_title"),
		abstract        = c("AB", "abstract"),
		keywords_author = c("DE", "author_keywords", "keywords"),
		keywords_index  = c("ID", "keywords_plus", "index_keywords"),
		authors         = c("AU", "authors", "author_full", "AF"),
		year            = c("PY", "year", "year_published"))

#	Fields That Only the Download Can Supply
#	Named so the coverage report can say which covariates are blocked when one of
#	them is missing rather than leaving the reader to work it out.
	NM_TEXT_FIELDS <- c("abstract", "keywords_author", "keywords_index", "title")

########################
#   INTERNAL HELPERS   #
########################

#	One Check Row
	pa_check_nm <- function(name, passed, detail = "") {
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

#	Load One Named Object From an Rda
	pa_load_one <- function(path, what = NULL) {
		#	"""
		#	Args:
		#		path: an .Rda file
		#		what: the object's name, or NULL to take the only object
		#	Returns:
		#		the object, as a data.frame
		#	Notes:
		#		Loads into a dedicated environment so nothing in the caller's frame
		#		is shadowed, and refuses to guess when a file holds several objects
		#		and no name was given. Coerced to data.frame because the saved
		#		objects are data.tables and several downstream helpers in this
		#		project select columns with data.frame semantics.
		#	"""

		#	Validation
			if (!file.exists(path)) {
				stop("pa_load_one: file not found: ", path)
			}

		#	Load
			env <- new.env(parent = emptyenv())
			found <- load(path, envir = env)

		#	Resolve the Object
			if (is.null(what)) {
				if (length(found) != 1L) {
					stop("pa_load_one: ", basename(path), " holds ", length(found),
						 " objects (", paste(found, collapse = ", "),
						 "); name the one you want")
				}
				what <- found[1]
			}
			if (!(what %in% found)) {
				stop("pa_load_one: ", basename(path), " has no object named ", what,
					 "; it holds ", paste(found, collapse = ", "))
			}

		#	Assembling Result
			return(as.data.frame(get(what, envir = env, inherits = FALSE)))
	}

#	Normalize a Join Key
	pa_norm_key <- function(x) {
		#	"""
		#	Args:
		#		x: a character vector of DOIs or labels
		#	Returns:
		#		character vector, lowercased with all whitespace removed
		#	Notes:
		#		Whitespace is stripped rather than trimmed because the corpus's
		#		doi-typed keys contain interior spaces -- "10. 1016/j. peh. 2013."
		#		is one real key -- so trimws alone would not align them.
		#	"""

		#	Assembling Result
			return(gsub("[[:space:]]", "", tolower(as.character(x))))
	}

#' @title pa_resolve_schema
#' @description Map each needed metadata field to a column that exists in the download.
#'
#' @param wos The Web of Science table.
#' @param fields A named list of candidate column names. Default NM_FIELDS.
#'
#' @details
#' The download's column names are not assumed anywhere in this file. Each field is
#' matched case-insensitively against its candidates and the first hit wins, so a
#' raw Web of Science export and a renamed one both resolve. A field with no match
#' is reported as NA rather than defaulted, because silently omitting a column would
#' turn a missing abstract into an empty abstract and make the coverage report lie.
#'
#' @return A data.frame with field, column and found.
#'
#' @examples
#' pa_resolve_schema(data.frame(DI = character(0), AB = character(0)))
#'
#' @export
pa_resolve_schema <- function(wos, fields = NM_FIELDS) {
	#	"""
	#	Args:
	#		wos: the Web of Science table
	#		fields: named list of candidate column names
	#	Returns:
	#		data.frame(field, column, found)
	#	"""

	#	Validation
		if (!is.data.frame(wos)) {
			stop("pa_resolve_schema: wos must be a data.frame")
		}

	#	Match Each Field, First Candidate Wins
		present <- names(wos)
		lower   <- tolower(present)
		matched <- vapply(fields, function(cands) {
			hit <- match(tolower(cands), lower)
			hit <- hit[!is.na(hit)]
			if (length(hit) == 0L) NA_character_ else present[hit[1]]
		}, character(1))

	#	Assembling Result
		return(data.frame(field  = names(fields),
						  column = unname(matched),
						  found  = !is.na(unname(matched)),
						  stringsAsFactors = FALSE))
}

#' @title pa_parse_references
#' @description Parse author, year and journal abbreviation out of the reference strings.
#'
#' @param edge_table The edge-level table holding a citation column.
#' @param citation_col Name of the reference-string column. Default "citation".
#'
#' @details
#' A Web of Science CR string is comma-delimited with the first three fields in a
#' fixed order -- first author, year, journal abbreviation -- followed by volume,
#' page and DOI tokens in varying combinations. The first three are taken
#' positionally; the DOI is taken by pattern, because it may be bare or bracketed
#' and may be absent. 84.0\% of reference strings carry a DOI \meas{}, and only
#' those can be keyed to a node.
#'
#' The DOI pattern is matched with perl = TRUE. Under the default engine a
#' backslash inside a bracket expression is literal, so a class written
#' \code{[^,\\]]} means "not comma, not backslash" and the closing bracket falls
#' outside it; the pattern then matches nothing and the function silently returns no
#' DOIs at all.
#'
#' @return A data.table with key, cr_author, cr_year and cr_journal, one row per DOI.
#'
#' @examples
#' cr <- data.frame(citation = "Smith J, 2004, J BIOL CHEM, V1, P2, DOI 10.1/x")
#' pa_parse_references(cr)
#'
#' @export
pa_parse_references <- function(edge_table, citation_col = "citation") {
	#	"""
	#	Args:
	#		edge_table: edge-level table holding the reference strings
	#		citation_col: name of the reference-string column
	#	Returns:
	#		data.table(key, cr_author, cr_year, cr_journal), unique on key
	#	"""

	#	Validation
		if (!(citation_col %in% names(edge_table))) {
			stop("pa_parse_references: no column named ", citation_col)
		}

	#	The Three Positional Fields
		cr <- data.table(citation = as.character(edge_table[[citation_col]]))
		parts <- tstrsplit(cr$citation, ", ", fixed = TRUE)
		cr[, cr_author  := trimws(parts[[1]])]
		cr[, cr_year    := suppressWarnings(as.integer(trimws(parts[[2]])))]
		cr[, cr_journal := trimws(parts[[3]])]

	#	The Reference DOI, by Pattern
		hit <- regexpr("DOI \\[?10\\.[^,\\]]+", cr$citation, perl = TRUE)
		cr[, key := NA_character_]
		cr$key[hit > 0] <- pa_norm_key(sub("^DOI \\[?", "",
										   regmatches(cr$citation, hit)))

	#	Keep the Keyed Rows With a Journal
		cr <- cr[!is.na(key) & nzchar(key) & !is.na(cr_journal) & nzchar(cr_journal)]

	#	Early return
		if (nrow(cr) == 0L) {
			return(data.table(key = character(0), cr_author = character(0),
							  cr_year = integer(0), cr_journal = character(0)))
		}

	#	Assembling Result
	#	One row per work. A work cited many times appears many times with the same
	#	parse, so the first is representative.
		out <- unique(cr[, .(key, cr_author, cr_year, cr_journal)], by = "key")
		setorder(out, key)
		return(out)
}

#' @title pa_node_metadata
#' @description Join article metadata onto the rebuilt node list.
#'
#' @param node_list The rebuilt node list.
#' @param wos The Web of Science table, or NULL to skip it.
#' @param schema Output of pa_resolve_schema, or NULL when wos is NULL.
#' @param references Output of pa_parse_references, or NULL to skip the fill.
#' @param legacy The legacy node list carrying author_id, or NULL to skip it.
#'
#' @details
#' Precedence is download, then reference strings, then legacy. The download wins
#' because it is the only source that is one row per article and the only one with
#' text; the reference parse fills journal and first author where the download does
#' not reach; the legacy list contributes author_id, which nothing else has.
#'
#' Every source column is kept under its own name alongside the resolved one --
#' journal and journal_cr, first_author and author_id -- so that a later lane can
#' tell which source a value came from and exclude the filled ones if it wants to.
#'
#' @return A data.table with one row per node and the metadata columns that resolved.
#'
#' @examples
#' meta <- pa_node_metadata(node_list, wos, schema, references, legacy)
#'
#' @export
pa_node_metadata <- function(node_list, wos = NULL, schema = NULL,
							 references = NULL, legacy = NULL) {
	#	"""
	#	Args:
	#		node_list: the rebuilt node list
	#		wos: Web of Science table, or NULL
	#		schema: output of pa_resolve_schema, or NULL
	#		references: output of pa_parse_references, or NULL
	#		legacy: legacy node list with author_id, or NULL
	#	Returns:
	#		data.table, one row per node
	#	"""

	#	Validation
		need <- c("node_id", "node_key", "is_article", "era")
		missing <- setdiff(need, names(node_list))
		if (length(missing) > 0L) {
			stop("pa_node_metadata: node list is missing: ",
				 paste(missing, collapse = ", "))
		}

	#	The Frame
		meta <- as.data.table(node_list)[, .(node_id = as.integer(node_id),
											 node_key = as.character(node_key),
											 is_article = as.logical(is_article),
											 era = as.integer(era))]
		meta[, key := pa_norm_key(node_key)]

	#	From the Download, by DOI
		if (!is.null(wos) && !is.null(schema)) {
			#	Validation
				doi_col <- schema$column[schema$field == "doi"]
				if (length(doi_col) == 0L || is.na(doi_col)) {
					stop("pa_node_metadata: the download has no resolvable doi ",
						 "column, so it cannot be joined to the node list")
				}

			#	Build the Lookup From the Resolved Columns Only
				take <- schema[schema$found & schema$field != "doi", , drop = FALSE]
				lookup <- as.data.table(wos)[, c(doi_col, take$column),
											 with = FALSE]
				setnames(lookup, c("key", take$field))
				lookup[, key := pa_norm_key(key)]
				lookup <- lookup[!is.na(key) & nzchar(key)]
				lookup <- unique(lookup, by = "key")

			#	Join
				meta[lookup, on = "key",
					 (take$field) := mget(paste0("i.", take$field))]
		}

	#	Fill Journal and First Author From the Reference Strings
		if (!is.null(references) && nrow(references) > 0L) {
			meta[references, on = "key",
				 c("journal_cr", "first_author_cr") := .(i.cr_journal, i.cr_author)]

			#	Fill Only Where the Download Did Not Reach
				if (!("journal" %in% names(meta))) meta[, journal := NA_character_]
				meta[is.na(journal) | !nzchar(journal), journal := journal_cr]
				if (!("first_author" %in% names(meta))) {
					meta[, first_author := NA_character_]
				}
				meta[is.na(first_author) | !nzchar(first_author),
					 first_author := first_author_cr]
		}

	#	Author Identifiers From the Legacy Node List
		if (!is.null(legacy)) {
			#	Validation
				if (!all(c("node_label", "author_id") %in% names(legacy))) {
					stop("pa_node_metadata: the legacy list needs node_label and ",
						 "author_id")
				}

			#	Join on the Normalized Label
				old <- as.data.table(legacy)[, .(key = pa_norm_key(node_label),
												 author_id = as.integer(author_id))]
				old <- unique(old[!is.na(key) & nzchar(key)], by = "key")
				meta[old, on = "key", author_id := i.author_id]
		}

	#	Assembling Result
		setorder(meta, node_id)
		return(meta)
}

#' @title pa_metadata_coverage
#' @description Report how much of each metadata field actually arrived, by era and role.
#'
#' @param meta Output of pa_node_metadata.
#' @param eras Integer eras to report. Default NM_ERAS.
#'
#' @details
#' Coverage is the finding, not a diagnostic. A covariate built on a field that
#' reaches 60\% of articles is measured on a subset, and if that subset's size moves
#' with era then the covariate carries an era gradient of its own into models that
#' have era terms. So coverage is reported per field, per era, and separately for
#' articles and cited-only nodes, because the community-level measures are computed
#' on articles while the partition contains both.
#'
#' @return A data.frame with field, role, era, n, present and pct.
#'
#' @examples
#' cov <- pa_metadata_coverage(meta)
#' cov[cov$field == "abstract", ]
#'
#' @export
pa_metadata_coverage <- function(meta, eras = NM_ERAS) {
	#	"""
	#	Args:
	#		meta: output of pa_node_metadata
	#		eras: integer eras to report
	#	Returns:
	#		data.frame(field, role, era, n, present, pct)
	#	"""

	#	The Fields Worth Reporting
		skip <- c("node_id", "node_key", "key", "is_article", "era")
		fields <- setdiff(names(meta), skip)

	#	Early return
		if (length(fields) == 0L) {
			return(data.frame(field = character(0), role = character(0),
							  era = integer(0), n = integer(0),
							  present = integer(0), pct = numeric(0),
							  stringsAsFactors = FALSE))
		}

	#	Iterating Over Fields, Roles and Eras
		dt <- as.data.table(meta)[era %in% as.integer(eras)]
		dt[, role := ifelse(is_article, "article", "cited")]
		rows <- list()
		for (f in fields) {
			#	Presence Is Non-Missing and Non-Empty
				dt[, .present := !is.na(get(f)) &
								 (!is.character(get(f)) | nzchar(as.character(get(f))))]

			#	Aggregate
				agg <- dt[, .(n = .N, present = sum(.present)), by = .(role, era)]
				agg[, field := f]
				agg[, pct := round(100 * present / n, 1)]
				rows[[length(rows) + 1L]] <- agg
		}

	#	Assembling Result
		out <- rbindlist(rows)
		setcolorder(out, c("field", "role", "era", "n", "present", "pct"))
		setorder(out, field, role, era)
		return(as.data.frame(out))
}

#' @title pa_run_node_metadata
#' @description Build the node metadata table and its coverage report.
#'
#' @param node_file Path to the rebuilt node list. Default NM_NODE_FILE.
#' @param wos_file Path to the Web of Science download. Default NM_WOS_FILE.
#' @param edge_file Path to the edge-level table. Default NM_EDGE_FILE.
#' @param legacy_file Path to the legacy node list. Default NM_LEGACY_FILE.
#' @param output_dir Destination. Default NM_OUT_DIR.
#' @param verbose Report progress.
#'
#' @details
#' Every source is optional except the node list. A missing download is reported and
#' the run continues on the reference strings alone, which is the honest behaviour:
#' the result is a table with no text columns and a coverage report that says so,
#' rather than a failure that leaves nothing to look at.
#'
#' @return Invisibly, a list with meta, schema, coverage and tests.
#'
#' @examples
#' res <- pa_run_node_metadata()
#' res$schema
#'
#' @export
pa_run_node_metadata <- function(node_file = NM_NODE_FILE,
								 wos_file = NM_WOS_FILE,
								 edge_file = NM_EDGE_FILE,
								 legacy_file = NM_LEGACY_FILE,
								 output_dir = NM_OUT_DIR,
								 verbose = TRUE) {
	#	"""
	#	Args:
	#		node_file: the rebuilt node list
	#		wos_file: the Web of Science download
	#		edge_file: the edge-level table
	#		legacy_file: the legacy node list
	#		output_dir: destination
	#		verbose: report progress
	#	Returns:
	#		invisible list(meta, schema, coverage, tests)
	#	"""

	#	The Node List Is Required
		node_list <- pa_load_one(node_file, "pa_nodes")
		if (verbose) cat("Node list: ", nrow(node_list), " nodes\n", sep = "")

	#	The Download, if Present
		wos <- NULL
		schema <- NULL
		if (file.exists(wos_file)) {
			wos <- pa_load_one(wos_file)
			schema <- pa_resolve_schema(wos)
			if (verbose) {
				cat("Download: ", nrow(wos), " rows, ", ncol(wos), " columns\n",
					sep = "")
				cat("\nSchema resolution\n")
				print(schema, row.names = FALSE)
				absent <- schema$field[!schema$found]
				if (length(absent) > 0L) {
					cat("\n  not found: ", paste(absent, collapse = ", "), "\n",
						sep = "")
					blocked <- intersect(absent, NM_TEXT_FIELDS)
					if (length(blocked) > 0L) {
						cat("  topical proximity and cognitive subject matter need ",
							paste(blocked, collapse = ", "),
							"; they stay blocked\n", sep = "")
					}
				}
				cat("\nAll columns in the download:\n")
				print(names(wos))
			}
		} else if (verbose) {
			cat("Download NOT FOUND at ", wos_file,
				"\n  continuing on the reference strings alone; no text columns\n",
				sep = "")
		}

	#	The Reference Strings, if Present
		references <- NULL
		if (file.exists(edge_file)) {
			edge_table <- pa_load_one(edge_file)
			references <- pa_parse_references(edge_table)
			if (verbose) {
				cat("\nReference strings: ", nrow(references),
					" works keyed by DOI with a journal\n", sep = "")
			}
			rm(edge_table)
		} else if (verbose) {
			cat("\nEdge table NOT FOUND at ", edge_file, "\n", sep = "")
		}

	#	The Legacy List, if Present
		legacy <- NULL
		if (file.exists(legacy_file)) {
			legacy <- pa_load_one(legacy_file, "node_list")
			if (verbose) {
				cat("Legacy list: ", nrow(legacy), " nodes with author_id\n",
					sep = "")
			}
		} else if (verbose) {
			cat("Legacy list NOT FOUND at ", legacy_file, "\n", sep = "")
		}

	#	Build
		if (verbose) cat("\nJoining...\n")
		meta <- pa_node_metadata(node_list, wos, schema, references, legacy)
		rm(wos)

	#	Coverage
		coverage <- pa_metadata_coverage(meta)

	#	Write
	#	The table itself goes out compressed, not as CSV. It is one row per node with
	#	a long key and, once the download is present, an abstract -- 70 MB as CSV
	#	before any text column exists. The reports stay CSV because they are small
	#	and are meant to be read. Named pa_node_metadata to match this project's
	#	convention of one named object per Rda.
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
	#	gzip, not xz. xz on 590,000 rows takes minutes for a few megabytes more --
	#	measured, after choosing it once and waiting.
		pa_node_metadata_table <- meta
		save(pa_node_metadata_table,
			 file = file.path(output_dir, "node_metadata.Rda"),
			 compress = "gzip")
		readr::write_csv(coverage, file.path(output_dir, "metadata_coverage.csv"))
		if (!is.null(schema)) {
			readr::write_csv(schema, file.path(output_dir, "metadata_schema.csv"))
		}

	#	Report
		if (verbose) {
			cat("\nCoverage, articles only, percent present\n")
			art <- coverage[coverage$role == "article", ]
			if (nrow(art) > 0L) {
				wide <- reshape(art[, c("field", "era", "pct")],
								idvar = "field", timevar = "era",
								direction = "wide")
				names(wide) <- sub("^pct\\.", "era", names(wide))
				print(wide, row.names = FALSE)
			}
		}

	#	Checks
		tests <- pa_run_node_metadata_tests(meta, node_list, coverage, schema,
											output_dir = output_dir)
		if (verbose) {
			#	Reported Rows Are Not Failures
			#	A SKIP row carries a measurement with no threshold to pass, so it
			#	must not be counted against the total -- "5 of 6 checks passed"
			#	reads as a failure when the sixth was never a test.
				cat("\n")
				print(tests, row.names = FALSE)
				n_pass <- sum(tests$result == "PASS")
				n_fail <- sum(tests$result == "FAIL")
				n_skip <- sum(tests$result == "SKIP")
				cat("\n", n_pass, " of ", n_pass + n_fail, " checks passed",
					if (n_skip > 0L) sprintf(", %d reported", n_skip) else "",
					"\n", sep = "")
		}

	#	Assembling Result
		return(invisible(list(meta = meta, schema = schema,
							  coverage = coverage, tests = tests)))
}

#' @title pa_run_node_metadata_tests
#' @description Verify the metadata table against quantities it did not use.
#'
#' @param meta Output of pa_node_metadata.
#' @param node_list The rebuilt node list.
#' @param coverage Output of pa_metadata_coverage.
#' @param schema Output of pa_resolve_schema, or NULL when the download was absent.
#' @param output_dir Destination for the test table, or NULL to skip writing.
#'
#' @details
#' The checks that matter are the ones a different code path can confirm: the join
#' must not change the row count, author_id must reach every node because the legacy
#' match was measured at 100\%, and every percentage must be a percentage. The
#' coverage spread across eras is reported rather than asserted, because there is no
#' threshold at which it becomes acceptable -- it is a fact the analysis has to state.
#'
#' @return A data.frame of checks.
#'
#' @export
pa_run_node_metadata_tests <- function(meta, node_list, coverage, schema = NULL,
									   output_dir = NM_OUT_DIR) {
	#	"""
	#	Args:
	#		meta: output of pa_node_metadata
	#		node_list: the rebuilt node list
	#		coverage: output of pa_metadata_coverage
	#		schema: output of pa_resolve_schema, or NULL
	#		output_dir: destination for the test table, or NULL
	#	Returns:
	#		data.frame of checks
	#	"""

	#	Collect
		out <- list()

	#	The Join Did Not Change the Row Count
	#	A many-to-many key collision would silently inflate the table, and every
	#	coverage figure after it would be computed on the wrong denominator.
		out[[length(out) + 1L]] <- pa_check_nm(
			"one row per node survives the join",
			nrow(meta) == nrow(node_list),
			sprintf("%d against %d", nrow(meta), nrow(node_list)))

	#	Node Ids Are Unique
		out[[length(out) + 1L]] <- pa_check_nm(
			"node_id is unique",
			!any(duplicated(meta$node_id)),
			sprintf("%d duplicate(s)", sum(duplicated(meta$node_id))))

	#	Author Identifiers Reach Every Node
		if ("author_id" %in% names(meta)) {
			reached <- sum(!is.na(meta$author_id))
			out[[length(out) + 1L]] <- pa_check_nm(
				"author_id reaches every node",
				reached == nrow(meta),
				sprintf("%d of %d (%.1f%%)", reached, nrow(meta),
						100 * reached / nrow(meta)))
		}

	#	Every Percentage Is a Percentage
		if (nrow(coverage) > 0L) {
			out[[length(out) + 1L]] <- pa_check_nm(
				"coverage percentages lie in [0, 100]",
				all(coverage$pct >= 0 & coverage$pct <= 100),
				sprintf("%.1f to %.1f", min(coverage$pct), max(coverage$pct)))
			out[[length(out) + 1L]] <- pa_check_nm(
				"present never exceeds n",
				all(coverage$present <= coverage$n),
				sprintf("max excess %d",
						max(coverage$present - coverage$n)))
		}

	#	The Era Gradient on Cited-Only Works
	#	Reported, not asserted. Cited-only works are reached by the reference strings
	#	alone, and that reach grows across the window, so a covariate computed over a
	#	community's full membership inherits an era gradient. Computed on its
	#	articles, it does not. There is no threshold here to pass; the number is a
	#	fact the analysis has to state.
		if (nrow(coverage) > 0L && "journal_cr" %in% coverage$field) {
			for (which_role in c("article", "cited")) {
				sub <- coverage[coverage$field == "journal_cr" &
								coverage$role == which_role, ]
				detail <- if (nrow(sub) > 0L) {
							  sprintf("%.1f%% to %.1f%% across %d era(s)",
									  min(sub$pct), max(sub$pct), nrow(sub))
						  } else {
							  "no rows"
						  }
				out[[length(out) + 1L]] <- pa_check_nm(
					paste0("reference strings reach ", which_role,
						   " nodes, reported"),
					NA, detail)
			}
		}

	#	Articles Carry a Journal Once the Download Is Joined
	#	This is the check the reference-string figures do not make. An article is a
	#	download record, so its journal should be present for all but the book and
	#	series items. It is a real test only when the download supplied a journal
	#	column; without it the column holds reference-string fills alone, and failing
	#	on 56.5% would be reporting the absent input twice rather than finding a
	#	defect.
		have_download_journal <- !is.null(schema) &&
								 any(schema$field == "journal" & schema$found)
		if (nrow(coverage) > 0L && "journal" %in% coverage$field) {
			sub <- coverage[coverage$field == "journal" &
							coverage$role == "article", ]
			if (nrow(sub) > 0L) {
				worst <- min(sub$pct)
				out[[length(out) + 1L]] <- pa_check_nm(
					"articles carry a journal in every analytic era",
					if (have_download_journal) worst >= 95 else NA,
					if (have_download_journal) {
						sprintf("worst era %.1f%%, best %.1f%%", worst,
								max(sub$pct))
					} else {
						sprintf("no journal column from the download; %.1f%% to %.1f%% from reference strings alone",
								worst, max(sub$pct))
					})
			}
		}

	#	Assembling Result
		tests <- do.call("rbind", out)
		rownames(tests) <- NULL
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests, file.path(output_dir, "metadata_tests.csv"))
		}
		return(tests)
}

####################################
#   EXTRACTING THE NODE METADATA   #
####################################

#	Run the Lane
#	Runs when the file is executed as a script and not when it is sourced. The guard
#	reads the call stack: sys.nframe() reports how many frames deep evaluation is, and
#	a file run under Rscript evaluates at depth 1 while source() evaluates its lines
#	from inside its own call, five or more frames down.
#
#		Rscript scripts/Path_A/PathA_NodeMetadata.R     runs the lane
#		source("scripts/Path_A/PathA_NodeMetadata.R")   defines the functions only
#
#	This lane has no dependencies on the other Path A files, so nothing is sourced
#	here. It reads four Rda files and writes three CSVs.
	if (sys.nframe() <= 1L) {

		cat("PathA_NodeMetadata.R -- article metadata keyed to node_id\n")
		cat("=========================================================\n\n")

		#	Preflight
		#	The node list is required; the other three are reported and skipped.
			inputs <- c(node_list = NM_NODE_FILE, download = NM_WOS_FILE,
						edge_table = NM_EDGE_FILE, legacy = NM_LEGACY_FILE)
			pre_tab <- data.frame(input = names(inputs),
								  exists = file.exists(inputs),
								  path = unname(inputs),
								  stringsAsFactors = FALSE)
			print(pre_tab, row.names = FALSE)
			if (!pre_tab$exists[pre_tab$input == "node_list"]) {
				cat("\nThe node list is required and was not found\n")
				quit(status = 1L)
			}

		#	Run
			cat("\n")
			t0 <- Sys.time()
			nm <- pa_run_node_metadata()
			secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

		#	Report
			cat("\n=========================================================\n")
			cat("Built in ", round(secs, 1), "s\n", sep = "")
			cat("Output: ", NM_OUT_DIR, "\n", sep = "")

		#	Exit Status
			n_fail <- sum(nm$tests$result == "FAIL")
			if (n_fail > 0L) {
				cat("FAILED: ", n_fail, " check(s)\n", sep = "")
				quit(status = 1L)
			}
			cat("OK\n")
	}
