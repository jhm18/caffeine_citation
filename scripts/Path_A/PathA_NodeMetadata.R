#	PathA_NodeMetadata.R -- Article Metadata Keyed to node_id
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	3 November 2026
#
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
#
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
#
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

#	Naming What to Take From the Download
#	NULL on both, which is the usual case. NM_WOS_OBJECT names the object inside the
#	Rda when it holds more than one; NM_WOS_COMPONENT names the component inside that
#	object when the object is a list of a table plus parameters and notes, as a
#	bibliometrics export often is. Left NULL, the run reports the structure and takes
#	the longest table it finds.
	NM_WOS_OBJECT    <- NULL
	NM_WOS_COMPONENT <- NULL

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
		year            = c("PY", "year", "year_published"),
		wos_categories  = c("WC", "wos_categories", "categories"),
		research_areas  = c("SC", "research_areas", "subject_categories"),
		accession       = c("UT", "accession", "wos_id"))

#	Web of Science Tags Whose Lines Are Separate Values
#	Everything else wraps: a title or abstract continued on the next line is one
#	sentence and joins with a space, not a semicolon.
	NM_LIST_TAGS <- c("AU", "AF", "BA", "BF", "CA", "GP", "DE", "ID", "CR", "C1",
					  "RP", "EM", "RI", "OI", "FU", "WC", "SC", "OA", "PA")

#	Tags That Mark the End of a Record Rather Than Carrying Content
	NM_TERMINATOR_TAGS <- c("ER", "EF")

#	Fields Retained When Flattening Batched Records
#	Every candidate name above, plus the two-letter tags a plaintext record uses for
#	them. Everything else is dropped on the way in, because a record's CR field holds
#	every reference it makes and keeping those for 39,000 records would cost hundreds
#	of megabytes to hold what the edge list already carries.
	NM_KEEP_FIELDS <- unique(c(unlist(NM_FIELDS, use.names = FALSE),
							   "UT", "PT", "AF", "C1", "WC", "SC"))

#	Why the Category Fields Are Kept
#	WC holds Web of Science Categories and SC holds Research Areas, both assigned per
#	record. They are the basis for disciplinary distance and for the venue-based
#	cognitive measure, and they are better than a hand-built journal list for that
#	job: \S8.4 records that the four core cognitive-architecture venues hold nine
#	nodes between them in a corpus of 590,170, so a measure keyed to journal titles
#	has almost nothing to key on, while a category assigned to every record does.
#	UT is the Web of Science accession number, kept as a stable record identifier in
#	case the download is ever refreshed.

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
		#		the object, exactly as it was saved
		#	Notes:
		#		Loads into a dedicated environment so nothing in the caller's frame
		#		is shadowed, and refuses to guess when a file holds several objects
		#		and no name was given.
		#
		#		Deliberately does NOT coerce. An earlier version wrapped the result
		#		in as.data.frame(), which crashed with "arguments imply differing
		#		number of rows" on a saved object that is a list of unequal-length
		#		components rather than a table. Coercion is the caller's decision,
		#		through pa_as_table(), so a file that is not a table can be
		#		described instead of killing the run.
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
			return(get(what, envir = env, inherits = FALSE))
	}

#	Coerce to a Table, or Give Up Cleanly
	pa_as_table <- function(x) {
		#	"""
		#	Args:
		#		x: any loaded object
		#	Returns:
		#		a data.frame, or NULL when x is not one table
		#	Notes:
		#		A data.frame or data.table passes through. Anything else is tried
		#		and the failure caught, because as.data.frame() on a list of
		#		unequal-length components raises rather than returning NULL.
		#	"""

		#	Early return
			if (is.data.frame(x)) {
				return(as.data.frame(x))
			}

		#	Refuse Anything That Is Not Already Column-Shaped
		#	as.data.frame() will happily accept a nested list whose leaves happen to
		#	share a length and return a frame of one column per leaf -- a batched
		#	download of equal-sized batches came back 120 x 8392 that way, which is
		#	worse than an error because it looks like success. So a list qualifies
		#	only when every component is an atomic vector of one common length.
			if (!is.list(x) || length(x) == 0L || is.null(names(x))) {
				return(NULL)
			}
			if (!all(vapply(x, is.atomic, logical(1)))) {
				return(NULL)
			}
			lens <- vapply(x, length, integer(1))
			if (length(unique(lens)) != 1L || lens[1] == 0L) {
				return(NULL)
			}

		#	Try, and Give Up Rather Than Raise
			out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE),
							error = function(e) NULL)

		#	Assembling Result
			if (is.null(out) || nrow(out) == 0L) return(NULL)
			return(out)
	}

#	Pick the Article Table Out of a List-Shaped File
	pa_pick_table <- function(x, prefer = NULL) {
		#	"""
		#	Args:
		#		x: a loaded object that is a list but not a table
		#		prefer: a component name to use, or NULL to choose
		#	Returns:
		#		list(table, chosen), or NULL when no component is a table
		#	Notes:
		#		A Web of Science export saved by a bibliometrics package is often a
		#		list of a data frame plus parameters and notes. Rather than make the
		#		caller edit code to name the data frame, this finds the components
		#		that are tables and takes the longest, which is the one row per
		#		article. The choice is returned so the run can report it.
		#	"""

		#	Early return
			if (!is.list(x) || is.data.frame(x)) {
				return(NULL)
			}

		#	A Named Preference Wins
			nm <- names(x)
			if (!is.null(prefer) && !is.null(nm) && prefer %in% nm &&
				is.data.frame(x[[prefer]])) {
				return(list(table = as.data.frame(x[[prefer]]), chosen = prefer))
			}

		#	Otherwise the Longest Table
			is_tab <- vapply(x, is.data.frame, logical(1))
			if (!any(is_tab)) {
				return(NULL)
			}
			rows <- vapply(seq_along(x), function(i) {
				if (is_tab[i]) nrow(x[[i]]) else 0L
			}, integer(1))
			best <- which.max(rows)
			if (rows[best] == 0L) {
				return(NULL)
			}
			label <- if (is.null(nm)) paste0("[[", best, "]]") else nm[best]

		#	Assembling Result
			return(list(table = as.data.frame(x[[best]]), chosen = label))
	}

#	Convert the Download With bibliometrix, if It Is Installed
	pa_wos_convert2df <- function(x, dbsource = "wos", format = "plaintext") {
		#	"""
		#	Args:
		#		x: the loaded download, a list of batches of records
		#		dbsource: bibliometrix dbsource (default "wos")
		#		format: bibliometrix format (default "plaintext")
		#	Returns:
		#		data.frame with bibliometrix's standard column names, or NULL
		#	Notes:
		#		This is the preferred route and pa_flatten_records is the fallback.
		#		The saved object's shape -- a list named by export file, each holding
		#		500 records -- is what a bibliometrix read produces, and convert2df
		#		is the maintained converter for it. It knows the tag quirks a
		#		hand-rolled parser has to rediscover one at a time.
		#
		#		Returns NULL rather than raising on any failure, so the fallback can
		#		run. A missing package is one such failure and not an error.
		#	"""

		#	Early return
			if (!requireNamespace("bibliometrix", quietly = TRUE)) {
				return(NULL)
			}

		#	Flatten to Lines, Which Is What convert2df Consumes
			lines <- tryCatch(as.character(unlist(x, use.names = FALSE)),
							  error = function(e) NULL)
			if (is.null(lines) || length(lines) == 0L) {
				return(NULL)
			}

		#	Convert
			out <- tryCatch(
				bibliometrix::convert2df(lines, dbsource = dbsource,
										 format = format),
				error = function(e) NULL, warning = function(w) NULL)

		#	Assembling Result
			if (is.null(out) || !is.data.frame(out) || nrow(out) == 0L) {
				return(NULL)
			}
			return(as.data.frame(out))
	}

#	Parse One Web of Science Record
	pa_wos_record <- function(rec) {
		#	"""
		#	Args:
		#		rec: one record from the download
		#	Returns:
		#		named list of one-element character fields, or NULL
		#	Notes:
		#		Two shapes are handled because an export can arrive either way. A
		#		NAMED record is already a field map and only needs its multi-element
		#		fields collapsed -- authors, keywords and references come as vectors.
		#		An UNNAMED character vector is raw plaintext, where a field begins
		#		with a two-letter tag and continues on lines indented by three
		#		spaces, so the tag has to be carried down the continuation lines.
		#	"""

		#	Early return
			if (length(rec) == 0L) {
				return(NULL)
			}

		#	A Named Record Is a Tag Map, but Its Values Are Lists of Words
		#	Caffeine_Compilation_31Mar2021.R built this object by splitting every
		#	line on spaces, so a tag's value is a list of lines and each line is a
		#	character vector of words -- and the first word of the first line is the
		#	tag itself. as.character() on that deparses the vector instead of
		#	reading it, which is why an earlier version produced strings like
		#	c("TI", "A", "study").
			if (!is.null(names(rec)) && any(nzchar(names(rec)))) {
				tags <- names(rec)
				out <- lapply(seq_along(rec), function(i) {
					#	Each Line Becomes One String
						value <- rec[[i]]
						if (!is.list(value)) value <- list(value)
						lines_text <- vapply(value, function(ln) {
							paste(as.character(ln), collapse = " ")
						}, character(1))

					#	Drop the Tag From the First Line
						if (length(lines_text) > 0L) {
							lines_text[1] <- sub(paste0("^", tags[i], "[[:space:]]*"),
												 "", lines_text[1])
						}

					#	A List Tag Keeps Its Line Boundaries
						sep <- if (tags[i] %in% NM_LIST_TAGS) "; " else " "
						trimws(paste(lines_text[nzchar(lines_text)], collapse = sep))
				})
				names(out) <- tags
				out <- out[!(tags %in% NM_TERMINATOR_TAGS)]
				return(out[nzchar(unlist(out))])
			}

		#	Otherwise Plaintext: Carry the Tag Down Continuation Lines
		#	A tag is two characters at the start of a line. It may be followed by a
		#	space and a value, or stand alone -- the record terminator ER does, which
		#	is why the pattern cannot require the space. Requiring it made ER a
		#	continuation of whatever field came last, so the DOI came out as
		#	"10.1/7; ER".
			lines <- as.character(unlist(rec, use.names = FALSE))
			is_tag <- grepl("^[A-Z][A-Z0-9]( |$)", lines)
			tag <- ifelse(is_tag, substr(lines, 1L, 2L), NA_character_)
			body <- ifelse(is_tag, sub("^[A-Z][A-Z0-9] ?", "", lines), lines)
			body <- sub("^[[:space:]]+", "", body)
			carried <- tag
			for (i in seq_along(carried)) {
				if (is.na(carried[i]) && i > 1L) carried[i] <- carried[i - 1L]
			}
			keep <- !is.na(carried) & nzchar(body) &
					!(carried %in% NM_TERMINATOR_TAGS)

		#	Early return
			if (!any(keep)) {
				return(NULL)
			}

		#	Join Each Field With the Separator Its Content Calls For
		#	A list field carries one value per line -- authors, keywords, references
		#	-- and joins with a semicolon. Everything else is text wrapped across
		#	lines, where a semicolon would split a title mid-sentence: "A study;
		#	continued" rather than "A study continued".
			fields <- split(body[keep], carried[keep])

		#	Assembling Result
			out <- lapply(names(fields), function(f) {
				sep <- if (f %in% NM_LIST_TAGS) "; " else " "
				paste(fields[[f]], collapse = sep)
			})
			names(out) <- names(fields)
			return(out)
	}

#' @title pa_flatten_records
#' @description Flatten a batched Web of Science download into one row per record.
#'
#' @param x The loaded download: a list of batches, each a list of records.
#' @param keep Field names to retain, or NULL for all. Default NULL.
#'
#' @details
#' A Web of Science export comes in 500-record batches because that is the interface's
#' download limit, so the saved object is a list of lists and no component is a table.
#' This unlists one level to reach the records, parses each, and binds them with
#' fill = TRUE so a record missing a field gets NA rather than shifting the columns.
#'
#' Retaining only the needed fields is not an optimization. A record's CR field holds
#' every reference it makes, often hundreds, and keeping those for 39,000 records
#' would cost hundreds of megabytes to hold a quantity the edge list already carries.
#'
#' @return A list with table, n_records and fields.
#'
#' @examples
#' flat <- pa_flatten_records(download)
#' flat$fields
#'
#' @export
pa_flatten_records <- function(x, keep = NULL) {
	#	"""
	#	Args:
	#		x: the loaded download, a list of batches of records
	#		keep: field names to retain, or NULL for all
	#	Returns:
	#		list(table, n_records, fields)
	#	"""

	#	Validation
		if (!is.list(x) || length(x) == 0L) {
			stop("pa_flatten_records: expected a non-empty list")
		}

	#	Reach the Records
	#	One level down from batches. recursive = FALSE matters: unlisting all the
	#	way would dissolve the records themselves into a character vector.
		records <- unlist(x, recursive = FALSE, use.names = FALSE)
		if (length(records) == 0L) {
			stop("pa_flatten_records: no records found one level down")
		}

	#	Which Fields Exist, From a Sample
		sample_n <- min(200L, length(records))
		parsed_sample <- lapply(records[seq_len(sample_n)], pa_wos_record)
		fields <- sort(unique(unlist(lapply(parsed_sample, names))))

	#	Early return
		if (length(fields) == 0L) {
			stop("pa_flatten_records: no field names could be read from the ",
				 "first ", sample_n, " records")
		}

	#	Where Each Record Sits
	#	Carried because it is the only complete link to the canonical node key.
	#	article_combinedv2.Rda holds file_id 1 to 79 and article_id 1 to 500 against
	#	source_combined, so a record's position identifies it even when it has no
	#	DOI -- and 5,061 of the 35,103 articles have a triple key rather than a DOI
	#	\meas{}, which a DOI join cannot reach at all.
		batch_names <- names(x)
		if (is.null(batch_names)) batch_names <- paste0("[[", seq_along(x), "]]")
		position <- rbindlist(lapply(seq_along(x), function(i) {
			rec_names <- names(x[[i]])
			if (is.null(rec_names)) rec_names <- as.character(seq_along(x[[i]]))
			data.table(batch_index  = i,
					   batch_name   = batch_names[i],
					   record_name  = rec_names,
					   record_index = seq_along(x[[i]]))
		}))

	#	Parse, Keeping Only What Is Asked For
		rows <- lapply(records, function(rec) {
			one <- pa_wos_record(rec)
			if (is.null(one)) return(list(.unparsed = NA_character_))
			if (!is.null(keep)) one <- one[intersect(names(one), keep)]
			if (length(one) == 0L) return(list(.unparsed = NA_character_))
			one
		})

	#	Assembling Result
	#	Unparsed records are kept as blank rows rather than dropped, so that row i
	#	still corresponds to position i. Dropping them would shift every record
	#	after the first failure out of alignment with its key.
		out <- rbindlist(rows, fill = TRUE, use.names = TRUE)
		if (".unparsed" %in% names(out)) {
			out[, ".unparsed" := NULL]
		}
		out <- cbind(position[seq_len(nrow(out))], out)
		return(list(table = as.data.frame(out), n_records = length(records),
					fields = fields))
}

#	Describe an Object That Is Not a Table
	pa_describe_object <- function(x, depth = 2L) {
		#	"""
		#	Args:
		#		x: any loaded object
		#		depth: how many levels to describe (default 2)
		#	Returns:
		#		data.frame(level, element, class, dim, names_head)
		#	Notes:
		#		Exists so an unexpected file shape produces a report rather than a
		#		stack trace. Recurses into the first component because a batched
		#		download is a list of lists, and one level of description stops
		#		exactly short of the field names that identify it.
		#	"""

		#	One Row Describing Anything
			describe_one <- function(el, label, level) {
				data.frame(
					level      = level,
					element    = label,
					class      = paste(class(el), collapse = "/"),
					dim        = paste(if (is.null(dim(el))) length(el) else dim(el),
									   collapse = " x "),
					names_head = paste(utils::head(names(el), 8), collapse = ", "),
					stringsAsFactors = FALSE)
			}

		#	Early return
			if (!is.list(x) || is.data.frame(x)) {
				return(describe_one(x, "<the object>", 1L))
			}

		#	Iterating Over Components
			nm <- names(x)
			if (is.null(nm)) nm <- paste0("[[", seq_along(x), "]]")
			rows <- lapply(seq_along(x), function(i) describe_one(x[[i]], nm[i], 1L))

		#	Recurse Into the First Component
		#	Capped at the first, because 79 batches of 500 would describe 39,500
		#	records to say one thing about their shape.
			if (depth > 1L && length(x) > 0L && is.list(x[[1]]) &&
				!is.data.frame(x[[1]]) && length(x[[1]]) > 0L) {
				first <- x[[1]]
				fnm <- names(first)
				if (is.null(fnm)) fnm <- paste0("[[", seq_along(first), "]]")
				rows[[length(rows) + 1L]] <- describe_one(
					first[[1]], paste0(nm[1], " -> ", fnm[1]), 2L)
			}

		#	Assembling Result
			return(do.call("rbind", rows))
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

#' @title pa_download_keys
#' @description The canonical node key for each download record, by its position.
#'
#' @param edge_table The edge-level table, article_combined.
#' @param key_col Name of the canonical key column. Default "source_combined".
#'
#' @details
#' This is the bridge that makes the download joinable in full. The edge table holds
#' file_id, article_id and the citing article's canonical key on every row, so taking
#' the distinct triples gives one key per download record -- 35,103 positions
#' resolving to 34,974 distinct keys, which is exactly the article-node count
#' \meas{}. A position appears more than once where consecutive export files overlap
#' at their boundary, so the result is deduplicated on the key.
#'
#' Position rather than DOI is the point. 5,061 of those keys are author-year-journal
#' triples formed by a canonicalization upstream of this project, and the rule that
#' formed them is not recorded here, so they cannot be reconstructed from the
#' download's author, year and journal fields without guessing. Their position can.
#'
#' @return A data.table with file_id, article_id and node_key.
#'
#' @examples
#' keys <- pa_download_keys(article_combined)
#'
#' @export
pa_download_keys <- function(edge_table, key_col = "source_combined") {
	#	"""
	#	Args:
	#		edge_table: the edge-level table
	#		key_col: name of the canonical key column
	#	Returns:
	#		data.table(file_id, article_id, node_key)
	#	"""

	#	Validation
		need <- c("file_id", "article_id", key_col)
		missing <- setdiff(need, names(edge_table))
		if (length(missing) > 0L) {
			stop("pa_download_keys: edge table is missing: ",
				 paste(missing, collapse = ", "))
		}

	#	One Row per Position
		dt <- as.data.table(edge_table)[, .(file_id = as.integer(file_id),
											article_id = as.integer(article_id),
											node_key = pa_norm_key(get(key_col)))]
		dt <- dt[!is.na(file_id) & !is.na(article_id) & !is.na(node_key)]
		dt <- unique(dt, by = c("file_id", "article_id"))

	#	Assembling Result
		setorder(dt, file_id, article_id)
		return(dt)
}

#' @title pa_position_alignment
#' @description Check that a download record's position resolves to its own DOI.
#'
#' @param wos The flattened download, carrying batch_index and record_name.
#' @param download_keys Output of pa_download_keys.
#' @param schema Output of pa_resolve_schema.
#'
#' @details
#' The positional join assumes that file_id enumerates the download's batches in the
#' same order the batches appear, which is the alphabetical order list.files() gave
#' the compilation script. That assumption is testable rather than something to take
#' on faith: where a record carries its own DOI and its position resolves to a
#' DOI-shaped key, the two must be the same string. A low agreement rate means the
#' batch ordering differs and the positional keys are attached to the wrong records,
#' which would silently mislabel every article's metadata.
#'
#' @return A data.frame with comparable, agreeing and pct.
#'
#' @examples
#' pa_position_alignment(wos, download_keys, schema)
#'
#' @export
pa_position_alignment <- function(wos, download_keys, schema) {
	#	"""
	#	Args:
	#		wos: the flattened download
	#		download_keys: output of pa_download_keys
	#		schema: output of pa_resolve_schema
	#	Returns:
	#		data.frame(comparable, agreeing, pct)
	#	"""

	#	Early return
		doi_col <- schema$column[schema$field == "doi"]
		if (is.null(download_keys) || length(doi_col) != 1L || is.na(doi_col) ||
			!all(c("batch_index", "record_name") %in% names(wos))) {
			return(data.frame(comparable = 0L, agreeing = 0L, pct = NA_real_,
							  stringsAsFactors = FALSE))
		}

	#	Attach the Positional Key
		dt <- as.data.table(wos)[, .(batch_index = as.integer(batch_index),
									 record_name = as.character(record_name),
									 own = pa_norm_key(get(doi_col)))]
		dk <- as.data.table(download_keys)[, .(batch_index = as.integer(file_id),
											   record_name = as.character(article_id),
											   positional = node_key)]
		dt[dk, on = .(batch_index, record_name), positional := i.positional]

	#	Compare Only Where Both Are DOIs
		cmp <- dt[!is.na(own) & nzchar(own) & !is.na(positional) &
				  grepl("^10\\.", positional)]

	#	Assembling Result
		return(data.frame(
			comparable = nrow(cmp),
			agreeing   = sum(cmp$own == cmp$positional),
			pct        = if (nrow(cmp) == 0L) NA_real_ else
							round(100 * mean(cmp$own == cmp$positional), 1),
			stringsAsFactors = FALSE))
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
#' @param download_keys Output of pa_download_keys, or NULL to join the download on DOI alone.
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
#' meta <- pa_node_metadata(node_list, wos, schema, references, legacy, keys)
#'
#' @export
pa_node_metadata <- function(node_list, wos = NULL, schema = NULL,
							 references = NULL, legacy = NULL,
							 download_keys = NULL) {
	#	"""
	#	Args:
	#		node_list: the rebuilt node list
	#		wos: Web of Science table, or NULL
	#		schema: output of pa_resolve_schema, or NULL
	#		references: output of pa_parse_references, or NULL
	#		legacy: legacy node list with author_id, or NULL
	#		download_keys: output of pa_download_keys, or NULL
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

	#	From the Download
		if (!is.null(wos) && !is.null(schema)) {
			#	Validation
				doi_col <- schema$column[schema$field == "doi"]
				have_doi <- length(doi_col) == 1L && !is.na(doi_col)
				have_position <- !is.null(download_keys) &&
								 all(c("batch_index", "record_name") %in%
									 names(wos))
				if (!have_doi && !have_position) {
					stop("pa_node_metadata: the download offers neither a doi ",
						 "column nor a position, so it cannot be joined")
				}

			#	The Columns Worth Taking
				take <- schema[schema$found & schema$field != "doi", , drop = FALSE]
				lookup <- as.data.table(wos)
				keep_cols <- intersect(c("batch_index", "record_name", doi_col,
										 take$column), names(lookup))
				lookup <- lookup[, keep_cols, with = FALSE]
				setnames(lookup, take$column, take$field, skip_absent = TRUE)

			#	Prefer the Canonical Key From Position
			#	A DOI join reaches only the DOI-keyed articles, which was measured
			#	at 62.8% of era 11 rising to 94.7% of era 17 -- matching, era for
			#	era, the share of article keys that are DOIs rather than triples
			#	\meas{}. Position reaches both kinds.
				lookup[, key := NA_character_]
				if (have_position) {
					dk <- as.data.table(download_keys)[, .(
							batch_index = as.integer(file_id),
							record_name = as.character(article_id),
							positional_key = node_key)]
					lookup[, record_name := as.character(record_name)]
					lookup[dk, on = .(batch_index, record_name),
						   key := i.positional_key]
				}

			#	Fall Back to the DOI Where Position Gave Nothing
				if (have_doi) {
					lookup[is.na(key),
						   key := pa_norm_key(get(doi_col))]
				}

			#	Join
				lookup <- lookup[!is.na(key) & nzchar(key)]
				lookup <- unique(lookup, by = "key")
				meta[lookup, on = "key",
					 (take$field) := mget(paste0("i.", take$field))]
		}

	#	First Author From the Download's Author List
	#	The download reaches every article, so taking the first name off its author
	#	list beats the reference-string route, which reaches 57 to 69%. The CR fill
	#	below then covers only the cited-only works the download never held.
		if ("authors" %in% names(meta)) {
			meta[, first_author := NA_character_]
			meta[!is.na(authors) & nzchar(authors),
				 first_author := trimws(sub(";.*$", "", authors))]
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
		node_list <- pa_as_table(pa_load_one(node_file, "pa_nodes"))
		if (is.null(node_list)) {
			stop("pa_run_node_metadata: pa_nodes in ", node_file,
				 " is not a table")
		}
		if (verbose) cat("Node list: ", nrow(node_list), " nodes\n", sep = "")

	#	The Download, if Present
	#	Loaded without coercion, then coerced here, because the file may hold a list
	#	of components rather than one table. If it does, the run describes it and
	#	continues rather than dying on as.data.frame().
		wos <- NULL
		schema <- NULL
		if (file.exists(wos_file)) {
			wos_raw <- pa_load_one(wos_file, NM_WOS_OBJECT)
			wos <- pa_as_table(wos_raw)

			#	A List-Shaped File: a Table Inside It, or Batched Records
				if (is.null(wos)) {
					cat("\nThe download is not a single table. Its structure:\n")
					print(utils::head(pa_describe_object(wos_raw), 6),
						  row.names = FALSE)
					cat("  ... ", length(wos_raw), " component(s) in all\n",
						sep = "")

					#	First Try: a Component That Is Already a Table
						picked <- pa_pick_table(wos_raw, NM_WOS_COMPONENT)
						if (!is.null(picked)) {
							wos <- picked$table
							cat("\n  Taking component \"", picked$chosen, "\": ",
								nrow(wos), " rows, ", ncol(wos), " columns.\n",
								"  Set NM_WOS_COMPONENT to override.\n", sep = "")
						}

					#	Second Try: Parse the Records Directly
					#	This comes before bibliometrix, which is the opposite of what
					#	you would expect, and the reason is in
					#	Caffeine_Compilation_31Mar2021.R. That script built this
					#	object by splitting every line on spaces, so a record is a
					#	tag map whose values are lists of word vectors. convert2df
					#	wants the plaintext LINES, and unlisting this object yields
					#	individual words, not lines -- so it cannot read it. It is
					#	kept below for a download that is still raw plaintext.
						if (is.null(wos)) {
							cat("\n  No component is a table. Reading it as ",
								"batched records...\n", sep = "")
							flat <- tryCatch(
								pa_flatten_records(wos_raw, keep = NM_KEEP_FIELDS),
								error = function(e) {
									cat("  could not: ", conditionMessage(e), "\n",
										sep = "")
									NULL
								})
							if (!is.null(flat)) {
								wos <- flat$table
								cat("  ", flat$n_records, " record(s) in ",
									length(wos_raw), " batch(es); ",
									nrow(wos), " flattened\n", sep = "")
								cat("  fields present in the download: ",
									paste(flat$fields, collapse = " "), "\n",
									sep = "")
							}
						}

					#	Third Try: bibliometrix, for a Still-Raw Plaintext Download
						if (is.null(wos)) {
							cat("\n  Trying bibliometrix::convert2df()...\n")
							wos <- pa_wos_convert2df(wos_raw)
							if (!is.null(wos)) {
								cat("  converted: ", nrow(wos), " records, ",
									ncol(wos), " columns\n", sep = "")
							} else if (!requireNamespace("bibliometrix",
														 quietly = TRUE)) {
								cat("  bibliometrix is not installed\n")
							} else {
								cat("  convert2df could not read it either\n")
							}
						}

					#	Give Up Cleanly
						if (is.null(wos)) {
							cat("\n  Continuing without the download, so no text ",
								"columns.\n", sep = "")
						}
				}
			rm(wos_raw)
			if (!is.null(wos)) {
				schema <- pa_resolve_schema(wos)
			}
		}
		if (!is.null(wos)) {
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
		} else if (verbose && !file.exists(wos_file)) {
			cat("Download NOT FOUND at ", wos_file,
				"\n  continuing on the reference strings alone; no text columns\n",
				sep = "")
		}

	#	The Reference Strings, if Present
		references <- NULL
		download_keys <- NULL
		if (file.exists(edge_file)) {
			edge_table <- pa_as_table(pa_load_one(edge_file))
			if (is.null(edge_table)) {
				stop("pa_run_node_metadata: ", edge_file, " is not a table")
			}
			references <- pa_parse_references(edge_table)

			#	The Positional Bridge to the Canonical Keys
			#	Built from the same table, because file_id and article_id index
			#	the download and source_combined is the key the node list uses.
				download_keys <- tryCatch(pa_download_keys(edge_table),
										  error = function(e) NULL)
			if (verbose) {
				cat("\nReference strings: ", nrow(references),
					" works keyed by DOI with a journal\n", sep = "")
				if (is.null(download_keys)) {
					cat("Positional keys: unavailable; the download will join on ",
						"DOI alone\n", sep = "")
				} else {
					cat("Positional keys: ", nrow(download_keys),
						" download position(s), ",
						sum(!grepl("^10\\.", download_keys$node_key)),
						" of them triples rather than DOIs\n", sep = "")
				}
			}
			rm(edge_table)
		} else if (verbose) {
			cat("\nEdge table NOT FOUND at ", edge_file, "\n", sep = "")
		}

	#	The Legacy List, if Present
		legacy <- NULL
		if (file.exists(legacy_file)) {
			legacy <- pa_as_table(pa_load_one(legacy_file, "node_list"))
			if (is.null(legacy)) {
				stop("pa_run_node_metadata: node_list in ", legacy_file,
					 " is not a table")
			}
			if (verbose) {
				cat("Legacy list: ", nrow(legacy), " nodes with author_id\n",
					sep = "")
			}
		} else if (verbose) {
			cat("Legacy list NOT FOUND at ", legacy_file, "\n", sep = "")
		}

	#	Does a Record's Position Resolve to Its Own DOI?
	#	The positional join rests on file_id enumerating the batches in the
	#	order they appear. Tested rather than assumed.
		alignment <- NULL
		if (!is.null(wos) && !is.null(schema) && !is.null(download_keys)) {
			alignment <- pa_position_alignment(wos, download_keys, schema)
			if (verbose) {
				cat("Position alignment: ", alignment$agreeing, " of ",
					alignment$comparable, " records resolve to their own DOI",
					if (!is.na(alignment$pct)) sprintf(" (%.1f%%)", alignment$pct)
					else "", "\n", sep = "")
			}
		}

	#	Build
		if (verbose) cat("\nJoining...\n")
		meta <- pa_node_metadata(node_list, wos, schema, references, legacy,
								 download_keys)
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
											alignment, output_dir = output_dir)
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
#' @param alignment Output of pa_position_alignment, or NULL.
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
									   alignment = NULL,
									   output_dir = NM_OUT_DIR) {
	#	"""
	#	Args:
	#		meta: output of pa_node_metadata
	#		node_list: the rebuilt node list
	#		coverage: output of pa_metadata_coverage
	#		schema: output of pa_resolve_schema, or NULL
	#		alignment: output of pa_position_alignment, or NULL
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

	#	The Positional Join Is Attached to the Right Records
	#	This is the check that protects every metadata column at once. If the
	#	batch ordering were wrong, each article would receive another
	#	article's title, journal and abstract, and nothing downstream would
	#	notice.
		if (!is.null(alignment) && alignment$comparable > 0L) {
			out[[length(out) + 1L]] <- pa_check_nm(
				"a record's position resolves to its own DOI",
				alignment$pct >= 99,
				sprintf("%d of %d (%.1f%%)", alignment$agreeing,
						alignment$comparable, alignment$pct))
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
