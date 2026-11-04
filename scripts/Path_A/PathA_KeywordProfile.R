#	PathA_KeywordProfile.R -- What the Keywords Say, and Which Terms Are Safe
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	3 November 2026
#
#	WHY THIS EXISTS. The cognitive subject-matter measure needs a lexicon, and a
#	hand-written one failed in a specific way: a draft produced false positives on
#	inhibit (enzyme inhibition), memory (immunological memory) and affect used as a
#	verb (\S8.4). Writing a longer list by hand would produce more of the same,
#	because the problem is not coverage. It is that a term's sense depends on what it
#	appears with, and a list of terms cannot record that.
#
#	WHAT THIS DOES INSTEAD. It starts from a small seed set of terms that are
#	unambiguous in this corpus -- vigilance, reaction time, sustained attention --
#	and asks the keyword record two questions about every other term:
#
#		PERSISTENCE. In how many eras does the term appear, and at what rate? A term
#		that surfaces in one era and vanishes is a fashion, not a concept. One
#		present across the window is a candidate for a measure that has to be
#		comparable across eras.
#
#		SEED ADJACENCY. Of the articles carrying the term, what share also carry a
#		seed term? This is the discriminator the hand-written list could not have.
#		"Memory" in a cognitive article sits beside working, recall, attention;
#		"memory" in an immunology article sits beside T cell and antigen. The term
#		is identical and the company is not.
#
#	HOW THE OUTPUT IS MEANT TO BE USED. A term with high persistence and high seed
#	adjacency can enter the lexicon unconditionally. A term with high persistence and
#	LOW seed adjacency is the inhibit case and should stay out. A term in between is
#	the memory case, and belongs in the lexicon only conditionally -- counted when it
#	co-occurs with a seed term and not otherwise. The three columns say which of the
#	three a term is; the judgement about where to put the thresholds is not a
#	measurement and is left to the reader.
#
#	A LIMIT WORTH STATING FIRST. Keyword coverage is 0.2% of articles in eras 11 and
#	12, 17 to 33% in era 13, and 58 to 97% from era 14 \meas{}. So persistence can
#	only be assessed over eras 14 to 17, and this lane reports era 13 for context
#	without counting it. Nothing here can speak about the early window.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, data.table for the heavy
#	computation, base R for everything else.

#################
#   LIBRARIES   #
#################

#	Libraries
	library(data.table)
	library(Matrix)

#####################
#   CONFIGURATION   #
#####################

#	Project Root
	KP_ROOT <- Sys.getenv("CAFFEINE_DIR", "/workspace/caffeine_citation")

#	Input
	KP_META_FILE <- file.path(KP_ROOT, "data/node_metadata/node_metadata.Rda")

#	Output
	KP_OUT_DIR <- file.path(KP_ROOT, "data/keyword_profile")

#	Eras With Enough Keyword Coverage to Assess Persistence
#	Era 13 is reported for context and excluded from the persistence count, because
#	a term cannot be shown absent from an era whose keywords are 33% recorded.
	KP_ERAS        <- 13:17
	KP_ERAS_SCORED <- 14:17

#	The Seed Set
#	Deliberately short and deliberately unambiguous in THIS corpus. Every term here
#	should be one a reader would accept as cognitive without argument. The point is
#	not to be complete -- completeness is what the adjacency statistic produces --
#	but to be above dispute, because everything downstream is measured against it.
#
#	Neuroscience and pharmacology terms are absent on purpose. They are the other
#	side of the asymmetry RQ9 predicts, and seeding with them would dissolve the
#	contrast the model exists to estimate.
#	The Category Anchor
#	The seed set below is hand-written, and a hand-written list is what produced the
#	inhibit and memory failures in the first place. So it is not the primary
#	instrument. The primary instrument is Clarivate's own classification: SC and WC
#	are assigned per record by an outside party, reach 99.8% of articles, and owe
#	nothing to anyone's judgement about this corpus. Terms are ranked by how much
#	more often they appear in articles carrying one of these categories than in the
#	rest, and that ranking is an empirical candidate lexicon with no term list behind
#	it.
#
#	Neuroscience and pharmacology are absent deliberately. They are the other side of
#	the asymmetry RQ9 predicts, and including them would dissolve the contrast.
	KP_COG_CATEGORIES <- c("Psychology", "Behavioral Sciences")

#	The Narrow Class and What It Is Compared Against
#	Two contrasts are run, because they answer different questions and only the
#	second can say anything about cognitive work specifically.
#
#		BROAD. Psychology and Behavioral Sciences against the whole corpus. This
#		separates psychology from biochemistry and immunology, so its top terms are
#		generic experimental vocabulary. Useful as a description of what
#		psychological caffeine research talks about; useless as a cognitive lexicon.
#
#		NESTED. Psychology, Experimental against the REST of psychology and
#		behavioral sciences. Terms that merely mark an article as psychological
#		score near one here, because both sides carry them, and fall out of the
#		ranking without being excluded by hand.
#
#	Clarivate has no "Cognitive Science" category, which is the same gap \S8.4
#	records on the venue side: the four core cognitive-architecture journals hold
#	nine nodes in a corpus of 590,170. Psychology, Experimental is the closest the
#	classification comes, and saying so is more honest than calling it cognitive.
	KP_NARROW_CATEGORIES   <- c("Psychology, Experimental")
	KP_CONTRAST_CATEGORIES <- c("Psychology", "Behavioral Sciences")

#	The Text Anchor
#	The primary instrument, and the one the categories cannot provide. An article is
#	in the cognitive set when its title, abstract or keywords say so. This is the
#	field naming itself, which is a better anchor than either a category assigned by
#	an indexer or a term list written by us.
#
#	THE WORD BOUNDARY IS NOT COSMETIC. A bare "cognit" also matches RECOGNITION, and
#	in this corpus that is not a rare edge case: 473 articles entered the set through
#	it \meas{}, almost all of them analytical chemistry -- molecularly imprinted
#	polymers for caffeine detection, where "molecular recognition" and "recognition
#	sites" are terms of art. The contrast then returned imprinted, polymer, monomer,
#	template, polymerization and nmr as the terms that most distinguish cognitive
#	articles from the rest of psychology, which is how the defect announced itself.
#
#	The prefixes are listed rather than left open because neurocognitive and
#	metacognitive should be in and recognition should not, and a word boundary alone
#	cannot tell them apart.
#
#	The contrast then asks the question that matters: of the words appearing in those
#	articles, which are MORE common inside the cognitive set than outside it, and
#	which are as common or commoner outside? The second group is vocabulary the
#	cognitive articles share with everyone else and carries no information about
#	cognition, however cognitive it sounds.
	KP_COG_PATTERN <- "\\b(neuro|meta|socio|psycho)?cognit"
	KP_TEXT_FIELDS <- c("title", "abstract", "keywords_author", "keywords_index")

#	Tokenization
#	Bigrams as well as single words, because the terms that matter here are mostly
#	two words -- working memory, reaction time, sustained attention -- and a unigram
#	model would split every one of them. Bigrams are formed on the cleaned token
#	sequence BEFORE stopwords are dropped, then any bigram containing a stopword is
#	discarded: dropping stopwords first would make "memory of working" into the
#	false bigram "memory working".
	KP_NGRAM_MAX     <- 2L
	KP_MIN_TOKEN_CHR <- 3L

#	Stopwords
#	Function words only. Domain words are deliberately absent -- "caffeine" and
#	"study" are left in so the contrast can demonstrate that it rates them near one,
#	which is a check on the method rather than an assumption built into it.
	KP_STOPWORDS <- c(
		"the", "and", "for", "are", "but", "not", "you", "all", "any", "can",
		"had", "her", "was", "one", "our", "out", "day", "get", "has", "him",
		"his", "how", "its", "may", "new", "now", "old", "see", "two", "who",
		"boy", "did", "use", "way", "she", "that", "this", "with", "from",
		"they", "have", "been", "were", "their", "which", "would", "there",
		"when", "what", "than", "them", "then", "these", "those", "into",
		"also", "such", "only", "more", "most", "some", "both", "each",
		"other", "after", "before", "during", "between", "within", "under",
		"over", "while", "where", "whether", "because", "however", "thus",
		"therefore", "about", "above", "below", "further", "being", "does",
		"doing", "done", "could", "should", "will", "shall", "must", "might",
		"well", "very", "much", "many", "less", "least", "same", "used",
		"using", "based", "found", "shown", "show", "showed", "observed",
		"compared", "including", "included", "respectively", "versus",
		"although", "though", "since", "upon", "among", "per", "via", "nor",
		"yet", "own", "off", "out", "ltd", "elsevier", "rights", "reserved",
		"copyright", "published", "wiley", "springer", "inc")

#	How Many Discriminating Terms to Treat as the Empirical Seed Set
	KP_DERIVED_SEED_N <- 20L

#	The Hand-Written Seed Set, Kept as a Comparison
#	Retained so the derived list can be checked against a prior expectation rather
#	than accepted blind. Where the two agree, the hand list is confirmed; where the
#	data offers a term nobody would have written down, that is the point of asking.
	KP_SEED <- c("vigilance", "sustained attention", "reaction time",
				 "working memory", "executive function", "psychomotor",
				 "cognitive performance", "alertness", "attention",
				 "cognition", "mental fatigue", "processing speed",
				 "divided attention", "selective attention")

#	Terms the Draft Lexicon Got Wrong
#	Carried explicitly so the report always addresses them, whatever their frequency.
	KP_SUSPECT <- c("memory", "inhibit", "inhibition", "affect", "arousal",
					"performance", "learning", "anxiety", "mood", "sleep")

#	Thresholds for the Report
	KP_MIN_ARTICLES <- 20L
	KP_TOP_N        <- 40L

#	Freezing the Lexicon
#	The report ranks; the lexicon cuts. A term is admitted on both strength and
#	support: at least KP_LEXICON_MIN_RATIO times more common inside the cognitive
#	set than outside it, carried by at least KP_MIN_ARTICLES articles. Ten is a
#	wide margin by design -- the covariate this feeds is a share, and a share
#	built from marginal terms moves with noise rather than with content.
	KP_LEXICON_MIN_RATIO <- 10

#	The Co-occurrence Visualization
#	Two matrices are drawn. The first is candidates against seeds, which decomposes
#	the adjacency number into WHICH seed term carries it -- a candidate riding
#	entirely on one seed is a different thing from one spread across several. The
#	second is the top terms against themselves, which shows the corpus's block
#	structure and is where an unexpected cluster announces itself.
	KP_WRITE_PLOTS  <- TRUE
	KP_PLOT_TOP_N   <- 24L
	KP_PLOT_LABEL_MIN <- 0.10

#	Sequential Ramp, One Hue, Light to Dark
#	Magnitude gets a single-hue sequential scale: the lightest step means near zero
#	and is allowed to recede toward the surface, the darkest means the maximum. Not
#	a rainbow, and no second hue -- a second hue would imply a polarity the data
#	does not have, since co-occurrence has a floor at zero and no midpoint.
	KP_RAMP <- c("#cde2fb", "#b7d3f6", "#9ec5f4", "#86b6ef", "#6da7ec",
				 "#5598e7", "#3987e5", "#2a78d6", "#256abf", "#1c5cab",
				 "#184f95", "#104281", "#0d366b")
	KP_SURFACE   <- "#fcfcfb"
	KP_INK       <- "#0b0b0b"
	KP_INK_MUTED <- "#52514e"

########################
#   INTERNAL HELPERS   #
########################

#	One Check Row
	pa_check_kp <- function(name, passed, detail = "") {
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

#	Normalize One Keyword
	pa_norm_term <- function(x) {
		#	"""
		#	Args:
		#		x: character vector of raw keyword strings
		#	Returns:
		#		character vector, lowercased with punctuation and runs of space
		#		reduced
		#	Notes:
		#		Hyphens become spaces so that "short-term memory" and "short term
		#		memory" are one term. Trailing plurals are left alone: stemming
		#		would merge "effects" with "effect" and also "caffeine" with
		#		"caffeines", and the gain is not worth the surprises.
		#	"""

		#	Normalize
			out <- tolower(as.character(x))
			out <- gsub("[-/]", " ", out)
			out <- gsub("[^a-z0-9 ]", " ", out)
			out <- gsub("[[:space:]]+", " ", out)

		#	Assembling Result
			return(trimws(out))
	}

#' @title pa_keyword_long
#' @description One row per article and keyword, from the author and index keyword fields.
#'
#' @param meta The node metadata table.
#' @param eras Integer eras to include. Default KP_ERAS.
#'
#' @details
#' Both keyword fields are used and the source is recorded. DE is author-supplied and
#' reaches 58 to 79\% of articles from era 14; ID is Web of Science's KeyWords Plus
#' and reaches 94 to 97\% \meas{}. They say different things -- DE is what the
#' authors called their work, ID is what the indexer inferred from its references --
#' so a term's behaviour may differ between them and the field is kept rather than
#' pooled away.
#'
#' Only articles are included. A cited-only work has no keyword record here.
#'
#' @return A data.table with node_id, era, field and term.
#'
#' @examples
#' kw <- pa_keyword_long(meta)
#' kw[field == "author", .N, by = era]
#'
#' @export
pa_keyword_long <- function(meta, eras = KP_ERAS) {
	#	"""
	#	Args:
	#		meta: the node metadata table
	#		eras: integer eras to include
	#	Returns:
	#		data.table(node_id, era, field, term)
	#	"""

	#	Validation
		need <- c("node_id", "era", "is_article")
		missing <- setdiff(need, names(meta))
		if (length(missing) > 0L) {
			stop("pa_keyword_long: metadata is missing: ",
				 paste(missing, collapse = ", "))
		}
		fields <- intersect(c("keywords_author", "keywords_index"), names(meta))
		if (length(fields) == 0L) {
			stop("pa_keyword_long: neither keyword column is present")
		}

	#	Articles in Scope
		dt <- as.data.table(meta)[is_article & era %in% as.integer(eras)]

	#	Early return
		if (nrow(dt) == 0L) {
			return(data.table(node_id = integer(0), era = integer(0),
							  field = character(0), term = character(0)))
		}

	#	Iterating Over the Two Keyword Fields
		rows <- list()
		for (f in fields) {
			#	Split on the Semicolon the Flattener Joined With
				sub <- dt[!is.na(get(f)) & nzchar(get(f)),
						  .(node_id, era, raw = get(f))]
				if (nrow(sub) == 0L) next
				split_terms <- strsplit(sub$raw, ";", fixed = TRUE)
				n_per <- lengths(split_terms)

			#	One Row per Term
				rows[[length(rows) + 1L]] <- data.table(
					node_id = rep(sub$node_id, n_per),
					era     = rep(sub$era, n_per),
					field   = if (f == "keywords_author") "author" else "index",
					term    = pa_norm_term(unlist(split_terms,
												  use.names = FALSE)))
		}

	#	Early return
		if (length(rows) == 0L) {
			return(data.table(node_id = integer(0), era = integer(0),
							  field = character(0), term = character(0)))
		}

	#	Assembling Result
		out <- rbindlist(rows)
		out <- out[nzchar(term) & nchar(term) > 2L]
		out <- unique(out)
		setorder(out, era, node_id, field, term)
		return(out)
}

#' @title pa_text_terms
#' @description Terms from titles, abstracts and keywords, one row per article and term.
#'
#' @param meta The node metadata table.
#' @param fields Metadata columns to read. Default KP_TEXT_FIELDS.
#' @param eras Integer eras to include. Default KP_ERAS.
#' @param ngram_max Longest n-gram to form. Default KP_NGRAM_MAX.
#' @param stopwords Function words to drop. Default KP_STOPWORDS.
#'
#' @details
#' Keywords alone are too thin an anchor: they reach 58 to 79\% of articles on the
#' author field and are absent before era 13 entirely. Titles are at 100\% and
#' abstracts at 93 to 96\% from era 14 \meas{}, so reading all four fields is what
#' makes the vocabulary large enough to contrast.
#'
#' Bigrams are formed on the cleaned token sequence BEFORE stopwords are dropped, and
#' any bigram containing a stopword is then discarded. Dropping stopwords first would
#' manufacture adjacencies that were never in the text -- "memory of working" would
#' become "memory working" -- and the terms this analysis is about are mostly two
#' words, so the bigrams are not optional.
#'
#' Terms are made unique per article, so a word repeated through an abstract counts
#' once. The question is which articles use a term, not how often one article does.
#'
#' @return A data.table with node_id, era, field and term.
#'
#' @examples
#' terms <- pa_text_terms(meta)
#' terms[, uniqueN(term)]
#'
#' @export
pa_text_terms <- function(meta, fields = KP_TEXT_FIELDS, eras = KP_ERAS,
						  ngram_max = KP_NGRAM_MAX,
						  stopwords = KP_STOPWORDS) {
	#	"""
	#	Args:
	#		meta: the node metadata table
	#		fields: metadata columns to read
	#		eras: integer eras to include
	#		ngram_max: longest n-gram to form
	#		stopwords: function words to drop
	#	Returns:
	#		data.table(node_id, era, field, term)
	#	"""

	#	Validation
		present <- intersect(fields, names(meta))
		if (length(present) == 0L) {
			stop("pa_text_terms: none of these columns exist: ",
				 paste(fields, collapse = ", "))
		}

	#	Articles in Scope
		dt <- as.data.table(meta)[is_article & era %in% as.integer(eras)]

	#	Early return
		if (nrow(dt) == 0L) {
			return(data.table(node_id = integer(0), era = integer(0),
							  field = character(0), term = character(0)))
		}

	#	Iterating Over Eras, to Keep the Peak Smaller
		stop_set <- unique(pa_norm_term(stopwords))
		rows <- list()
		for (e in sort(unique(dt$era))) {
			sub <- dt[era == e]
			for (f in present) {
				#	Clean and Split
					text <- pa_norm_term(sub[[f]])
					keep <- !is.na(text) & nzchar(text)
					if (!any(keep)) next
					ids <- sub$node_id[keep]
					tokens <- strsplit(text[keep], " ", fixed = TRUE)
					n_per <- lengths(tokens)
					flat <- unlist(tokens, use.names = FALSE)
					owner <- rep(ids, n_per)

				#	Unigrams
					good <- nchar(flat) >= KP_MIN_TOKEN_CHR &
							!(flat %in% stop_set)
					rows[[length(rows) + 1L]] <- data.table(
						node_id = owner[good], era = e, field = f,
						term = flat[good])

				#	Bigrams, Formed Before Stopwords Are Dropped
					if (ngram_max >= 2L && length(flat) > 1L) {
						left  <- flat[-length(flat)]
						right <- flat[-1]
						same  <- owner[-length(owner)] == owner[-1]
						usable <- same &
								  !(left %in% stop_set) &
								  !(right %in% stop_set) &
								  nchar(left) >= KP_MIN_TOKEN_CHR &
								  nchar(right) >= KP_MIN_TOKEN_CHR
						if (any(usable)) {
							rows[[length(rows) + 1L]] <- data.table(
								node_id = owner[-length(owner)][usable],
								era = e, field = f,
								term = paste(left[usable], right[usable]))
						}
					}
			}
		}

	#	Early return
		if (length(rows) == 0L) {
			return(data.table(node_id = integer(0), era = integer(0),
							  field = character(0), term = character(0)))
		}

	#	Assembling Result
		out <- unique(rbindlist(rows))
		setorder(out, era, node_id, term)
		return(out)
}

#' @title pa_cognitive_articles
#' @description The articles whose own text calls them cognitive.
#'
#' @param meta The node metadata table.
#' @param pattern Regular expression matched case-insensitively. Default KP_COG_PATTERN.
#' @param fields Metadata columns searched. Default KP_TEXT_FIELDS.
#' @param eras Integer eras to include. Default KP_ERAS.
#'
#' @details
#' The anchor is the field naming itself. "cognit" matches cognitive, cognition and
#' cognitively, which is what an author writes when the work is about cognition, and
#' it needs no judgement from us about which terms belong.
#'
#' Searched across title, abstract and both keyword fields rather than keywords
#' alone, because keyword coverage is partial and an article whose title says
#' "cognitive performance" belongs in the set whether or not it also keyworded it.
#'
#' @return An integer vector of node_ids.
#'
#' @examples
#' ids <- pa_cognitive_articles(meta)
#' length(ids)
#'
#' @export
pa_cognitive_articles <- function(meta, pattern = KP_COG_PATTERN,
								  fields = KP_TEXT_FIELDS, eras = KP_ERAS) {
	#	"""
	#	Args:
	#		meta: the node metadata table
	#		pattern: regular expression, matched case-insensitively
	#		fields: metadata columns searched
	#		eras: integer eras to include
	#	Returns:
	#		integer vector of node_ids
	#	"""

	#	Validation
		present <- intersect(fields, names(meta))
		if (length(present) == 0L) {
			stop("pa_cognitive_articles: none of these columns exist: ",
				 paste(fields, collapse = ", "))
		}

	#	Search Every Field
		dt <- as.data.table(meta)[is_article & era %in% as.integer(eras)]
		hit <- rep(FALSE, nrow(dt))
		for (f in present) {
			value <- dt[[f]]
			hit <- hit | (!is.na(value) &
						  grepl(pattern, value, ignore.case = TRUE,
								perl = TRUE))
		}

	#	Assembling Result
		return(sort(unique(dt$node_id[hit])))
}

#' @title pa_set_discriminator
#' @description Which terms are commoner inside one set of articles than outside it.
#'
#' @param terms A long term table with node_id and term.
#' @param class_ids Article ids in the class.
#' @param contrast_ids Article ids to compare against, or NULL for every other article.
#' @param min_articles Minimum articles carrying the term. Default KP_MIN_ARTICLES.
#'
#' @details
#' The whole criterion in one number. A term used by a larger share of the class than
#' of the comparison is an indicator of the class; a term used as much or more outside
#' it is shared vocabulary and says nothing, however much it sounds like it should.
#'
#' Rates are smoothed by half a count on each side. Without it a term appearing twice,
#' both times inside the class, outranks a term appearing four hundred times at three
#' to one, and the head of the ranking fills with noise.
#'
#' @return A data.frame with term, n_class, n_contrast, rate_class, rate_contrast and ratio.
#'
#' @examples
#' pa_set_discriminator(terms, cognitive_ids)
#'
#' @export
pa_set_discriminator <- function(terms, class_ids, contrast_ids = NULL,
								 min_articles = KP_MIN_ARTICLES) {
	#	"""
	#	Args:
	#		terms: long term table with node_id and term
	#		class_ids: article ids in the class
	#		contrast_ids: ids to compare against, or NULL for all others
	#		min_articles: minimum articles carrying the term
	#	Returns:
	#		data.frame(term, n_class, n_contrast, rate_class, rate_contrast, ratio)
	#	"""

	#	Scope
		dt <- unique(as.data.table(terms)[, .(node_id, term)])
		class_ids <- unique(as.integer(class_ids))
		if (is.null(contrast_ids)) {
			contrast_ids <- setdiff(unique(dt$node_id), class_ids)
		} else {
			contrast_ids <- setdiff(unique(as.integer(contrast_ids)), class_ids)
		}
		dt <- dt[node_id %in% c(class_ids, contrast_ids)]
		dt[, in_class := node_id %in% class_ids]

	#	The Two Denominators
		n_class    <- length(intersect(class_ids, unique(dt$node_id)))
		n_contrast <- length(intersect(contrast_ids, unique(dt$node_id)))

	#	Early return
		if (n_class == 0L || n_contrast == 0L || nrow(dt) == 0L) {
			return(data.frame(term = character(0), n_class = integer(0),
							  n_contrast = integer(0), rate_class = numeric(0),
							  rate_contrast = numeric(0), ratio = numeric(0),
							  stringsAsFactors = FALSE))
		}

	#	Counts on Each Side
		agg <- dt[, .(n_class    = uniqueN(node_id[in_class]),
					  n_contrast = uniqueN(node_id[!in_class])), by = term]
		agg <- agg[(n_class + n_contrast) >= as.integer(min_articles)]

	#	Rates, Smoothed
		denominator_class    <- n_class + 1
		denominator_contrast <- n_contrast + 1
		agg[, rate_class    := (n_class + 0.5) / denominator_class]
		agg[, rate_contrast := (n_contrast + 0.5) / denominator_contrast]
		agg[, ratio := rate_class / rate_contrast]

	#	Assembling Result
		setorder(agg, -ratio)
		out <- as.data.frame(agg)
		attr(out, "n_class_articles") <- n_class
		attr(out, "n_contrast_articles") <- n_contrast
		return(out)
}

#' @title pa_category_discriminator
#' @description Which terms distinguish category-cognitive articles from the rest.
#'
#' @param meta The node metadata table, carrying research_areas and wos_categories.
#' @param keywords Output of pa_keyword_long.
#' @param categories Category substrings that mark an article cognitive. Default KP_COG_CATEGORIES.
#' @param eras_scored Eras to measure over. Default KP_ERAS_SCORED.
#' @param min_articles Minimum articles carrying the term. Default KP_MIN_ARTICLES.
#'
#' @details
#' This is the instrument the hand-written seed set is not. An article is treated as
#' cognitive when Clarivate's own Research Areas or Web of Science Categories contain
#' one of the given substrings -- a classification assigned by an outside party, on
#' 99.8\% of articles, owing nothing to anybody's reading of this corpus. Every term
#' is then scored by how much more often it appears inside that class than outside.
#'
#' The statistic is a smoothed rate ratio. Raw ratios are unusable at the tail, where
#' a term appearing twice, both times inside the class, would outrank a term appearing
#' four hundred times at three-to-one; the half-count added to each side pulls those
#' back toward one without touching the terms that carry weight. A term above one is
#' over-represented among cognitive articles; below one, under-represented.
#'
#' \strong{The contrast class decides what the ranking can find.} Compared against
#' the whole corpus, a Psychology class is separated by whatever separates psychology
#' from biochemistry, and the winning terms are generic experimental vocabulary.
#' Passing \code{contrast_categories} restricts the comparison so the question
#' becomes nested -- given that an article is psychological, what marks it as the
#' narrower thing -- and the generic terms fall to a ratio near one on their own,
#' because both sides carry them.
#'
#' The result is a candidate lexicon with no term list behind it. It should still be
#' read rather than adopted -- a ratio says a term marks the class, not that the term
#' means what its string suggests.
#'
#' @return A data.frame with term, n_cognitive, n_other, rate_cognitive, rate_other and ratio.
#'
#' @examples
#' disc <- pa_category_discriminator(meta, kw)
#' head(disc, 25)
#'
#' @export
pa_category_discriminator <- function(meta, keywords,
									  categories = KP_COG_CATEGORIES,
									  contrast_categories = NULL,
									  eras_scored = KP_ERAS_SCORED,
									  min_articles = KP_MIN_ARTICLES) {
	#	"""
	#	Args:
	#		meta: node metadata carrying research_areas and wos_categories
	#		keywords: output of pa_keyword_long
	#		categories: category substrings marking an article in the class
	#		contrast_categories: substrings restricting what the class is compared
	#			against, or NULL for the whole corpus
	#		eras_scored: eras to measure over
	#		min_articles: minimum articles carrying the term
	#	Returns:
	#		data.frame(term, n_cognitive, n_other, rate_cognitive, rate_other, ratio)
	#	"""

	#	Validation
		have <- intersect(c("research_areas", "wos_categories"), names(meta))
		if (length(have) == 0L) {
			stop("pa_category_discriminator: the metadata carries neither ",
				 "research_areas nor wos_categories")
		}

	#	Which Articles the Classification Calls Cognitive
		md <- as.data.table(meta)[is_article & era %in% as.integer(eras_scored)]
		pattern <- paste(categories, collapse = "|")
		md[, category_text := ""]
		for (column in have) {
			md[, category_text := paste(category_text,
										ifelse(is.na(get(column)), "",
											   get(column)))]
		}
		md[, is_cognitive := grepl(pattern, category_text, ignore.case = TRUE)]

	#	What the Class Is Compared Against
	#	The default compares the class to the whole corpus, and that comparison
	#	answers a question nobody asked. Psychology against biochemistry is won by
	#	whatever separates psychology from biochemistry -- performance, arousal,
	#	reaction time -- which is generic experimental vocabulary, not cognitive
	#	vocabulary. Nothing specifically cognitive can surface when nothing in the
	#	comparison class is psychological.
	#
	#	Restricting the contrast makes the question nested: given that an article is
	#	psychological, what marks it as the narrower thing? Terms that merely
	#	separate psychology from the rest of the corpus then score near one, because
	#	both sides carry them, and drop out of the ranking by themselves.
		if (!is.null(contrast_categories) && length(contrast_categories) > 0L) {
			contrast_pattern <- paste(contrast_categories, collapse = "|")
			md[, in_contrast := grepl(contrast_pattern, category_text,
									  ignore.case = TRUE)]
			md <- md[is_cognitive | in_contrast]
		}

	#	Early return
		if (sum(md$is_cognitive) == 0L) {
			return(data.frame(term = character(0), n_cognitive = integer(0),
							  n_other = integer(0), rate_cognitive = numeric(0),
							  rate_other = numeric(0), ratio = numeric(0),
							  stringsAsFactors = FALSE))
		}

	#	Term Counts on Each Side
		kw <- unique(as.data.table(keywords)[era %in% as.integer(eras_scored),
											 .(node_id, term)])
		kw[md, on = "node_id", is_cognitive := i.is_cognitive]
		kw <- kw[!is.na(is_cognitive)]

	#	The Two Denominators
	#	Named apart from the column names on purpose. A scalar called n_other beside
	#	a column called n_other resolves to the COLUMN inside a data.table
	#	expression, so the rate would have been computed against itself.
		n_cognitive_articles <- uniqueN(kw$node_id[kw$is_cognitive])
		n_other_articles     <- uniqueN(kw$node_id[!kw$is_cognitive])

	#	Early return
		if (n_cognitive_articles == 0L || n_other_articles == 0L) {
			return(data.frame(term = character(0), n_cognitive = integer(0),
							  n_other = integer(0), rate_cognitive = numeric(0),
							  rate_other = numeric(0), ratio = numeric(0),
							  stringsAsFactors = FALSE))
		}

	#	Rates, Smoothed
	#	Half a count on each side. Without it a term seen twice, both times inside
	#	the class, outranks a term seen four hundred times at three to one.
		agg <- kw[, .(n_cognitive = uniqueN(node_id[is_cognitive]),
					  n_other     = uniqueN(node_id[!is_cognitive])), by = term]
		agg <- agg[(n_cognitive + n_other) >= as.integer(min_articles)]
		denominator_cognitive <- n_cognitive_articles + 1
		denominator_other     <- n_other_articles + 1
		agg[, rate_cognitive := (n_cognitive + 0.5) / denominator_cognitive]
		agg[, rate_other     := (n_other + 0.5) / denominator_other]
		agg[, ratio := rate_cognitive / rate_other]

	#	Assembling Result
		setorder(agg, -ratio)
		out <- as.data.frame(agg)
		attr(out, "n_cognitive_articles") <- n_cognitive_articles
		attr(out, "n_other_articles") <- n_other_articles
		return(out)
}

#' @title pa_term_persistence
#' @description How many eras a term appears in, and at what rate.
#'
#' @param keywords Output of pa_keyword_long.
#' @param eras_scored Eras counted toward persistence. Default KP_ERAS_SCORED.
#' @param min_articles Minimum articles carrying the term. Default KP_MIN_ARTICLES.
#'
#' @details
#' A term is persistent when it appears in every scored era, and the rate says
#' whether it is central or marginal there. Both are reported because they come
#' apart: a term can appear in all four eras on a handful of articles each, which is
#' persistent and peripheral, and a measure built on it would be noisy rather than
#' wrong.
#'
#' Rates are shares of articles that have any keyword in that era, not of all
#' articles, so the figure does not move with keyword coverage.
#'
#' @return A data.frame with term, n_eras, n_articles, mean_rate and min_rate.
#'
#' @examples
#' pers <- pa_term_persistence(kw)
#' head(pers[order(-pers$n_articles), ], 20)
#'
#' @export
pa_term_persistence <- function(keywords, eras_scored = KP_ERAS_SCORED,
								min_articles = KP_MIN_ARTICLES) {
	#	"""
	#	Args:
	#		keywords: output of pa_keyword_long
	#		eras_scored: eras counted toward persistence
	#		min_articles: minimum articles carrying the term
	#	Returns:
	#		data.frame(term, n_eras, n_articles, mean_rate, min_rate)
	#	"""

	#	Early return
		if (nrow(keywords) == 0L) {
			return(data.frame(term = character(0), n_eras = integer(0),
							  n_articles = integer(0), mean_rate = numeric(0),
							  min_rate = numeric(0), stringsAsFactors = FALSE))
		}

	#	The Denominator: Articles With Any Keyword, per Era
		dt <- as.data.table(keywords)[era %in% as.integer(eras_scored)]
		denom <- dt[, .(n_with_keywords = uniqueN(node_id)), by = era]

	#	Per Term and Era
		per <- dt[, .(n_articles = uniqueN(node_id)), by = .(term, era)]
		per[denom, on = "era", rate := n_articles / i.n_with_keywords]

	#	Aggregate Across Eras
		agg <- per[, .(n_eras     = .N,
					   n_articles = sum(n_articles),
					   mean_rate  = mean(rate),
					   min_rate   = min(rate)), by = term]
		agg <- agg[n_articles >= as.integer(min_articles)]

	#	Assembling Result
		setorder(agg, -n_articles)
		return(as.data.frame(agg))
}

#' @title pa_seed_adjacency
#' @description For each term, the share of its articles that also carry a seed term.
#'
#' @param keywords Output of pa_keyword_long.
#' @param seed Seed terms. Default KP_SEED.
#' @param eras_scored Eras to measure over. Default KP_ERAS_SCORED.
#' @param min_articles Minimum articles carrying the term. Default KP_MIN_ARTICLES.
#'
#' @details
#' This is the statistic the hand-written lexicon could not have had. A term's sense
#' is carried by its company, so the question is not whether "memory" is a cognitive
#' word -- it is both -- but what share of the articles using it are doing cognitive
#' work. Enzyme inhibition and sustained attention do not co-occur; working memory
#' and attention do.
#'
#' The baseline matters for reading it. If a third of all keyworded articles carry
#' some seed term, then a term at 35\% adjacency is telling you nothing, and the lift
#' column is what separates that from a term at 90\%. Seed terms themselves are
#' reported with adjacency 1 and excluded from the lift calculation.
#'
#' @return A data.frame with term, n_articles, n_adjacent, adjacency, lift and is_seed.
#'
#' @examples
#' adj <- pa_seed_adjacency(kw)
#' adj[adj$term %in% c("memory", "inhibition"), ]
#'
#' @export
pa_seed_adjacency <- function(keywords, seed = KP_SEED,
							  eras_scored = KP_ERAS_SCORED,
							  min_articles = KP_MIN_ARTICLES) {
	#	"""
	#	Args:
	#		keywords: output of pa_keyword_long
	#		seed: seed terms
	#		eras_scored: eras to measure over
	#		min_articles: minimum articles carrying the term
	#	Returns:
	#		data.frame(term, n_articles, n_adjacent, adjacency, lift, is_seed)
	#	"""

	#	Early return
		if (nrow(keywords) == 0L) {
			return(data.frame(term = character(0), n_articles = integer(0),
							  n_adjacent = integer(0), adjacency = numeric(0),
							  lift = numeric(0), is_seed = logical(0),
							  stringsAsFactors = FALSE))
		}

	#	Which Articles Carry Any Seed Term
		dt <- as.data.table(keywords)[era %in% as.integer(eras_scored)]
		seed_norm <- pa_norm_term(seed)
		seed_articles <- unique(dt[term %in% seed_norm, node_id])
		dt[, adjacent := node_id %in% seed_articles]

	#	The Baseline Rate
		all_articles <- uniqueN(dt$node_id)
		baseline <- if (all_articles == 0L) NA_real_ else
						length(seed_articles) / all_articles

	#	Per Term
		agg <- dt[, .(n_articles = uniqueN(node_id),
					  n_adjacent = uniqueN(node_id[adjacent])), by = term]
		agg <- agg[n_articles >= as.integer(min_articles)]
		agg[, adjacency := n_adjacent / n_articles]
		agg[, is_seed := term %in% seed_norm]

	#	Lift Against the Baseline
	#	A plain if, not ifelse. The test is length one, and ifelse returns a value
	#	the length of its TEST -- so it computed the first row's lift and recycled
	#	it to every term, giving inhibition the same figure as performance.
		if (is.na(baseline) || baseline == 0) {
			agg[, lift := NA_real_]
		} else {
			agg[, lift := adjacency / baseline]
		}
		agg[is_seed == TRUE, lift := NA_real_]

	#	Assembling Result
		setorder(agg, -adjacency, -n_articles)
		out <- as.data.frame(agg)
		attr(out, "baseline") <- baseline
		return(out)
}

#' @title pa_term_company
#' @description The terms that most often accompany a given term.
#'
#' @param keywords Output of pa_keyword_long.
#' @param terms Terms to profile.
#' @param eras_scored Eras to measure over. Default KP_ERAS_SCORED.
#' @param top_n Neighbours to report per term. Default 12.
#'
#' @details
#' Reported for the suspect terms so a reader can see the two senses directly rather
#' than inferring them from a single adjacency number. If "memory" comes back
#' accompanied by working, recall and attention, it is a cognitive term in this
#' corpus; if by antigen and lymphocyte, it is not; if by both, it is conditional and
#' the report says so by showing both.
#'
#' @return A data.frame with term, n_carriers, neighbour, n_shared and share.
#'
#' @examples
#' pa_term_company(kw, c("memory", "inhibition"))
#'
#' @export
pa_term_company <- function(keywords, terms, eras_scored = KP_ERAS_SCORED,
							top_n = 12L) {
	#	"""
	#	Args:
	#		keywords: output of pa_keyword_long
	#		terms: terms to profile
	#		eras_scored: eras to measure over
	#		top_n: neighbours to report per term
	#	Returns:
	#		data.frame(term, n_carriers, neighbour, n_shared, share)
	#	Notes:
	#		n_carriers is the denominator every share is taken over, and it is
	#		printed because without it a 25% that means one article in four looks
	#		identical to one that means two hundred and fifty in a thousand.
	#	"""

	#	Scope
		dt <- as.data.table(keywords)[era %in% as.integer(eras_scored),
									  .(node_id, term)]
		dt <- unique(dt)
		wanted <- pa_norm_term(terms)

	#	Iterating Over the Requested Terms
		rows <- list()
		for (t in wanted) {
			#	Articles Carrying It
				carriers <- unique(dt[term == t, node_id])
				if (length(carriers) == 0L) next

			#	What Else Those Articles Carry
				nb <- dt[node_id %in% carriers & term != t,
						 .(n_shared = uniqueN(node_id)), by = term]
				if (nrow(nb) == 0L) next
				nb[, share := n_shared / length(carriers)]
				setorder(nb, -n_shared)
				nb <- utils::head(nb, as.integer(top_n))
				rows[[length(rows) + 1L]] <- data.table(
					term = t, n_carriers = length(carriers),
					neighbour = nb$term,
					n_shared = nb$n_shared, share = round(nb$share, 3))
		}

	#	Early return
		if (length(rows) == 0L) {
			return(data.frame(term = character(0), n_carriers = integer(0),
							  neighbour = character(0), n_shared = integer(0),
							  share = numeric(0), stringsAsFactors = FALSE))
		}

	#	Assembling Result
		return(as.data.frame(rbindlist(rows)))
}

#' @title pa_cooccurrence_matrix
#' @description Co-occurrence between two sets of terms, as a matrix.
#'
#' @param keywords Output of pa_keyword_long.
#' @param row_terms Terms on the rows.
#' @param col_terms Terms on the columns, or NULL to reuse row_terms.
#' @param measure "conditional" or "jaccard". Default "conditional".
#' @param eras_scored Eras to measure over. Default KP_ERAS_SCORED.
#'
#' @details
#' Two measures, because the two matrices answer different questions.
#' \code{conditional} is the share of the ROW term's articles that also carry the
#' column term. It is asymmetric, and it is what decomposes a candidate's adjacency
#' into the seeds that produce it. \code{jaccard} is the intersection over the union,
#' symmetric and scale-free, which is what makes a square term-by-term matrix
#' comparable across cells of very different frequency.
#'
#' Built as a matrix product on a sparse article-by-term incidence rather than a loop
#' over term pairs: the shared counts are the cross product, and the union follows
#' from the carrier counts by inclusion-exclusion.
#'
#' @return A numeric matrix with row_terms as rows and col_terms as columns.
#'
#' @examples
#' m <- pa_cooccurrence_matrix(kw, KP_SUSPECT, KP_SEED)
#' round(m, 2)
#'
#' @export
pa_cooccurrence_matrix <- function(keywords, row_terms, col_terms = NULL,
								   measure = c("conditional", "jaccard"),
								   eras_scored = KP_ERAS_SCORED) {
	#	"""
	#	Args:
	#		keywords: output of pa_keyword_long
	#		row_terms: terms on the rows
	#		col_terms: terms on the columns, or NULL to reuse row_terms
	#		measure: "conditional" or "jaccard"
	#		eras_scored: eras to measure over
	#	Returns:
	#		numeric matrix, length(row_terms) x length(col_terms)
	#	"""

	#	Validation
		measure <- match.arg(measure)
		rows <- unique(pa_norm_term(row_terms))
		cols <- if (is.null(col_terms)) rows else unique(pa_norm_term(col_terms))

	#	Scope
		dt <- unique(as.data.table(keywords)[era %in% as.integer(eras_scored),
											 .(node_id, term)])
		vocabulary <- union(rows, cols)
		dt <- dt[term %in% vocabulary]

	#	Early return
		if (nrow(dt) == 0L) {
			return(matrix(NA_real_, nrow = length(rows), ncol = length(cols),
						  dimnames = list(rows, cols)))
		}

	#	Sparse Article-by-Term Incidence
		articles <- sort(unique(dt$node_id))
		incidence <- Matrix::sparseMatrix(
			i = match(dt$node_id, articles),
			j = match(dt$term, vocabulary),
			x = 1,
			dims = c(length(articles), length(vocabulary)))

	#	Shared Counts and Carrier Counts
		counts <- as.matrix(Matrix::t(incidence) %*% incidence)
		dimnames(counts) <- list(vocabulary, vocabulary)
		carriers <- diag(counts)

	#	Normalize
		shared <- counts[rows, cols, drop = FALSE]
		if (measure == "conditional") {
			denominator <- carriers[rows]
			out <- sweep(shared, 1,
						 ifelse(denominator == 0, NA_real_, denominator), "/")
		} else {
			union_size <- outer(carriers[rows], carriers[cols], "+") - shared
			out <- shared / ifelse(union_size == 0, NA_real_, union_size)
		}

	#	Assembling Result
		dimnames(out) <- list(rows, cols)
		return(out)
}

#' @title pa_plot_cooccurrence
#' @description Draw a co-occurrence matrix as a sequential heatmap.
#'
#' @param mat A matrix from pa_cooccurrence_matrix.
#' @param path Destination PNG.
#' @param title Plot title.
#' @param subtitle One line under the title saying what a cell means.
#' @param label_min Cells at or above this value get a printed number. Default KP_PLOT_LABEL_MIN.
#'
#' @details
#' Magnitude, so a single-hue sequential ramp light to dark, the lightest step
#' meaning near zero. A colour bar carries the scale, because a cell's position says
#' nothing about its value. Cells are separated by hairlines in the surface colour
#' rather than butted together, so a run of similar values still reads as discrete
#' cells.
#'
#' Numbers are printed selectively rather than on every cell -- a number in all three
#' hundred cells is a table pretending to be a chart. The threshold keeps labels on
#' the cells a reader is actually deciding about, and the label flips to the surface
#' colour on dark cells so it stays legible.
#'
#' Axis text is in ink, never in the ramp's colour: the cell carries the magnitude and
#' the label carries the name, and tinting the label would imply the name has a value.
#'
#' @return Invisibly, the path written, or NULL when there was nothing to draw.
#'
#' @examples
#' pa_plot_cooccurrence(m, "cooccurrence.png", "Candidates against seeds")
#'
#' @export
pa_plot_cooccurrence <- function(mat, path, title, subtitle = "",
								 label_min = KP_PLOT_LABEL_MIN) {
	#	"""
	#	Args:
	#		mat: matrix from pa_cooccurrence_matrix
	#		path: destination PNG
	#		title: plot title
	#		subtitle: one line saying what a cell means
	#		label_min: cells at or above this value get a printed number
	#	Returns:
	#		invisible path, or NULL
	#	"""

	#	Early return
		if (is.null(mat) || nrow(mat) == 0L || ncol(mat) == 0L ||
			all(is.na(mat)) || max(mat, na.rm = TRUE) <= 0) {
			return(invisible(NULL))
		}

	#	The Diagonal Carries No Information and Owns the Scale
	#	A term's Jaccard with itself is one by construction. Left in, it sets the
	#	top of the colour ramp, and every real value -- which among frequent terms
	#	runs 0 to 0.15 -- collapses into the lightest step. Blanked, so the scale is
	#	set by the comparisons the plot is about.
		if (nrow(mat) == ncol(mat) &&
			identical(rownames(mat), colnames(mat))) {
			diag(mat) <- NA_real_
		}

	#	Prune Empty Rows and Columns
	#	A term with no carriers, or a seed that nothing co-occurs with, draws as a
	#	blank strip across the panel. Several of them and the plot is mostly empty
	#	cells, which reads as a finding rather than as an absent term. The CSV keeps
	#	the full matrix; the picture keeps what has something in it.
		n_row_before <- nrow(mat)
		n_col_before <- ncol(mat)
		row_has <- apply(mat, 1, function(v) any(!is.na(v) & v > 0))
		col_has <- apply(mat, 2, function(v) any(!is.na(v) & v > 0))
		mat <- mat[row_has, col_has, drop = FALSE]
		dropped <- (n_row_before - nrow(mat)) + (n_col_before - ncol(mat))

	#	Early return
		if (nrow(mat) == 0L || ncol(mat) == 0L) {
			return(invisible(NULL))
		}

	#	Say What Was Left Out
	#	On its own line rather than appended to the subtitle, which ran off the
	#	right edge of the device once the counts were long enough.
		dropped_note <- if (dropped > 0L) {
							sprintf("%d empty row(s) and %d empty column(s) not shown",
									n_row_before - nrow(mat),
									n_col_before - ncol(mat))
						} else {
							""
						}

	#	Device, Sized to the Labels Rather Than to the Data
	#	Width has to clear the header text as well as the cells. Sizing from the
	#	matrix alone clipped the subtitle off the right edge, because a short matrix
	#	with a long caption is narrower than its own caption.
		row_label_in <- 0.062 * max(nchar(rownames(mat)))
		col_label_in <- 0.062 * max(nchar(colnames(mat)))
		header_in <- max(0.085 * nchar(title),
						 0.055 * nchar(subtitle),
						 0.050 * nchar(dropped_note))
		width_in  <- max(4.5, row_label_in + 0.34 * ncol(mat) + 1.6,
						 header_in + 1.2)
		height_in <- max(3.2, col_label_in + 0.34 * nrow(mat) + 1.7)
		grDevices::png(path, width = width_in, height = height_in,
					   units = "in", res = 150, bg = KP_SURFACE)
		on.exit(grDevices::dev.off(), add = TRUE)

	#	Layout: the Matrix, Then a Colour Bar
	#	The colour bar is a fixed width, not a fixed share. As a share it grew with
	#	the device and reached nearly two inches on the larger matrix.
		graphics::layout(matrix(c(1, 2), nrow = 1),
						 widths = c(1, min(0.16, 1.0 / width_in)))
		graphics::par(bg = KP_SURFACE, col.axis = KP_INK_MUTED,
					  col.main = KP_INK, family = "sans")
		graphics::par(mar = c(col_label_in * 5 + 1.0, row_label_in * 5 + 1.0,
							  4.4, 0.6))

	#	The Cells
	#	image() fills by column from the bottom, so the matrix is transposed and its
	#	rows reversed to put row one at the top, where a reader looks first.
		top_value <- max(mat, na.rm = TRUE)
		drawn <- t(mat[nrow(mat):1, , drop = FALSE])
		breaks <- seq(0, top_value * 1.0000001,
					  length.out = length(KP_RAMP) + 1L)
		graphics::image(x = seq_len(ncol(mat)), y = seq_len(nrow(mat)),
						z = drawn, col = KP_RAMP, breaks = breaks,
						axes = FALSE, xlab = "", ylab = "")

	#	Hairlines in the Surface Colour, So Cells Stay Discrete
		graphics::abline(v = seq(0.5, ncol(mat) + 0.5, by = 1),
						 col = KP_SURFACE, lwd = 1.6)
		graphics::abline(h = seq(0.5, nrow(mat) + 0.5, by = 1),
						 col = KP_SURFACE, lwd = 1.6)

	#	Labels
		graphics::axis(1, at = seq_len(ncol(mat)), labels = colnames(mat),
					   las = 2, tick = FALSE, cex.axis = 0.68, line = -0.5)
		graphics::axis(2, at = seq_len(nrow(mat)),
					   labels = rev(rownames(mat)), las = 2, tick = FALSE,
					   cex.axis = 0.68, line = -0.5)
	#	Three header lines, placed explicitly. title() puts the main title at its own
	#	default line, which collided with an mtext subtitle, so both are mtext here
	#	and the spacing is stated rather than inherited.
		graphics::mtext(title, side = 3, adj = 0, line = 2.6, cex = 0.95,
						col = KP_INK, font = 1)
		if (nzchar(subtitle)) {
			graphics::mtext(subtitle, side = 3, adj = 0, line = 1.45,
							cex = 0.66, col = KP_INK_MUTED)
		}
		if (nzchar(dropped_note)) {
			graphics::mtext(dropped_note, side = 3, adj = 0, line = 0.5,
							cex = 0.6, col = KP_INK_MUTED)
		}

	#	Selective Numbers
		for (r in seq_len(nrow(mat))) {
			for (c in seq_len(ncol(mat))) {
				value <- mat[r, c]
				if (!is.na(value) && value >= label_min) {
					graphics::text(
						c, nrow(mat) - r + 1L,
						formatC(value, format = "f", digits = 2),
						cex = 0.5,
						col = if (value > 0.55 * top_value) KP_SURFACE
							  else KP_INK)
				}
			}
		}

	#	The Colour Bar
		graphics::par(mar = c(col_label_in * 5 + 1.0, 0.3, 4.4, 2.6))
		graphics::image(x = 1, y = breaks[-1],
						z = matrix(breaks[-1], nrow = 1),
						col = KP_RAMP, breaks = breaks,
						axes = FALSE, xlab = "", ylab = "")
		graphics::axis(4, at = pretty(c(0, top_value), 4), las = 2,
					   tick = FALSE, cex.axis = 0.62, line = -0.7)

	#	Assembling Result
		return(invisible(path))
}

#' @title pa_build_lexicon
#' @description Freeze the discriminator's ranking into a lexicon the covariate lane can read.
#'
#' @param discriminator Output of pa_set_discriminator, the against-everything contrast.
#' @param within_psych The same contrast against psychology alone, or NULL.
#' @param min_ratio Lowest rate ratio admitted. Default KP_LEXICON_MIN_RATIO.
#' @param min_articles Lowest carrying-article count admitted. Default KP_MIN_ARTICLES.
#'
#' @details
#' The anchor terms stay in. Their ratio is circular -- the set was defined by them
#' -- and that circularity is a reason not to cite them as evidence the method
#' works, not a reason to leave them out of a lexicon whose job is to recognise
#' cognitive articles. A cognitive lexicon without the word cognitive would be a
#' strange object. They are flagged instead, which turns the question into a
#' sensitivity: the measure computed from anchors alone against the measure
#' computed from the whole list says how much the discovered terms are buying.
#'
#' psych_concentration asks one question about the term's behaviour OUTSIDE the
#' cognitive set: among non-cognitive articles, is this psychological vocabulary
#' or general-corpus vocabulary? It is the term's rate among non-cognitive
#' psychology articles over its rate among all non-cognitive articles. Above one
#' means that when the term is used outside the cognitive set it is mostly
#' psychology using it -- the experimental-task vocabulary behaves this way,
#' because stroop and working memory outside a caffeine-and-cognition paper are
#' still psychology. Below one means its non-cognitive uses are elsewhere in the
#' corpus, which is how the neurodegeneration vocabulary behaves: alzheimer and
#' amyloid outside the cognitive set are neurology and pharmacology.
#'
#' It is NA for a term with no non-cognitive occurrences at all. Those terms have
#' no outside behaviour to characterise, and giving them a number would be
#' reporting the smoothing constant: both contrasts put the same half-count floor
#' in the denominator, so every such term returns the identical quotient and they
#' tie in a block that looks like a ranking and is not. An earlier version of this
#' column divided the two rate RATIOS instead and did exactly that -- it put the
#' zero-contrast anchors in one indistinguishable tail and ordered the rest mostly
#' by how common they are in the corpus at large.
#'
#' The column is diagnostic. Nothing downstream subsets on it: cog_core is
#' computed from the whole lexicon, so no threshold here has to be defended.
#'
#' An earlier version clustered the lexicon by within-set co-occurrence instead.
#' It is gone because it did not work: in a set of 1,341 articles where the top
#' term carries 1,229 of them, Jaccard between terms is near zero almost
#' everywhere, so average linkage peeled off three terms and left the other 152
#' in one group. A two-way cut of that tree is not a finding about the vocabulary.
#'
#' @return A data.frame with term, n_class, n_contrast, ratio, is_anchor,
#'   ratio_psych and psych_concentration.
#'
#' @examples
#' lex <- pa_build_lexicon(text_discriminator, text_within_psych)
#' lex[order(-lex$psych_concentration), c("term", "ratio", "psych_concentration")]
#'
#' @export
pa_build_lexicon <- function(discriminator, within_psych = NULL,
							 min_ratio = KP_LEXICON_MIN_RATIO,
							 min_articles = KP_MIN_ARTICLES) {
	#	"""
	#	Args:
	#		discriminator: output of pa_set_discriminator
	#		within_psych: the same contrast against psychology alone, or NULL
	#		min_ratio: lowest rate ratio admitted
	#		min_articles: lowest carrying-article count admitted
	#	Returns:
	#		data.frame(term, n_class, n_contrast, ratio, is_anchor,
	#				   ratio_psych, psych_concentration)
	#	"""

	#	Validation
		need <- c("term", "n_class", "ratio")
		missing <- setdiff(need, names(discriminator))
		if (length(missing) > 0L) {
			stop("pa_build_lexicon: the discriminator is missing: ",
				 paste(missing, collapse = ", "))
		}

	#	Admit on Both Strength and Support
	#	A high ratio on four articles is noise; a low ratio on four hundred is
	#	shared vocabulary. Both floors are needed.
		lex <- as.data.frame(discriminator)
		lex <- lex[!is.na(lex$ratio) & lex$ratio >= min_ratio &
				   lex$n_class >= as.integer(min_articles), , drop = FALSE]

	#	Early return
		if (nrow(lex) == 0L) {
			lex$ratio_psych <- numeric(0)
			lex$psych_concentration <- numeric(0)
			return(lex)
		}

	#	Flag the Anchors
		if (!("is_anchor" %in% names(lex))) {
			lex$is_anchor <- grepl(KP_COG_PATTERN, lex$term, ignore.case = TRUE,
								   perl = TRUE)
		}

	#	Where the Term Lives Outside the Cognitive Set
	#	Both contrasts carry the term's rate among the articles it is being
	#	contrasted against. Their quotient is the whole measure: the rate among
	#	non-cognitive psychology over the rate among all non-cognitive articles.
		lex$ratio_psych <- NA_real_
		lex$psych_concentration <- NA_real_
		if (!is.null(within_psych) && nrow(within_psych) > 0L &&
			all(c("term", "ratio", "rate_contrast") %in% names(within_psych)) &&
			"rate_contrast" %in% names(lex)) {
			where <- match(lex$term, within_psych$term)
			lex$ratio_psych <- as.numeric(within_psych$ratio[where])
			concentration <- as.numeric(within_psych$rate_contrast[where]) /
							 as.numeric(lex$rate_contrast)

			#	Silence the Terms With No Outside Behaviour
			#	A term absent from every non-cognitive article has the smoothing
			#	floor in both numerator and denominator, so the quotient is the
			#	ratio of the two floors and says nothing about the term.
				concentration[!is.finite(concentration) |
							  as.integer(lex$n_contrast) == 0L] <- NA_real_
				lex$psych_concentration <- round(concentration, 4)
		}

	#	Assembling Result
		rownames(lex) <- NULL
		return(lex[order(-lex$ratio), , drop = FALSE])
}

#' @title pa_run_keyword_profile
#' @description Build the keyword profile and write its tables.
#'
#' @param meta_file Path to the node metadata. Default KP_META_FILE.
#' @param output_dir Destination. Default KP_OUT_DIR.
#' @param verbose Report progress.
#'
#' @details
#' Writes four tables. keyword_coverage records how many articles and terms each era
#' contributes, which is the context every other number has to be read against.
#' term_persistence and seed_adjacency are the two axes a lexicon decision runs on.
#' term_company is the qualitative check on the suspect terms.
#'
#' @return Invisibly, a list with keywords, persistence, adjacency, company and tests.
#'
#' @examples
#' res <- pa_run_keyword_profile()
#' res$adjacency[1:20, ]
#'
#' @export
pa_run_keyword_profile <- function(meta_file = KP_META_FILE,
								   output_dir = KP_OUT_DIR, verbose = TRUE) {
	#	"""
	#	Args:
	#		meta_file: path to the node metadata
	#		output_dir: destination
	#		verbose: report progress
	#	Returns:
	#		invisible list(keywords, persistence, adjacency, company, tests)
	#	"""

	#	Validation
		if (!file.exists(meta_file)) {
			stop("pa_run_keyword_profile: node metadata not found: ", meta_file,
				 ". Run PathA_NodeMetadata.R first.")
		}

	#	Load
		env <- new.env(parent = emptyenv())
		found <- load(meta_file, envir = env)
		meta <- as.data.frame(get(found[1], envir = env, inherits = FALSE))
		if (verbose) {
			cat("Metadata: ", nrow(meta), " nodes, ", ncol(meta), " columns\n",
				sep = "")
		}

	#	Keywords, Long
		keywords <- pa_keyword_long(meta)
		if (verbose) {
			cat("Keyword rows: ", nrow(keywords), " over ",
				uniqueN(as.data.table(keywords)$node_id), " article(s)\n", sep = "")
		}

	#	Coverage per Era, Which Frames Everything Else
		kw <- as.data.table(keywords)
		arts <- as.data.table(meta)[is_article & era %in% KP_ERAS,
									.(articles = .N), by = era]
		coverage <- kw[, .(with_keywords = uniqueN(node_id),
						   distinct_terms = uniqueN(term),
						   term_rows = .N), by = era]
		coverage <- merge(arts, coverage, by = "era", all.x = TRUE)
		coverage[, pct_with_keywords := round(100 * with_keywords / articles, 1)]
		setorder(coverage, era)

	#	The Two Axes
		persistence <- pa_term_persistence(keywords)
		adjacency   <- pa_seed_adjacency(keywords)
		baseline    <- attr(adjacency, "baseline")

	#	The Text Anchor, Which Is the Primary Instrument
	#	Articles whose own title, abstract or keywords say cognitive, against
	#	everything else; then against psychology alone, which asks the sharper
	#	question of what separates cognitive work from its own neighbours rather
	#	than from biochemistry.
		terms <- pa_text_terms(meta)
		cognitive_ids <- pa_cognitive_articles(meta)
		if (verbose) {
			cat("Text terms: ", nrow(terms), " row(s), ",
				uniqueN(as.data.table(terms)$term), " distinct\n", sep = "")
			cat("Cognitive set: ", length(cognitive_ids), " article(s) whose text ",
				"matches \"", KP_COG_PATTERN, "\"\n", sep = "")
		}

		text_discriminator <- pa_set_discriminator(terms, cognitive_ids)

	#	The Same Contrast, Against Psychology Alone
		psych_ids <- tryCatch({
			md <- as.data.table(meta)[is_article & era %in% KP_ERAS]
			have <- intersect(c("research_areas", "wos_categories"), names(md))
			if (length(have) == 0L) integer(0) else {
				text <- rep("", nrow(md))
				for (column in have) {
					text <- paste(text, ifelse(is.na(md[[column]]), "",
											   md[[column]]))
				}
				md$node_id[grepl(paste(KP_CONTRAST_CATEGORIES, collapse = "|"),
								 text, ignore.case = TRUE)]
			}
		}, error = function(e) integer(0))
		text_within_psych <- if (length(psych_ids) > 0L) {
								 pa_set_discriminator(terms, cognitive_ids,
													  contrast_ids = psych_ids)
							 } else {
								 text_discriminator[0, , drop = FALSE]
							 }

	#	Mark the Anchor Terms
	#	The set is defined by them, so their ratio is a property of the definition
	#	rather than a finding. Marked rather than removed: seeing them at the top
	#	confirms the contrast is pointed the right way.
		for (name in c("text_discriminator", "text_within_psych")) {
			frame <- get(name)
			if (nrow(frame) > 0L) {
				frame$is_anchor <- grepl(KP_COG_PATTERN, frame$term,
										 ignore.case = TRUE, perl = TRUE)
				assign(name, frame)
			}
		}

	#	What Distinguishes Category-Cognitive Articles
	#	Kept as a secondary view. The categories cannot resolve cognitive work, only
	#	psychological work, which is why the text anchor leads.
		discriminator <- pa_category_discriminator(meta, keywords)
		nested <- pa_category_discriminator(
			meta, keywords,
			categories = KP_NARROW_CATEGORIES,
			contrast_categories = KP_CONTRAST_CATEGORIES)
	#	The Derived Seed Set
	#	Taken from the text contrast against psychology where that exists, since it
	#	is the sharpest of the three, and with the anchor terms dropped because they
	#	define the set rather than describe it.
		seed_source <- if (nrow(text_within_psych) > 0L) text_within_psych
					   else if (nrow(text_discriminator) > 0L) text_discriminator
					   else nested
		if (nrow(seed_source) > 0L && "is_anchor" %in% names(seed_source)) {
			seed_source <- seed_source[!seed_source$is_anchor, , drop = FALSE]
		}
		derived_seed <- utils::head(seed_source$term, KP_DERIVED_SEED_N)

	#	The Frozen Lexicon
	#	Built from the against-everything contrast rather than the within-psychology
	#	one. The within-psychology contrast is sharper about what separates cognitive
	#	work from the rest of psychology, which is the right question for reading the
	#	field and the wrong one for a lexicon: it returns neurodegeneration vocabulary
	#	because psychology already carries the task terms, and a lexicon that drops
	#	the task terms will not recognise an experimental community.
		lexicon <- pa_build_lexicon(text_discriminator, text_within_psych)

	#	The Suspect Terms, in Detail
		company <- pa_term_company(keywords, KP_SUSPECT)

	#	The Co-occurrence Matrices
	#	Candidates against seeds decomposes the adjacency figure; the square matrix
	#	over the most frequent terms shows the block structure the adjacency figure
	#	cannot. Both are returned as well as drawn, so a reader who wants the numbers
	#	is not reduced to reading them off a picture.
		candidates <- unique(c(pa_norm_term(KP_SUSPECT),
							   utils::head(persistence$term[
								   !persistence$term %in% pa_norm_term(KP_SEED)],
								   KP_PLOT_TOP_N)))
		seed_matrix <- pa_cooccurrence_matrix(keywords, candidates, KP_SEED,
											  measure = "conditional")
	#	Terms Chosen by How Much They Co-occur, Not by How Often They Appear
	#	The most frequent terms are frequent because they are spread across the whole
	#	corpus, which is exactly why they do not co-occur with each other -- the first
	#	run of this plot was a diagonal and a wash. A wider pool is scored on total
	#	off-diagonal overlap and the strongest are kept.
		pool <- utils::head(persistence$term, KP_PLOT_TOP_N * 4L)
		pool_matrix <- pa_cooccurrence_matrix(keywords, pool, NULL,
											  measure = "jaccard")
		if (nrow(pool_matrix) > 0L) {
			off_diagonal <- pool_matrix
			diag(off_diagonal) <- NA_real_
			strength <- rowSums(off_diagonal, na.rm = TRUE)
			top_terms <- names(sort(strength, decreasing = TRUE))[
							seq_len(min(KP_PLOT_TOP_N, length(strength)))]
		} else {
			top_terms <- utils::head(persistence$term, KP_PLOT_TOP_N)
		}
		term_matrix <- pa_cooccurrence_matrix(keywords, top_terms, NULL,
											  measure = "jaccard")

	#	Write
		dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
		readr::write_csv(as.data.frame(coverage),
						 file.path(output_dir, "keyword_coverage.csv"))
		readr::write_csv(persistence,
						 file.path(output_dir, "term_persistence.csv"))
		readr::write_csv(adjacency, file.path(output_dir, "seed_adjacency.csv"))
		readr::write_csv(company, file.path(output_dir, "term_company.csv"))
		readr::write_csv(discriminator,
						 file.path(output_dir, "category_discriminator.csv"))
		readr::write_csv(nested,
						 file.path(output_dir, "category_discriminator_nested.csv"))
		readr::write_csv(text_discriminator,
						 file.path(output_dir, "text_discriminator.csv"))
		readr::write_csv(text_within_psych,
						 file.path(output_dir, "text_discriminator_within_psychology.csv"))
		readr::write_csv(lexicon, file.path(output_dir, "cognitive_lexicon.csv"))
		readr::write_csv(
			cbind(data.frame(term = rownames(seed_matrix),
							 stringsAsFactors = FALSE),
				  as.data.frame(round(seed_matrix, 4))),
			file.path(output_dir, "cooccurrence_candidates_by_seed.csv"))
		readr::write_csv(
			cbind(data.frame(term = rownames(term_matrix),
							 stringsAsFactors = FALSE),
				  as.data.frame(round(term_matrix, 4))),
			file.path(output_dir, "cooccurrence_top_terms.csv"))

	#	Draw
		plots <- character(0)
		if (isTRUE(KP_WRITE_PLOTS)) {
			p1 <- pa_plot_cooccurrence(
				seed_matrix,
				file.path(output_dir, "cooccurrence_candidates_by_seed.png"),
				title = "Candidate terms against the cognitive seed set",
				subtitle = sprintf("Share of the row's articles carrying the column term, eras %d-%d",
								   min(KP_ERAS_SCORED), max(KP_ERAS_SCORED)))
			p2 <- pa_plot_cooccurrence(
				term_matrix,
				file.path(output_dir, "cooccurrence_top_terms.png"),
				title = "The most persistent terms against each other",
				subtitle = sprintf("Jaccard overlap of the two article sets, eras %d-%d; diagonal blank",
								   min(KP_ERAS_SCORED), max(KP_ERAS_SCORED)))
			plots <- c(p1, p2)
		}

	#	Report
		if (verbose) {
			cat("\nKeyword coverage by era\n")
			print(as.data.frame(coverage), row.names = FALSE)

			cat("\nSeed-term baseline: ",
				sprintf("%.1f%%", 100 * baseline),
				" of keyworded articles carry at least one seed term\n", sep = "")

			cat("\nMost frequent terms overall, before any seeding\n")
			print(utils::head(persistence[, c("term", "n_articles", "n_eras",
											  "mean_rate")], KP_TOP_N),
				  row.names = FALSE)

			cat("\nTerms commoner inside the cognitive set than outside it\n")
			cat("  ", attr(text_discriminator, "n_class_articles"),
				" cognitive article(s); ",
				attr(text_discriminator, "n_contrast_articles"),
				" others\n", sep = "")
			print(utils::head(text_discriminator, KP_TOP_N), row.names = FALSE)

			if (nrow(text_within_psych) > 0L) {
				cat("\nThe same contrast against psychology alone\n")
				cat("  ", attr(text_within_psych, "n_class_articles"),
					" cognitive article(s); ",
					attr(text_within_psych, "n_contrast_articles"),
					" other psychology article(s)\n", sep = "")
				print(utils::head(text_within_psych, KP_TOP_N), row.names = FALSE)
			}

			cat("\nTerms that distinguish category-cognitive articles\n")
			cat("  ", attr(discriminator, "n_cognitive_articles"),
				" article(s) carry ", paste(KP_COG_CATEGORIES, collapse = " or "),
				"; ", attr(discriminator, "n_other_articles"), " do not\n",
				sep = "")
			print(utils::head(discriminator, KP_TOP_N), row.names = FALSE)

			cat("\nWithin psychology, what marks Psychology, Experimental\n")
			if (nrow(nested) == 0L) {
				cat("  the narrow class is empty; no nested contrast\n")
			} else {
				cat("  ", attr(nested, "n_cognitive_articles"),
					" article(s) in the narrow class; ",
					attr(nested, "n_other_articles"),
					" in the rest of psychology\n", sep = "")
				print(utils::head(nested, KP_TOP_N), row.names = FALSE)
			}

			cat("\nThe frozen lexicon\n")
			if (nrow(lexicon) == 0L) {
				cat("  empty at ratio >= ", KP_LEXICON_MIN_RATIO,
					" and ", KP_MIN_ARTICLES, " article(s)\n", sep = "")
			} else {
				cat("  ", nrow(lexicon), " term(s) at ratio >= ",
					KP_LEXICON_MIN_RATIO, " and >= ", KP_MIN_ARTICLES,
					" article(s); ", sum(lexicon$is_anchor),
					" anchor(s), flagged not dropped\n", sep = "")
				outside <- lexicon[!is.na(lexicon$psych_concentration), ,
								   drop = FALSE]
				cat("  ", nrow(lexicon) - nrow(outside),
					" term(s) never appear outside the cognitive set and have ",
					"no outside behaviour to read\n", sep = "")
				if (nrow(outside) > 0L) {
					outside <- outside[order(-outside$psych_concentration), ,
									   drop = FALSE]
					cat("  outside the set, used mostly by psychology: ",
						paste(utils::head(outside$term, 12L), collapse = ", "),
						"\n", sep = "")
					cat("  outside the set, used elsewhere in the corpus: ",
						paste(utils::head(rev(outside$term), 12L),
							  collapse = ", "), "\n", sep = "")
				}
			}

			cat("\nHand-written seed terms the data did not rank highly\n")
			missed <- setdiff(pa_norm_term(KP_SEED), derived_seed)
			cat("  ", if (length(missed) == 0L) "none" else
					  paste(missed, collapse = ", "), "\n", sep = "")

			cat("\nMost persistent terms, present in all scored eras\n")
			p <- persistence[persistence$n_eras == length(KP_ERAS_SCORED), ]
			print(utils::head(p, KP_TOP_N), row.names = FALSE)

			cat("\nHighest seed adjacency, non-seed terms\n")
			a <- adjacency[!adjacency$is_seed, ]
			print(utils::head(a, KP_TOP_N), row.names = FALSE)

			cat("\nThe suspect terms\n")
			s <- adjacency[adjacency$term %in% pa_norm_term(KP_SUSPECT), ]
			print(s, row.names = FALSE)

			cat("\nCandidates against the seed set, share of the row's articles\n")
			shown <- seed_matrix[rownames(seed_matrix) %in%
								 pa_norm_term(KP_SUSPECT), , drop = FALSE]
			shown <- shown[, colSums(shown, na.rm = TRUE) > 0, drop = FALSE]
			if (nrow(shown) > 0L && ncol(shown) > 0L) {
				print(round(shown, 2))
			}

			if (length(plots) > 0L) {
				cat("\nWrote ", length(plots), " plot(s):\n", sep = "")
				for (f in plots) cat("  ", f, "\n", sep = "")
			}

			cat("\nWhat the suspect terms appear with\n")
			for (t in unique(company$term)) {
				sub <- company[company$term == t, ]
				cat("  ", t, " [", sub$n_carriers[1], " article(s)]: ",
					paste(sprintf("%s (%.0f%%)", sub$neighbour,
								  100 * sub$share), collapse = ", "),
					"\n", sep = "")
			}
		}

	#	Checks
		tests <- pa_run_keyword_profile_tests(keywords, persistence, adjacency,
											  coverage, output_dir = output_dir)
		if (verbose) {
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
		return(invisible(list(keywords = keywords, persistence = persistence,
							  adjacency = adjacency, company = company,
							  coverage = coverage, discriminator = discriminator,
							  nested = nested, terms = terms,
							  cognitive_ids = cognitive_ids,
							  text_discriminator = text_discriminator,
							  text_within_psych = text_within_psych,
							  derived_seed = derived_seed,
							  lexicon = lexicon,
							  seed_matrix = seed_matrix,
							  term_matrix = term_matrix, plots = plots,
							  tests = tests)))
}

#' @title pa_run_keyword_profile_tests
#' @description Verify the keyword profile against quantities it did not use.
#'
#' @param keywords Output of pa_keyword_long.
#' @param persistence Output of pa_term_persistence.
#' @param adjacency Output of pa_seed_adjacency.
#' @param coverage The per-era coverage table.
#' @param output_dir Destination for the test table, or NULL to skip writing.
#'
#' @details
#' The checks worth having are the ones that would catch a silent error in the
#' splitting or the counting: a term can never be carried by more articles than have
#' keywords, adjacency is a share, every seed term that appears must appear with
#' adjacency 1, and the seed set must actually be present -- a seed that matches
#' nothing would make every adjacency figure zero and look like a finding.
#'
#' @return A data.frame of checks.
#'
#' @export
pa_run_keyword_profile_tests <- function(keywords, persistence, adjacency,
										 coverage, output_dir = KP_OUT_DIR) {
	#	"""
	#	Args:
	#		keywords: output of pa_keyword_long
	#		persistence: output of pa_term_persistence
	#		adjacency: output of pa_seed_adjacency
	#		coverage: per-era coverage table
	#		output_dir: destination for the test table, or NULL
	#	Returns:
	#		data.frame of checks
	#	"""

	#	Collect
		out <- list()

	#	The Anchor Does Not Match Recognition
	#	A regression check with a name. The first run used a bare "cognit", which
	#	matches RECOGNITION, and 473 analytical-chemistry articles entered the
	#	cognitive set through molecular recognition and recognition sites. The
	#	words below are the ones that distinguish a working anchor from that one.
		should_match <- c("cognitive", "cognition", "cognitively",
						  "neurocognitive", "metacognition")
		should_not   <- c("recognition", "molecular recognition",
						  "pattern recognition", "recognition sites",
						  "precognition")
		matched_wanted <- grepl(KP_COG_PATTERN, should_match, ignore.case = TRUE,
								perl = TRUE)
		matched_unwanted <- grepl(KP_COG_PATTERN, should_not, ignore.case = TRUE,
								  perl = TRUE)
		out[[length(out) + 1L]] <- pa_check_kp(
			"the anchor matches cognition and not recognition",
			all(matched_wanted) && !any(matched_unwanted),
			sprintf("%d of %d wanted, %d of %d unwanted",
					sum(matched_wanted), length(should_match),
					sum(matched_unwanted), length(should_not)))

	#	The Seed Set Is Present
	#	First, because every adjacency number is meaningless if it is not.
	#	Only a real test when there ARE keywords. With both keyword fields empty --
	#	which is the true state of eras 11 and 12 -- this would report the absent
	#	input as a failure of the seed set.
		seed_norm <- pa_norm_term(KP_SEED)
		present <- intersect(seed_norm, unique(as.data.table(keywords)$term))
		have_keywords <- nrow(keywords) > 0L
		out[[length(out) + 1L]] <- pa_check_kp(
			"seed terms appear in the keyword record",
			if (have_keywords) length(present) > 0L else NA,
			if (have_keywords) {
				sprintf("%d of %d present: %s", length(present),
						length(seed_norm),
						paste(utils::head(present, 6), collapse = ", "))
			} else {
				"no keyword rows; the keyword views are empty by construction"
			})

	#	Every Seed Term Found Has Adjacency One
		if (nrow(adjacency) > 0L && any(adjacency$is_seed)) {
			seeds <- adjacency[adjacency$is_seed, ]
			out[[length(out) + 1L]] <- pa_check_kp(
				"a seed term is adjacent to itself",
				all(abs(seeds$adjacency - 1) < 1e-9),
				sprintf("min %.4f over %d seed term(s)",
						min(seeds$adjacency), nrow(seeds)))
		}

	#	Adjacency Is a Share
		if (nrow(adjacency) > 0L) {
			out[[length(out) + 1L]] <- pa_check_kp(
				"adjacency lies in [0, 1]",
				all(adjacency$adjacency >= 0 & adjacency$adjacency <= 1),
				sprintf("%.3f to %.3f", min(adjacency$adjacency),
						max(adjacency$adjacency)))
			out[[length(out) + 1L]] <- pa_check_kp(
				"adjacent never exceeds the carriers",
				all(adjacency$n_adjacent <= adjacency$n_articles),
				sprintf("max excess %d",
						max(adjacency$n_adjacent - adjacency$n_articles)))
		}

	#	A Term Cannot Beat Its Era's Keyworded Population
		if (nrow(persistence) > 0L && nrow(coverage) > 0L) {
			ceiling_n <- sum(coverage$with_keywords[
								coverage$era %in% KP_ERAS_SCORED], na.rm = TRUE)
			out[[length(out) + 1L]] <- pa_check_kp(
				"no term is carried by more articles than have keywords",
				max(persistence$n_articles) <= ceiling_n,
				sprintf("max %d against %d", max(persistence$n_articles),
						ceiling_n))
		}

	#	The Two Keyword Fields Both Contributed
		fields <- table(as.data.table(keywords)$field)
		out[[length(out) + 1L]] <- pa_check_kp(
			"both keyword fields contributed", NA,
			paste(sprintf("%s %d", names(fields), as.integer(fields)),
				  collapse = ", "))

	#	Assembling Result
		tests <- do.call("rbind", out)
		rownames(tests) <- NULL
		if (!is.null(output_dir)) {
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			readr::write_csv(tests, file.path(output_dir, "keyword_tests.csv"))
		}
		return(tests)
}

####################################
#   PROFILING THE KEYWORD RECORD   #
####################################

#	Run the Lane
#	Runs when the file is executed as a script and not when it is sourced. The guard
#	reads the call stack: a file run under Rscript evaluates at depth 1, while
#	source() evaluates its lines from inside its own call, several frames down.
#
#		Rscript scripts/Path_A/PathA_KeywordProfile.R     runs the lane
#		source("scripts/Path_A/PathA_KeywordProfile.R")   defines the functions only
#
#	Depends on nothing but the node metadata.
	if (sys.nframe() <= 1L) {

		cat("PathA_KeywordProfile.R -- the keyword record\n")
		cat("============================================\n\n")

		#	Preflight
			if (!file.exists(KP_META_FILE)) {
				cat("Node metadata not found at ", KP_META_FILE, "\n",
					"Run PathA_NodeMetadata.R first.\n", sep = "")
				quit(status = 1L)
			}

		#	Run
			t0 <- Sys.time()
			kp <- pa_run_keyword_profile()
			secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

		#	Report
			cat("\n============================================\n")
			cat("Built in ", round(secs, 1), "s\n", sep = "")
			cat("Output: ", KP_OUT_DIR, "\n", sep = "")

		#	Exit Status
			n_fail <- sum(kp$tests$result == "FAIL")
			if (n_fail > 0L) {
				cat("FAILED: ", n_fail, " check(s)\n", sep = "")
				quit(status = 1L)
			}
			cat("OK\n")
	}
