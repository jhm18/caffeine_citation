#	PathA_EraConstruction.R -- Building 7-Year Era Networks from Raw WoS Records
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	2 October 2026
#
#	WHERE THIS SITS. Replaces the single-year era construction that produced the
#	original .net files. The interval analysis (Analyses/interval_analysis.ipynb)
#	established that the median citation lag is 7 years and that a 7-year window
#	captures ~52% of article-reference pairs -- enough to retain network structure
#	while keeping eras manageable and reflective of the field's state in a given
#	period.
#
#	INPUT:  caffeine_articles_20April2021.Rda (raw Web of Science records)
#
#	OUTPUT per era:
#		1. .Rda node list   (article_id, doi, year, era)
#		2. .Rda edge list   (sender_id, target_id, sender_year, target_year, era)
#		3. Pajek .net file  (vertices + arcs)
#
#	The within-era rule: an arc appears in an era's network only when BOTH the
#	citing article and the cited reference have publication years inside the
#	7-year window. This is the construction change motivated by the interval
#	analysis -- the old single-year window discarded ~70% of the citation record
#	and produced disconnected star graphs.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.
#	Large operations use data.table.

################
#   PACKAGES   #
################

#   Load Packages
    library(data.table)

#	Source Pajek I/O
	source("/workspace/caffeine_citation/scripts/RPajekFunctions_30April2023.r")

#####################
#   CONFIGURATION   #
#####################

#	Era width in years
	ERA_WIDTH <- 7L

#	Year range of the corpus
	YEAR_MIN <- 1901L
	YEAR_MAX <- 2021L

#	Output directories
    setwd("/workspace/caffeine_citation")
	OUTPUT_DIR <- "data/eras_7yr"
	PAJEK_DIR  <- "pajek_files/eras_7yr"

#################
#   FUNCTIONS   #
#################

#	Build the Era Lookup Table
	build_era_table <- function(year_min, year_max, width) {
		#	"""
		#	Args:
		#		year_min: first year of the corpus (integer)
		#		year_max: last year of the corpus (integer)
		#		width: era width in years (integer)
		#	Returns:
		#		data.frame(era, year_start, year_end) with one row per era
		#	Notes:
		#		Tiles from year_min. The final era may be shorter than width
		#		if the range is not evenly divisible.
		#	"""

		#	Build boundaries
			starts <- seq(year_min, year_max, by = width)
			ends <- pmin(starts + width - 1L, year_max)

		#	Assemble result
			return(data.frame(
				era = seq_along(starts),
				year_start = starts,
				year_end = ends,
				stringsAsFactors = FALSE
			))
	}

#	Assign a Year to Its Era
	year_to_era <- function(year, era_table) {
		#	"""
		#	Args:
		#		year: integer vector of publication years
		#		era_table: data.frame from build_era_table()
		#	Returns:
		#		integer vector of era numbers (same length as year)
		#	Notes:
		#		Uses findInterval on the era start boundaries. Years outside
		#		the range get NA.
		#	"""

		#	Map via interval lookup
			idx <- findInterval(year, era_table$year_start, rightmost.closed = FALSE)
			era <- era_table$era[idx]

		#	Clip: years beyond the last era's end get NA
			beyond <- year > era_table$year_end[idx]
			era[beyond] <- NA_integer_

		#	Assemble result
			return(era)
	}

#	Extract Articles from file_outputs
	extract_articles <- function(file_outputs) {
		#	"""
		#	Args:
		#		file_outputs: list of chunks, each a list of WoS article records
		#	Returns:
		#		data.table(article_id, doi, year) with one row per article
		#	Notes:
		#		article_id is a sequential integer across the entire corpus.
		#		doi is from the DI field; NA when absent.
		#	"""

		#	Flatten across chunks
			rows <- vector("list", length(file_outputs))
			id_counter <- 0L

			for (ci in seq_along(file_outputs)) {
				chunk <- file_outputs[[ci]]
				chunk_rows <- vector("list", length(chunk))
				for (ai in seq_along(chunk)) {
					art <- chunk[[ai]]
					id_counter <- id_counter + 1L

					#	Publication year
						py_raw <- art$PY
						if (is.null(py_raw)) next
						py <- suppressWarnings(as.integer(py_raw[[1]][2]))
						if (is.na(py)) next

					#	DOI
						di_raw <- art$DI
						doi <- if (!is.null(di_raw)) {
							paste(unlist(di_raw), collapse = " ")
						} else {
							NA_character_
						}
						doi <- sub("^DI ", "", doi)

					chunk_rows[[ai]] <- data.table(
						article_id = id_counter,
						doi = doi,
						year = py
					)
				}
				rows[[ci]] <- rbindlist(chunk_rows)
			}

		#	Assemble result
			return(rbindlist(rows))
	}

#	Extract Citation Pairs from file_outputs
	extract_citations <- function(file_outputs) {
		#	"""
		#	Args:
		#		file_outputs: list of chunks, each a list of WoS article records
		#	Returns:
		#		data.table(sender_id, ref_label, ref_year) with one row per
		#		article-reference pair
		#	Notes:
		#		sender_id matches the article_id from extract_articles().
		#		ref_label is the full CR string (for building the target node list).
		#		ref_year is parsed from the second comma-delimited element of the
		#		CR string; rows where this fails are dropped.
		#	"""

		#	Flatten across chunks
			rows <- vector("list", length(file_outputs))
			id_counter <- 0L

			for (ci in seq_along(file_outputs)) {
				chunk <- file_outputs[[ci]]
				chunk_rows <- vector("list", length(chunk))
				for (ai in seq_along(chunk)) {
					art <- chunk[[ai]]
					id_counter <- id_counter + 1L

					py_raw <- art$PY
					if (is.null(py_raw)) next
					py <- suppressWarnings(as.integer(py_raw[[1]][2]))
					if (is.na(py) || is.null(art$CR)) next

					#	Parse each cited reference
						n_refs <- length(art$CR)
						ref_labels <- character(n_refs)
						ref_years <- integer(n_refs)
						valid <- logical(n_refs)

						for (ri in seq_len(n_refs)) {
							ref_str <- paste(unlist(art$CR[[ri]]), collapse = " ")
							ref_str <- sub("^CR ", "", ref_str)
							ref_labels[ri] <- ref_str

							parts <- trimws(strsplit(ref_str, ",")[[1]])
							if (length(parts) >= 2) {
								yr <- suppressWarnings(as.integer(parts[2]))
								if (!is.na(yr) && yr >= 1800 && yr <= 2025) {
									ref_years[ri] <- yr
									valid[ri] <- TRUE
								}
							}
						}

					if (any(valid)) {
						chunk_rows[[ai]] <- data.table(
							sender_id = id_counter,
							ref_label = ref_labels[valid],
							ref_year = ref_years[valid]
						)
					}
				}
				rows[[ci]] <- rbindlist(chunk_rows)
			}

		#	Assemble result
			return(rbindlist(rows))
	}

#	Build Target Node Table from Unique References
	build_target_nodes <- function(citations) {
		#	"""
		#	Args:
		#		citations: data.table from extract_citations()
		#	Returns:
		#		data.table(target_id, ref_label, ref_year) with one row per
		#		unique reference
		#	Notes:
		#		target_id is assigned by deterministic order (ref_label, ref_year).
		#		IDs start after the maximum article_id to avoid collisions.
		#	"""

		#	Deduplicate references
			targets <- unique(citations[, .(ref_label, ref_year)])
			setorder(targets, ref_label, ref_year)
			targets[, target_id := .I]

		#	Assemble result
			return(targets)
	}

#	Build One Era's Node List
	build_era_nodes <- function(articles, targets, citations, era_table, era_num) {
		#	"""
		#	Args:
		#		articles: data.table from extract_articles(), with era column
		#		targets: data.table from build_target_nodes(), with era column
		#		citations: data.table from extract_citations(), with target_id
		#		era_table: data.frame from build_era_table()
		#		era_num: integer era number
		#	Returns:
		#		data.table(node_id, label, doi, year, role, era)
		#		role is "article" or "reference"
		#	Notes:
		#		Only includes nodes that participate in at least one within-era arc.
		#	"""

		#	Articles in this era
			era_articles <- articles[era == era_num]

		#	Targets in this era
			era_targets <- targets[era == era_num]

		#	Find within-era arcs to restrict to participating nodes
			era_cites <- citations[sender_id %in% era_articles$article_id &
								   target_id %in% era_targets$target_id]

		#	Active senders and targets
			active_senders <- unique(era_cites$sender_id)
			active_targets <- unique(era_cites$target_id)

		#	Article nodes
			art_nodes <- era_articles[article_id %in% active_senders,
				.(node_id = article_id, label = doi, doi = doi,
				  year = year, role = "article")]

		#	Reference nodes
			ref_nodes <- era_targets[target_id %in% active_targets,
				.(node_id = target_id, label = ref_label, doi = NA_character_,
				  year = ref_year, role = "reference")]

		#	Assemble result
			era_nodes <- rbindlist(list(art_nodes, ref_nodes))
			era_nodes[, era := era_num]
			return(era_nodes)
	}

#	Build One Era's Edge List
	build_era_edges <- function(articles, targets, citations, era_num) {
		#	"""
		#	Args:
		#		articles: data.table with era column
		#		targets: data.table with era column
		#		citations: data.table with target_id column
		#		era_num: integer era number
		#	Returns:
		#		data.table(sender_id, target_id, era)
		#	Notes:
		#		Only arcs where both endpoints fall inside the era.
		#	"""

		#	Articles and targets in this era
			era_senders <- articles[era == era_num, article_id]
			era_tgts <- targets[era == era_num, target_id]

		#	Filter citations to within-era arcs
			era_edges <- citations[sender_id %in% era_senders &
								   target_id %in% era_tgts,
								   .(sender_id, target_id)]
			era_edges[, era := era_num]

		#	Assemble result
			return(era_edges)
	}

########################
#   MAIN CONSTRUCTION  #
########################

#	Run the Full Era Construction Pipeline
	run_era_construction <- function(data_path, output_dir = OUTPUT_DIR,
									 pajek_dir = PAJEK_DIR,
									 era_width = ERA_WIDTH,
									 year_min = YEAR_MIN,
									 year_max = YEAR_MAX) {
		#	"""
		#	Args:
		#		data_path: path to caffeine_articles_20April2021.Rda
		#		output_dir: directory for .Rda output files
		#		pajek_dir: directory for .net output files
		#		era_width: width of each era in years (default 7)
		#		year_min: first year of the corpus (default 1901)
		#		year_max: last year of the corpus (default 2021)
		#	Returns:
		#		list(era_table, articles, targets, era_summary) invisibly
		#	Notes:
		#		Creates output directories if they do not exist.
		#		Saves one .Rda and one .net per era, plus the combined
		#		node list and edge list as .Rda files.
		#	"""

		#	Create output directories
			dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
			dir.create(pajek_dir, recursive = TRUE, showWarnings = FALSE)

		#	Build era boundaries
			era_table <- build_era_table(year_min, year_max, era_width)
			cat("Era table:\n")
			print(era_table)

		#	Load data
			cat("\nLoading articles...\n")
			load(data_path)

		#	Extract articles and citations
			cat("Extracting articles...\n")
			articles <- extract_articles(file_outputs)
			cat("  ", nrow(articles), "articles extracted\n")

			cat("Extracting citations...\n")
			citations <- extract_citations(file_outputs)
			cat("  ", nrow(citations), "citation pairs extracted\n")

		#	Build target node table
			cat("Building target nodes...\n")
			targets <- build_target_nodes(citations)
			cat("  ", nrow(targets), "unique references\n")

		#	Offset target IDs to avoid collision with article IDs
			max_art_id <- max(articles$article_id)
			targets[, target_id := target_id + max_art_id]

		#	Add target_id to citations via join on ref_label + ref_year
			citations <- dplyr::left_join(
				citations, targets[, .(ref_label, ref_year, target_id)],
				by = c("ref_label", "ref_year"),
				relationship = "many-to-one"
			)
			setDT(citations)

		#	Assign eras
			articles[, era := year_to_era(year, era_table)]
			targets[, era := year_to_era(ref_year, era_table)]

		#	Construct each era
			cat("\nConstructing eras...\n")
			era_summary <- vector("list", nrow(era_table))

			for (e in era_table$era) {
				#	Build nodes and edges
					era_nodes <- build_era_nodes(articles, targets, citations,
						era_table, e)
					era_edges <- build_era_edges(articles, targets, citations, e)

				#	Report
					n_articles <- era_nodes[role == "article", .N]
					n_refs <- era_nodes[role == "reference", .N]
					n_arcs <- nrow(era_edges)
					cat(sprintf("  Era %2d (%d-%d): %d articles, %d references, %d arcs\n",
						e, era_table$year_start[e], era_table$year_end[e],
						n_articles, n_refs, n_arcs))

				#	Save .Rda files
					era_node_file <- file.path(output_dir,
						sprintf("era%02d_nodes.Rda", e))
					era_edge_file <- file.path(output_dir,
						sprintf("era%02d_edges.Rda", e))
					save(era_nodes, file = era_node_file)
					save(era_edges, file = era_edge_file)

				#	Write Pajek .net via sourced write_net()
					if (n_arcs > 0) {
						#	Remap node_ids to contiguous 1..N for Pajek
							node_ids <- sort(era_nodes$node_id)
							id_map <- setNames(seq_along(node_ids), as.character(node_ids))
							pajek_ids <- id_map[as.character(era_nodes$node_id)]

						#	Prepare write_net arguments
							net_name <- file.path(pajek_dir, sprintf("era%02d", e))
							write_net(
								tie_type     = "Arcs",
								data_id      = era_nodes$label,
								x_coord      = rep("0.0000", nrow(era_nodes)),
								y_coord      = rep("0.0000", nrow(era_nodes)),
								z_coord      = rep("0.5000", nrow(era_nodes)),
								node_color   = "Blue",
								node_border  = "White",
								`person i`   = id_map[as.character(era_edges$sender_id)],
								`person j`   = id_map[as.character(era_edges$target_id)],
								weight       = rep(1, nrow(era_edges)),
								tie_color    = "Gray",
								net_name     = net_name,
								`sort and simplify` = TRUE
							)
					}

				#	Track summary
					era_summary[[e]] <- data.table(
						era = e,
						year_start = era_table$year_start[e],
						year_end = era_table$year_end[e],
						n_articles = n_articles,
						n_references = n_refs,
						n_nodes = n_articles + n_refs,
						n_arcs = n_arcs
					)
			}

		#	Combined summary
			era_summary <- rbindlist(era_summary)
			cat("\nSummary:\n")
			print(era_summary)

		#	Save the era table and summary
			save(era_table, era_summary,
				file = file.path(output_dir, "era_construction_summary.Rda"))

		#	Assemble result
			return(invisible(list(
				era_table = era_table,
				articles = articles,
				targets = targets,
				era_summary = era_summary
			)))
	}

#   Executing Construction
    data_path <- "/workspace/caffeine_citation/data/caffeine_articles_20April2021.Rda"
    run_era_construction(data_path, output_dir = OUTPUT_DIR, pajek_dir = PAJEK_DIR,
						 era_width = ERA_WIDTH, year_min = YEAR_MIN, year_max = YEAR_MAX)

##################
#   EVALUATION   #
##################

#   Examining Era 17
    load("/workspace/caffeine_citation/data/eras_7yr/era17_edges.Rda")
    load("/workspace/caffeine_citation/data/eras_7yr/era17_nodes.Rda")

#   Check File Structure
    head(era_edges)
    head(era_nodes)

#   File Loads in Pajek

#   Constructing List of Node Lists
    era_files <- era_files <- base::list.files("/workspace/caffeine_citation/data/eras_7yr", full.names=TRUE)
    era_nodes_list <- vector("list", 18)
    names(era_nodes_list) <- paste("era_nodes_", seq(1,18,1))

    eras_edge_list <- vector("list", 18)
    names(eras_edge_list) <- paste("era_edges_", seq(1,18,1))

#   Populating Lists
    nodes_files <- era_files[grep("_nodes", era_files)]
    edges_files <- era_files[grep("_edges", era_files)]
    for(i in seq_along(era_nodes_list)){
        #   Load Nodes File
            load(nodes_files[[i]])

        #   Load Edges File
            load(edges_files[[i]])

        #   Populate Lists
            era_nodes_list[[i]] <- era_nodes
            eras_edge_list[[i]] <- era_edges
    }

#   Save Lists
    save(eras_edge_list, file = "/workspace/caffeine_citation/data/eras_7yr/era_7yr_edge_lists.Rda")
    save(era_nodes_list, file = "/workspace/caffeine_citation/data/eras_7yr/era_7yr_node_lists.Rda")
