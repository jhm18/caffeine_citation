#	PathA_EraResolution.R -- Finding Each Era's Files and Mapping Its Vertices
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHERE THIS SITS. Stages 1 and 2 of the pipeline order of operations
#	(specification S14.1). Requires PathA_PajekReaders.R. Feeds PathA_TieLayer.R,
#	which needs one era map per era before it can attribute any citation.
#	Tested by tests/Test_PathA_TieLayer.R.
#
#	The pajek_files tree grew three layouts and two partition naming conventions,
#	verified on disk 8 September 2026:
#
#		eras 1-17	Era<N>/era<N>.net				era<N>_Community.clu     (top level)
#		eras 18-21	era<N>.net       (top level)	era<N>_Community.clu     (top level)
#		eras 22-23	Era<N>/era<N>.net				Era<N>/era<N>_testCommunity.clu
#
#	Resolution is deterministic: candidates are tried in a fixed documented order
#	and the winner is recorded, so a run is reproducible and a layout change shows
#	up as a diff in the manifest rather than as a silent switch of input file.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

#####################
#   CONFIGURATION   #
#####################

#	Integration window (specification S4): eras 12-23, single publication years
	PA_ERA_YEAR <- c("12" = 2009, "13" = 2010, "14" = 2011, "15" = 2012,
					 "16" = 2013, "17" = 2014, "18" = 2015, "19" = 2016,
					 "20" = 2017, "21" = 2018, "22" = 2019, "23" = 2020)

	PA_INTEGRATION_ERAS <- 12:23

#################
#   FUNCTIONS   #
#################

#	Map an Era to Its Publication Year
	pa_era_year <- function(era) {
		#	"""
		#	Args:
		#		era: integer era number
		#	Returns:
		#		integer publication year, or NA for eras outside the annual run
		#	Notes:
		#		Only eras 12-23 are single years and only those are mapped. Era 2 is
		#		also a single year (1945) but is not part of an unbroken run, so it
		#		is deliberately absent -- the integration models never see it.
		#	"""

		#	Assemble result
			return(unname(PA_ERA_YEAR[as.character(era)]))
	}

#	Build the Era Pairs of the Integration Window
	pa_era_pairs <- function(eras = PA_INTEGRATION_ERAS) {
		#	"""
		#	Args:
		#		eras: integer vector of eras, assumed contiguous and ascending
		#	Returns:
		#		data.frame(pair_index, era_prior, era_t, year_prior, year_t, pair_id)
		#	Notes:
		#		Eleven pairs from twelve eras. pair_index is 1..11 and is the column
		#		the specification's design matrices carry as the time trend.
		#	"""

		#	Validation
			eras <- sort(unique(as.integer(eras)))
			if (length(eras) < 2L) stop("Need at least two eras to form a pair")
			if (!all(diff(eras) == 1L)) stop("Era sequence is not contiguous: ", paste(eras, collapse = ", "))

		#	Assemble result
			prior <- eras[-length(eras)]
			curr  <- eras[-1L]
			return(data.frame(pair_index = seq_along(prior),
							  era_prior  = prior,
							  era_t      = curr,
							  year_prior = pa_era_year(prior),
							  year_t     = pa_era_year(curr),
							  pair_id    = paste0(prior, "_", curr),
							  stringsAsFactors = FALSE))
	}

#	Resolve One Era's Input Files
	pa_resolve_era <- function(pajek_dir, era) {
		#	"""
		#	Args:
		#		pajek_dir: root of the pajek_files tree
		#		era: integer era number
		#	Returns:
		#		list(era, net, clu, layout, clu_convention, n_net_candidates,
		#			 n_clu_candidates)
		#	Notes:
		#		Candidate order is fixed. For the network, the Era<N>/ subdirectory
		#		wins over the top level; for the partition, _testCommunity wins over
		#		_Community and the subdirectory wins over the top level. Where more
		#		than one candidate exists the count is reported so a preflight can
		#		flag the ambiguity rather than resolve it silently.
		#	"""

		#	Candidate networks, in priority order
			net_candidates <- c(file.path(pajek_dir, sprintf("Era%d", era), sprintf("era%d.net", era)),
								file.path(pajek_dir, sprintf("era%d.net", era)))

		#	Candidate partitions, in priority order
			clu_candidates <- c(file.path(pajek_dir, sprintf("Era%d", era), sprintf("era%d_testCommunity.clu", era)),
								file.path(pajek_dir, sprintf("Era%d", era), sprintf("era%d_Community.clu", era)),
								file.path(pajek_dir, sprintf("era%d_testCommunity.clu", era)),
								file.path(pajek_dir, sprintf("era%d_Community.clu", era)))

		#	Select
			net_hits <- net_candidates[file.exists(net_candidates)]
			clu_hits <- clu_candidates[file.exists(clu_candidates)]
			if (length(net_hits) == 0L) {
				stop("No .net found for era ", era, ". Looked in:\n  ", paste(net_candidates, collapse = "\n  "))
			}
			if (length(clu_hits) == 0L) {
				stop("No .clu found for era ", era, ". Looked in:\n  ", paste(clu_candidates, collapse = "\n  "))
			}

		#	Describe what we found
			net <- net_hits[[1L]]
			clu <- clu_hits[[1L]]
			layout <- if (basename(dirname(net)) == sprintf("Era%d", era)) "subdir" else "flat"
			convention <- if (grepl("_testCommunity\\.clu$", clu)) "testCommunity" else "Community"

		#	Assemble result
			return(list(era              = as.integer(era),
						net              = net,
						clu              = clu,
						layout           = layout,
						clu_convention   = convention,
						n_net_candidates = length(net_hits),
						n_clu_candidates = length(clu_hits)))
	}

#	Build a Manifest for Every Era in Scope
	pa_era_manifest <- function(pajek_dir, eras = PA_INTEGRATION_ERAS) {
		#	"""
		#	Args:
		#		pajek_dir: root of the pajek_files tree
		#		eras: integer vector of eras to resolve
		#	Returns:
		#		data.frame, one row per era, columns era, year, net, clu, layout,
		#		clu_convention, n_net_candidates, n_clu_candidates, net_bytes,
		#		clu_bytes
		#	Notes:
		#		This is the preflight. It runs before any worker starts, so a missing
		#		or ambiguous input fails the whole run in under a second rather than
		#		twenty minutes into a parallel job. Write it out with the run and a
		#		later reader can see exactly which files produced the results.
		#	"""

		#	Resolve each era
			rows <- lapply(eras, function(e) {
				r <- pa_resolve_era(pajek_dir, e)
				data.frame(era              = r$era,
						   year             = pa_era_year(r$era),
						   net              = r$net,
						   clu              = r$clu,
						   layout           = r$layout,
						   clu_convention   = r$clu_convention,
						   n_net_candidates = r$n_net_candidates,
						   n_clu_candidates = r$n_clu_candidates,
						   net_bytes        = file.size(r$net),
						   clu_bytes        = file.size(r$clu),
						   stringsAsFactors = FALSE)
			})

		#	Assemble result
			return(do.call("rbind", rows))
	}

#	Report Partition-Convention Splits
	pa_manifest_warnings <- function(manifest) {
		#	"""
		#	Args:
		#		manifest: the data.frame returned by pa_era_manifest()
		#	Returns:
		#		character vector of warning strings, empty if nothing is amiss
		#	Notes:
		#		Does not stop the run. The specification (S14.4) records that eras
		#		1-21 and 22-23 use different partition naming and that the current
		#		partitions are development fixtures to be replaced by CHAMP at a
		#		pooled gamma*. This surfaces that state on every run so it cannot be
		#		forgotten, and flags any era where the resolver had a real choice.
		#	"""

		#	Collect
			msgs <- character(0)
			conv <- unique(manifest$clu_convention)
			if (length(conv) > 1L) {
				msgs <- c(msgs, paste0(
					"Partition naming is not uniform across the run: ",
					paste(sprintf("%s (eras %s)", conv,
						vapply(conv, function(k) paste(manifest$era[manifest$clu_convention == k], collapse = ","), "")),
						collapse = "; "),
					". Community granularity is not established as comparable; ",
					"these are development fixtures pending CHAMP at a pooled gamma*."))
			}
			amb <- manifest[manifest$n_net_candidates > 1L | manifest$n_clu_candidates > 1L, ]
			if (nrow(amb) > 0L) {
				msgs <- c(msgs, paste0(
					"More than one candidate input existed for era(s) ",
					paste(amb$era, collapse = ", "),
					"; resolution used the documented priority order."))
			}

		#	Assemble result
			return(msgs)
	}

#	Build the Era Map
	pa_era_map <- function(node_list, vertices, partition, era) {
		#	"""
		#	Args:
		#		node_list: full citation node list
		#		vertices: vertex table from pa_read_net()
		#		partition: integer vector from pa_read_clu()
		#		era: integer era number
		#	Returns:
		#		data.frame(pajek_id, node_id, node_label, community, era, role), with
		#		attribute "vertex_set" giving list(n_vertices, n_era_nodes,
		#		n_nodes_absent_from_network, exact_match)
		#	Notes:
		#		Replaces id_map() from EraPipeline.R.
		#
		#		THE JOIN KEY IS node_id, NOT node_label. The Label column of a Pajek
		#		vertex line in this project holds the node_id as a quoted string, not
		#		the DOI or author_year string that the node list calls node_label:
		#
		#			*Vertices 18256
		#			     1 "8112"    0.0000 0.0000 0.5000 ic Blue bc White
		#
		#		Vertex 1 of era 12 is node_id 8112, whose node_label is
		#		"kojh_2009_molcelltoxicol". id_map() encoded this correctly by
		#		renaming vertex column 2 to node_id; an earlier draft of this function
		#		misread it as node_label and failed on every era. The validation below
		#		names that failure explicitly so it can never cost an hour again.
		#
		#		Two differences from id_map(), both deliberate:
		#
		#		1. Columns are selected by NAME. id_map() used id_index[c(8,1,2,9,7)],
		#		   which silently rewires if the node list ever gains a column.
		#		2. It joins vertices -> node_list, not node_list -> vertices, so that
		#		   an unmappable vertex is an error rather than a quiet row of NA.
		#		   Vertices are what carry community assignments; a vertex we cannot
		#		   identify is a vertex whose community we cannot attribute.
		#
		#		The vertex set is also compared with the era's node set in both
		#		directions and the result returned as an attribute, so the input test
		#		of S14.4 can assert on it rather than assuming it.
		#	"""

		#	Validation
			if (nrow(vertices) != length(partition)) {
				stop("Era ", era, ": ", nrow(vertices), " vertices but ", length(partition), " assignments")
			}

		#	The Label column carries node_id
			raw <- as.character(vertices[["Label"]])
			node_id <- suppressWarnings(as.integer(raw))
			if (anyNA(node_id)) {
				bad <- raw[is.na(node_id)][[1L]]
				stop("Era ", era, ": ", sum(is.na(node_id)), " of ", length(raw),
					 " vertex labels are not integer node ids. First offender: \"", bad, "\". ",
					 "This project's .net files label vertices by node_id; a file labelled ",
					 "by DOI or author_year string was probably exported by a different route.")
			}

		#	Vertex table, named rather than positional
			v <- data.frame(pajek_id  = as.integer(vertices[["ID"]]),
							node_id   = node_id,
							community = as.integer(partition),
							stringsAsFactors = FALSE)

		#	Node list side, reduced to this era
			nl <- node_list[node_list$era == era, c("node_id", "node_label", "role"), drop = FALSE]
			nl <- nl[!duplicated(nl$node_id), , drop = FALSE]

		#	Join on the node id
			out <- dplyr::left_join(v, nl, by = "node_id", relationship = "many-to-one")

		#	Every vertex must resolve
			missing <- sum(is.na(out$node_label))
			if (missing > 0L) {
				stop("Era ", era, ": ", missing, " of ", nrow(out),
					 " network vertices carry a node id absent from era ", era,
					 " of the node list. First unmatched id: ",
					 out$node_id[is.na(out$node_label)][[1L]])
			}

		#	Compare the two vertex sets in both directions
			absent <- length(setdiff(nl$node_id, v$node_id))
			vertex_set <- list(n_vertices                  = nrow(v),
							   n_era_nodes                 = nrow(nl),
							   n_nodes_absent_from_network = absent,
							   exact_match                 = identical(absent, 0L))

		#	Assemble result
			out$era <- as.integer(era)
			out <- out[c("pajek_id", "node_id", "node_label", "community", "era", "role")]
			attr(out, "vertex_set") <- vertex_set
			return(out)
	}
