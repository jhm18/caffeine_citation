#	PathA_PajekReaders.R -- Reading Pajek Networks and Partitions Without Side Effects
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHERE THIS SITS. Support file for the Path A pipeline. Nothing here is a
#	pipeline stage; every stage depends on it. Load order:
#
#		RPajekFunctions_30April2023.r   (existing; supplies read_net)
#		  -> PathA_PajekReaders.R       (this file)
#		  -> PathA_EraResolution.R      (stages 1-2: find the files, map the eras)
#		  -> PathA_TieLayer.R           (stage 3: build the ties)
#
#	Tested by tests/Test_PathA_TieLayer.R.
#
#	Why this file exists. RPajekFunctions_30April2023.r returns its results by
#	assign()-ing `vertices`, `ties`, and `network_partition` into .GlobalEnv, and
#	read_clu() additionally calls setwd(). Both are process-global, which makes any
#	concurrent use of those readers unsafe and makes the determinism test of the
#	specification (S14.4) impossible to write. The wrappers here return values.
#
#	The .net parser is not reimplemented. read_net() is battle-tested and its output
#	format is depended on downstream, so pa_read_net() calls it inside a guard that
#	saves, harvests, and restores the three global names. The guard makes the reader
#	safe to call from separate PROCESSES; it does not make it safe to call from two
#	threads of one process, and nothing in this pipeline does that.
#
#	Conventions: dplyr for joins only, readr for CSV I/O, base R for everything else.

#################
#   CONTENTS    #
#################

#	Names RPajekFunctions writes into .GlobalEnv
	PA_GLOBAL_NAMES <- c("vertices", "ties", "network_partition")

#################
#   FUNCTIONS   #
#################

#	Evaluate With Global Names Protected
	pa_protect_globals <- function(expr, names = PA_GLOBAL_NAMES) {
		#	"""
		#	Args:
		#		expr: an expression to evaluate, unevaluated (promise)
		#		names: character vector of .GlobalEnv names the expression may clobber
		#	Returns:
		#		named list of the values `names` held in .GlobalEnv AFTER evaluation;
		#		an element is NULL if the name was never created
		#	Notes:
		#		Prior bindings are saved before evaluation and restored afterwards,
		#		including the case where a name did not previously exist -- that name
		#		is removed again rather than left behind. Restoration runs on.exit()
		#		so it survives an error inside expr.
		#	"""

		#	Record what was there before
			had <- vapply(names, exists, logical(1L), envir = .GlobalEnv, inherits = FALSE)
			prior <- lapply(names[had], get, envir = .GlobalEnv, inherits = FALSE)
			names(prior) <- names[had]

		#	Restore on the way out, whatever happens
			on.exit({
				for (nm in names) {
					if (nm %in% base::names(prior)) {
						assign(nm, prior[[nm]], envir = .GlobalEnv)
					} else if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
						rm(list = nm, envir = .GlobalEnv)
					}
				}
			}, add = TRUE)

		#	Evaluate for its side effects
			force(expr)

		#	Harvest
			out <- vector("list", length(names))
			names(out) <- names
			for (nm in names) {
				if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
					out[[nm]] <- get(nm, envir = .GlobalEnv, inherits = FALSE)
				}
			}

		#	Assemble result
			return(out)
	}

#	Read a Pajek Partition
	pa_read_clu <- function(clu_path) {
		#	"""
		#	Args:
		#		clu_path: full path to a .clu partition file
		#	Returns:
		#		integer vector of community assignments, one element per vertex
		#	Notes:
		#		Replaces read_clu() outright rather than wrapping it: the original
		#		calls setwd() and writes to .GlobalEnv to deliver two lines of
		#		parsing. The header line is *Vertices <n>; it is dropped, and its
		#		count is checked against the number of assignments that follow.
		#	"""

		#	Validation
			if (!file.exists(clu_path)) stop("Partition file not found: ", clu_path)

		#	Read
			lines <- readLines(clu_path, warn = FALSE)
			if (length(lines) < 2L) stop("Partition file has no assignments: ", clu_path)

		#	Parse the header count, when the header carries one
			header <- lines[[1L]]
			declared <- suppressWarnings(as.integer(sub("^\\s*\\*[Vv]ertices\\s+([0-9]+).*$", "\\1", header)))

		#	Parse the assignments
			body <- trimws(lines[-1L])
			body <- body[nzchar(body)]
			part <- suppressWarnings(as.integer(body))
			if (anyNA(part)) {
				bad <- which(is.na(part))[[1L]]
				stop("Non-integer community assignment at line ", bad + 1L, " of ", clu_path)
			}

		#	Cross-check the declared vertex count
			if (!is.na(declared) && declared != length(part)) {
				stop("Partition header declares ", declared, " vertices but file carries ",
					 length(part), " assignments: ", clu_path)
			}

		#	Assemble result
			return(part)
	}

#	Read a Pajek Network
	pa_read_net <- function(net_path, want_ties = FALSE) {
		#	"""
		#	Args:
		#		net_path: full path to a .net network file
		#		want_ties: return the edge table as well? (default FALSE)
		#	Returns:
		#		list(vertices = data.frame, ties = data.frame or NULL)
		#	Notes:
		#		Delegates parsing to read_net() from RPajekFunctions_30April2023.r,
		#		which must already be sourced. The .GlobalEnv writes that function
		#		performs are captured and undone by pa_protect_globals().
		#
		#		The Path A tie layer does not use the .net edges at all -- ties come
		#		from the citation edge list -- so want_ties defaults to FALSE and the
		#		edge table is dropped to keep the memory profile of a parallel run
		#		down. Era 23's network alone parses to a large object.
		#	"""

		#	Validation
			if (!file.exists(net_path)) stop("Network file not found: ", net_path)
			if (!exists("read_net", mode = "function")) {
				stop("read_net() not found; source RPajekFunctions_30April2023.r first")
			}

		#	Parse under the guard
			captured <- pa_protect_globals(read_net(net_path))
			if (is.null(captured$vertices)) stop("read_net() produced no vertices for ", net_path)

		#	Assemble result
			return(list(vertices = captured$vertices,
						ties     = if (want_ties) captured$ties else NULL))
	}

#	Read a Network and Its Partition Together
	pa_read_era_network <- function(net_path, clu_path, want_ties = FALSE) {
		#	"""
		#	Args:
		#		net_path: full path to the era's .net file
		#		clu_path: full path to the era's .clu partition
		#		want_ties: return the edge table as well? (default FALSE)
		#	Returns:
		#		list(vertices = data.frame, ties = data.frame or NULL,
		#			 partition = integer vector, n_vertices = integer,
		#			 n_communities = integer)
		#	Notes:
		#		Enforces the input test of the specification (S14.4): vertex count
		#		must equal partition length. The original pipeline assumed this and
		#		would have silently misaligned communities had it ever been false.
		#	"""

		#	Read both sides
			net <- pa_read_net(net_path, want_ties = want_ties)
			part <- pa_read_clu(clu_path)

		#	Enforce agreement
			n_v <- nrow(net$vertices)
			if (n_v != length(part)) {
				stop("Vertex/partition length mismatch: ", n_v, " vertices in ",
					 basename(net_path), " but ", length(part), " assignments in ",
					 basename(clu_path))
			}

		#	Assemble result
			return(list(vertices      = net$vertices,
						ties          = net$ties,
						partition     = part,
						n_vertices    = n_v,
						n_communities = length(unique(part))))
	}
