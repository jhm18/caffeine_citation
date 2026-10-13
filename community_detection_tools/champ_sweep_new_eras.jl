#	champ_sweep_new_eras.jl -- CHAMP Sweeps for the Rebuilt 7-Year Era Networks
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	13 October 2026
#
#	WHAT THIS DOES. Profiles and sweeps the 7-year era networks written by
#	PathA_Revised_Eras.R. Two stages:
#
#		1. PROFILING. Calls profile_era_network on every era file and reports
#		   components, giant-component share, the citing and cited counts, and the
#		   two depth measures. Costs seconds, and is what the viability screen is
#		   decided from.
#
#		2. CHAMP SWEEP. Runs champ_era_sweep on each viable era and writes its hull
#		   coefficients, its diagnostics, and its partition at that era's own gamma.
#
#	WHAT CHANGED FROM THE PREVIOUS VERSION.
#
#		a.	THE VIABILITY SCREEN MEASURES CONNECTIVITY, NOT VOLUME. The old rule
#			skipped an era with fewer than 50 arcs. That admits an era like the
#			2020-2021 tail -- 3,001 vertices and 2,598 arcs in 260 fragments, a 13%
#			giant component -- which clears any arc threshold while offering a
#			resolution parameter nothing to choose among, since gamma separates
#			partitions only WITHIN a component. The screen is now a floor on the
#			giant component, in vertices and in share.
#
#		b.	PROFILING USES THE MODULE. The old profiler counted arc lines in the
#			file and read the vertex count from the header, which cannot see
#			components at all. profile_era_network reads the network properly and
#			returns the structure.
#
#		c.	THE DEFAULT RANGE IS ERAS 11 TO 17. Mean degree reaches its plateau at
#			era 11 (2.96) and stays near 3.0 through era 17; the share of citing
#			articles that are themselves cited within the era rises from 24% at era
#			10 to 35% at era 11 and 46% by era 17. Eras 1 to 10 are too small to
#			carry structure and era 18 spans only 2020-2021. All eras are still
#			profiled, so the boundary stays empirical.
#
#		d.	THE OUTPUT DIRECTORY IS TAGGED BY THE GRID. Two sweeps on different
#			windows cannot write over each other, which they could when the
#			directory was fixed.
#
#	Run inside the container with threads:
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl
#
#	Profile only, no sweep:
#		julia community_detection_tools/champ_sweep_new_eras.jl --profile-only
#
#	Specific eras, screen or no screen:
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl 11 17
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl --force 10 18
#
#	Outputs, in pajek_files/champ_sweep_7yr/gamma_<min>_<max>_n<points>/:
#		era<NN>_sweep.tsv           one row per gamma
#		era<NN>_local_gamma.clu     the partition at era N's own gamma
#		champ_sweep_summary.tsv     one row per era of diagnostics
#		era_profiles.tsv            structure of every era, swept or not
#		sweep.lock                  present only while a run is writing

#	Activating the Environment
	using Pkg
	Pkg.activate("/workspace/caffeine_citation/community_detection_tools")

################
#   PACKAGES   #
################

using DataFrames
using Printf
using community_detection_tools

##############
#   CONFIG   #
##############

#	Project Layout
	const PROJECT_DIR = get(ENV, "CAFFEINE_DIR", "/workspace/caffeine_citation")
	const PAJEK_DIR   = joinpath(PROJECT_DIR, "pajek_files", "eras_7yr")

#	Era Range
	const N_ERAS        = 18
	const ANALYTIC_ERAS = 11:17

#	Sweep Settings
	const MIN_COMPONENT_SIZE = 4
	const RESOLUTION_RANGE   = (0.01, 1.0)
	const N_RESOLUTIONS      = 40
	const N_RUNS_PER_GAMMA   = 5

#	Viability Screen
#	A resolution parameter can only choose among partitions within a connected
#	component, so both the size of the largest component and its share of the
#	network have to clear a floor. Era 18 fails on both counts while holding more
#	arcs than era 10, which is the case the old arc threshold could not see.
	const MIN_GIANT_VERTICES = 500
	const MIN_GIANT_SHARE    = 0.50

#	Output Directory, One per Window
	const GRID_TAG  = "gamma_$(RESOLUTION_RANGE[1])_$(RESOLUTION_RANGE[2])_n$(N_RESOLUTIONS)"
	const OUT_DIR   = joinpath(PROJECT_DIR, "pajek_files", "champ_sweep_7yr", GRID_TAG)
	const LOCK_FILE = joinpath(OUT_DIR, "sweep.lock")

#################
#   PROFILING   #
#################

#	Helper Function for profile_eras: Resolve Era Network Path
	function resolve_new_era_net(pajek_dir::AbstractString, era::Int)
		"""
		Args:
			pajek_dir::AbstractString: directory containing era<NN>.net files
			era::Int: era number
		Returns:
			String: path to the era's .net file, or "" when absent
		Notes:
			The rebuilt eras use a flat naming convention: era<NN>.net in a single
			directory, two digits so a lexical listing sorts correctly.
		"""

		#	Build Path
			path = joinpath(pajek_dir, @sprintf("era%02d.net", era))

		#	Assembling Result
			return isfile(path) ? path : ""
	end

#	Profile All Era Networks
	function profile_eras(pajek_dir::AbstractString, n_eras::Int;
						  min_component_size::Int = MIN_COMPONENT_SIZE)
		"""
		Args:
			pajek_dir::AbstractString: directory containing era<NN>.net files
			n_eras::Int: highest era number to look for
			min_component_size::Int: component floor a sweep would apply
		Returns:
			DataFrame: one row per era, whether or not a file exists
		Notes:
			profile_era_network does the reading, so this function only has to walk
			the eras and record a row of zeros where a file is absent. An era that
			throws while being read is recorded as absent rather than halting the
			profile, since a run should be able to report on seventeen good eras
			when one file is malformed.

			Rows are stacked one at a time rather than passed to DataFrame as a
			vector, because the vector's element type is abstract and column
			inference from an abstract row vector is not reliable.
		"""

		#	Iterating Over Eras
			rows = NamedTuple[]
			for e in 1:n_eras
				net_file = resolve_new_era_net(pajek_dir, e)

				#	No file
					if isempty(net_file)
						push!(rows, (era = e, file_exists = false,
									 net_file = joinpath(pajek_dir, @sprintf("era%02d.net", e)),
									 n_vertices = 0, n_arcs = 0, n_duplicate_arcs = 0,
									 n_self_loops = 0, n_components = 0,
									 giant_component_size = 0, giant_component_share = 0.0,
									 n_kept_vertices = 0, n_kept_arcs = 0,
									 n_kept_components = 0, n_citing = 0, n_cited = 0,
									 n_both_degree = 0, n_arcs_two_step = 0,
									 mean_degree = 0.0))
						continue
					end

				#	Profile
					try
						p = profile_era_network(net_file;
												min_component_size = min_component_size)
						push!(rows, merge((era = e, file_exists = true), p))
					catch err
						@error "Era $e: could not be profiled" net_file exception = err
						push!(rows, (era = e, file_exists = false, net_file = net_file,
									 n_vertices = 0, n_arcs = 0, n_duplicate_arcs = 0,
									 n_self_loops = 0, n_components = 0,
									 giant_component_size = 0, giant_component_share = 0.0,
									 n_kept_vertices = 0, n_kept_arcs = 0,
									 n_kept_components = 0, n_citing = 0, n_cited = 0,
									 n_both_degree = 0, n_arcs_two_step = 0,
									 mean_degree = 0.0))
					end
			end

		#	Assembling Result
			return vcat([DataFrame([r]) for r in rows]...)
	end

#	Helper Function for profile_eras: Viability of One Profile Row
	function is_viable(row)
		"""
		Args:
			row: one row of the profile table
		Returns:
			Bool: whether the era has enough connected structure to sweep
		Notes:
			Both tests matter. The share alone would admit a tiny era whose handful
			of vertices happen to be connected; the size alone would admit a large
			era that is mostly fragments.
		"""

		#	Assembling Result
			return row.file_exists &&
				   row.n_arcs > 0 &&
				   row.giant_component_size >= MIN_GIANT_VERTICES &&
				   row.giant_component_share >= MIN_GIANT_SHARE
	end

#	Write the Profile Table
	function write_profiles(profiles::DataFrame, directory::AbstractString)
		"""
		Args:
			profiles::DataFrame: output of profile_eras
			directory::AbstractString: destination directory
		Returns:
			String: path written
		Notes:
			Written whether or not a sweep follows, so a profile-only run leaves the
			same record a full run would.
		"""

		#	Write
			mkpath(directory)
			path = joinpath(directory, "era_profiles.tsv")
			open(path, "w") do io
				println(io, join(names(profiles), '\t'))
				for row in eachrow(profiles)
					println(io, join((string(x) for x in row), '\t'))
				end
			end

		#	Assembling Result
			return path
	end

###############
#   HELPERS   #
###############

#	Helper Function for run_new_era_sweeps: Acquire the Output Lock
	function acquire_lock(lock_file::AbstractString)
		"""
		Args:
			lock_file::AbstractString: lock path inside the output directory
		Returns:
			Nothing
		Notes:
			Prevents concurrent sweeps from overwriting each other's output, which
			happened once: a second run started from the VS Code REPL finished later
			and wrote its own summary over the good one. A stale lock from a dead
			process is replaced with a warning.
		"""

		#	Check for a Live Lock
			if isfile(lock_file)
				pid = tryparse(Int, strip(read(lock_file, String)))
				if pid !== nothing && pid != getpid() && isdir("/proc/$(pid)")
					throw(ErrorException("Another sweep (process $pid) is writing to " *
						"$(dirname(lock_file)). Stop it, or wait for it to " *
						"finish, before starting this one."))
				end
				@warn "Replacing a stale lock left by an earlier run" lock_file pid
			end

		#	Write This Run's Lock
			write(lock_file, string(getpid()))

		#	Assembling Result
			return nothing
	end

########################
#   SWEEP EXECUTION    #
########################

#	Sweep the Requested Eras
	function run_new_era_sweeps(eras::Vector{Int}, profiles::DataFrame,
								summary_name::AbstractString)
		"""
		Args:
			eras::Vector{Int}: eras to sweep, already screened by the caller
			profiles::DataFrame: output of profile_eras, for the file paths
			summary_name::AbstractString: file stem for the summary table
		Returns:
			Vector{Int}: eras that failed
		Notes:
			The summary is rewritten after every completed era, and every row carries
			the run stamp, so a file on disk can always be traced to the run that
			wrote it. The lock is released however the loop ends.
		"""

		#	Output Directory and Lock
			mkpath(OUT_DIR)
			acquire_lock(LOCK_FILE)

		#	Run Stamp, Carried on Every Summary Row
			run_stamp = (run_started   = Libc.strftime("%Y-%m-%dT%H:%M:%S", time()),
						 run_pid       = getpid(),
						 julia_threads = Threads.nthreads())

		#	Iterating Over Eras
			rows = NamedTuple[]
			failed = Int[]
			try
				for era in eras
					#	Locate the Network
					#	Taken from the profile table, so the file swept is the file
					#	profiled; falls back to the directory when an era was requested
					#	that the profile pass never saw.
						hit = findfirst(==(era), profiles.era)
						net_file = (hit !== nothing && profiles.file_exists[hit]) ?
								   String(profiles.net_file[hit]) :
								   resolve_new_era_net(PAJEK_DIR, era)
						if isempty(net_file) || !isfile(net_file)
							@warn "Era $era: network file not found; skipping"
							push!(failed, era)
							continue
						end

					#	Sweep, Write, and Refresh the Summary
						try
							seconds = @elapsed res = champ_era_sweep(net_file;
								min_component_size = MIN_COMPONENT_SIZE,
								resolution_range   = RESOLUTION_RANGE,
								n_resolutions      = N_RESOLUTIONS,
								n_runs_per_gamma   = N_RUNS_PER_GAMMA)
							write_champ_sweep(res, @sprintf("era%02d", era);
											  directory = OUT_DIR)
							push!(rows, merge((era = era,
											   seconds = round(seconds; digits = 1)),
											  run_stamp, res.diagnostics))
							write_champ_summary(rows, summary_name; directory = OUT_DIR)
							d = res.diagnostics
							@info "Era $era done" seconds d.n_kept_vertices d.gamma_star d.best_index d.n_dominant d.gain_over_components d.modularity_range
						catch err
							@error "Era $era failed" exception = (err, catch_backtrace())
							push!(failed, era)
						end
				end
			finally
				rm(LOCK_FILE; force = true)
			end

		#	Report
			if isempty(rows)
				@error "No era completed; no summary written"
			else
				@info "Summary complete" path = joinpath(OUT_DIR, "$(summary_name).tsv") eras_written = [r.era for r in rows]
			end

		#	Assembling Result
			return failed
	end

############
#   MAIN   #
############

#	Refuse an Interactive Run
	if isinteractive()
		error("champ_sweep_new_eras.jl writes shared output files and must be run " *
			"from a terminal: julia --threads auto community_detection_tools/champ_sweep_new_eras.jl")
	end

#	Read the Flags
	profile_only = "--profile-only" in ARGS
	force_sweep  = "--force" in ARGS
	era_args = filter(a -> !startswith(a, "--"), ARGS)

#	Era Selection from the Command Line
	eras = isempty(era_args) ? collect(ANALYTIC_ERAS) : parse.(Int, era_args)
	summary_name = isempty(era_args) ? "champ_sweep_summary" :
		"champ_sweep_summary_eras_" * join(eras, "_")

#	Step 1: Profile Every Era
	@info "Profiling era networks" PAJEK_DIR N_ERAS
	profiles = profile_eras(PAJEK_DIR, N_ERAS)
	mkpath(OUT_DIR)
	@info "Era profiles written" path = write_profiles(profiles, OUT_DIR)

#	Display the Profile
	for row in eachrow(profiles)
		status = if !row.file_exists
			"missing"
		elseif is_viable(row)
			"viable"
		else
			"below the screen"
		end
		@info @sprintf("  Era %2d: %6d vertices, %7d arcs, %5d components, giant %5.1f%%, %5d both-degree  [%s]",
			row.era, row.n_vertices, row.n_arcs, row.n_components,
			100 * row.giant_component_share, row.n_both_degree, status)
	end

	viable = filter(is_viable, profiles)
	@info "Viable eras" n = nrow(viable) eras = viable.era MIN_GIANT_VERTICES MIN_GIANT_SHARE

#	Step 2: CHAMP Sweep, Unless Profiling Only
	if profile_only
		@info "Profile-only mode; stopping before the sweep"
	else
		#	Warn on a Single Thread
			if Threads.nthreads() == 1
				@warn "Running on one thread; start Julia with --threads auto"
			end

		#	Apply the Screen, Unless Overridden
			sweep_eras = force_sweep ? eras : intersect(eras, viable.era)
			if force_sweep
				@warn "Screen overridden with --force; sweeping every requested era" sweep_eras
			end

		#	Run
			if isempty(sweep_eras)
				@error "No viable era in the requested set" eras
			else
				@info "CHAMP sweeps" sweep_eras Threads.nthreads() RESOLUTION_RANGE N_RESOLUTIONS OUT_DIR
				failed = run_new_era_sweeps(sweep_eras, profiles, summary_name)
				if !isempty(failed)
					@warn "Eras that did not complete" failed
				end
			end
	end
