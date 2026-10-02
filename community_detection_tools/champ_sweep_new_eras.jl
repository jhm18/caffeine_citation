#	champ_sweep_new_eras.jl -- CHAMP Sweeps for 7-Year Era Networks
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	2 October 2026
#
#	WHAT THIS DOES. Profiles and sweeps the 7-year era networks produced by
#	PathA_Revised_Eras.R. Two stages:
#
#		1. PROFILING. Reports vertex count, arc count, and component structure for
#		   every era, so the analyst can see which eras carry enough structure for
#		   community detection before the sweep runs.
#
#		2. CHAMP SWEEP. Runs champ_era_sweep on each era whose largest component
#		   meets a minimum size threshold, and writes .clu partitions to the output
#		   directory.
#
#	Settings: directed, unweighted, weak components of fewer than four vertices
#	dropped before the sweep, 5 Leiden runs per gamma.
#
#	Run inside the container with threads:
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl
#
#	Profile only (no sweep):
#		julia community_detection_tools/champ_sweep_new_eras.jl --profile-only
#
#	Specific eras:
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl 15 16 17
#
#	Outputs, in pajek_files/new_champ_sweep/:
#		era<NN>_sweep.tsv           one row per gamma
#		era<NN>_local_gamma.clu     the partition at era N's own gamma
#		champ_sweep_summary.tsv     one row per era of diagnostics
#		era_profiles.tsv            vertex/arc/component counts for all eras
#		sweep.lock                  present only while a run is writing

#	Activating the Environment
	using Pkg
	Pkg.activate("/workspace/caffeine_citation/community_detection_tools")

#	Packages
	using DataFrames
	using community_detection_tools

##############
#   CONFIG   #
##############

#	Project Layout
	const PROJECT_DIR = get(ENV, "CAFFEINE_DIR", "/workspace/caffeine_citation")
	const PAJEK_DIR   = joinpath(PROJECT_DIR, "pajek_files", "eras_7yr")
	const OUT_DIR     = joinpath(PROJECT_DIR, "pajek_files", "new_champ_sweep")
	const LOCK_FILE   = joinpath(OUT_DIR, "sweep.lock")

#	Era Range (18 eras from 7-year tiling of 1901-2021)
	const N_ERAS = 18

#	Sweep Settings
	const MIN_COMPONENT_SIZE = 4
	const MIN_VIABLE_ARCS    = 50
	const RESOLUTION_RANGE   = (0.01, 1.0)
	const N_RESOLUTIONS      = 40
	const N_RUNS_PER_GAMMA   = 5

################
#   PROFILING  #
################

#	Helper Function for profile_eras: Count Arcs in a .net File
	function _count_arcs(lines::Vector{String})
		"""
		Args:
			lines::Vector{String}: raw lines from a .net file
		Returns:
			Int: number of arc lines after the *Arcs header
		Notes:
			Counts non-empty lines between *Arcs and the next section header or EOF.
		"""

		#	Locate the *Arcs Section
			arc_start = 0
			for i in eachindex(lines)
				if occursin(r"^\*[Aa]rcs", lines[i])
					arc_start = i
					break
				end
			end
			if arc_start == 0
				return 0
			end

		#	Count Arc Lines
			n_arcs = 0
			for i in (arc_start + 1):length(lines)
				ln = strip(lines[i])
				if startswith(ln, '*')
					break
				end
				if !isempty(ln)
					n_arcs += 1
				end
			end

		#	Assembling Result
			return n_arcs
	end

#	Profile All Era Networks
	function profile_eras(pajek_dir::AbstractString, n_eras::Int)
		"""
		Args:
			pajek_dir::AbstractString: directory containing era<NN>.net files
			n_eras::Int: number of eras to profile
		Returns:
			DataFrame: one row per era with columns era, net_file, n_vertices,
			           n_arcs, file_exists
		Notes:
			Reads each .net file header for vertex count and counts arc lines.
			Eras without a .net file are reported with zeros.
		"""

		#	Iterating Over Eras
			rows = NamedTuple[]
			for e in 1:n_eras
				net_file = joinpath(pajek_dir, @sprintf("era%02d.net", e))

				if !isfile(net_file)
					push!(rows, (era = e, net_file = net_file,
						n_vertices = 0, n_arcs = 0, file_exists = false))
					continue
				end

				#	Read File
					lines = readlines(net_file)

				#	Parse Vertex Count from Header
					n_vertices = 0
					for ln in lines
						m = match(r"^\*[Vv]ertices\s+(\d+)", ln)
						if m !== nothing
							n_vertices = parse(Int, m.captures[1])
							break
						end
					end

				#	Count Arcs
					n_arcs = _count_arcs(lines)

				push!(rows, (era = e, net_file = net_file,
					n_vertices = n_vertices, n_arcs = n_arcs, file_exists = true))
			end

		#	Assembling Result
			return DataFrame(rows)
	end

#	Helper Function for profile_eras: Write Profile Table
	function write_profiles(profiles::DataFrame, directory::AbstractString)
		"""
		Args:
			profiles::DataFrame: output of profile_eras
			directory::AbstractString: destination directory
		Returns:
			String: path written
		Notes:
			Tab-separated, one row per era.
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
			Prevents concurrent sweeps from overwriting each other's output.
			A stale lock from a dead process is replaced with a warning.
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

#	Helper Function for run_new_era_sweeps: Resolve Era Network Path
	function resolve_new_era_net(pajek_dir::AbstractString, era::Int)
		"""
		Args:
			pajek_dir::AbstractString: directory containing era<NN>.net files
			era::Int: era number (1-18)
		Returns:
			String: path to the era's .net file, or "" when absent
		Notes:
			New eras use a flat naming convention: era<NN>.net in a single directory.
		"""

		#	Build Path
			path = joinpath(pajek_dir, @sprintf("era%02d.net", era))

		#	Assembling Result
			return isfile(path) ? path : ""
	end

########################
#   SWEEP EXECUTION    #
########################

#	Sweep Viable Eras
	function run_new_era_sweeps(eras::Vector{Int}, profiles::DataFrame,
								summary_name::AbstractString)
		"""
		Args:
			eras::Vector{Int}: eras to sweep
			profiles::DataFrame: output of profile_eras, used to skip non-viable eras
			summary_name::AbstractString: file stem for the summary table
		Returns:
			Vector{Int}: eras that failed or were skipped
		Notes:
			Eras with fewer than MIN_VIABLE_ARCS arcs are skipped as non-viable.
			The summary is rewritten after each completed era. The lock is released
			however the loop ends.
		"""

		#	Output Directory and Lock
			mkpath(OUT_DIR)
			acquire_lock(LOCK_FILE)

		#	Run Stamp
			run_stamp = (run_started   = Libc.strftime("%Y-%m-%dT%H:%M:%S", time()),
				run_pid       = getpid(),
				julia_threads = Threads.nthreads())

		#	Iterating Over Eras
			rows = NamedTuple[]
			skipped = Int[]
			try
				for era in eras
					#	Check Viability
						era_row = filter(r -> r.era == era, profiles)
						if nrow(era_row) == 0 || !era_row.file_exists[1]
							@warn "Era $era: no network file; skipping"
							push!(skipped, era)
							continue
						end
						if era_row.n_arcs[1] < MIN_VIABLE_ARCS
							@warn "Era $era: only $(era_row.n_arcs[1]) arcs (< $MIN_VIABLE_ARCS); skipping"
							push!(skipped, era)
							continue
						end

					#	Locate the Network
						net_file = resolve_new_era_net(PAJEK_DIR, era)
						if isempty(net_file)
							@warn "Era $era: network file not found; skipping"
							push!(skipped, era)
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
							push!(rows, merge((era = era, seconds = round(seconds; digits = 1)),
								run_stamp, res.diagnostics))
							write_champ_summary(rows, summary_name; directory = OUT_DIR)
							d = res.diagnostics
							@info "Era $era done" seconds d.n_kept_vertices d.gamma_star d.best_index d.n_dominant d.gain_over_components d.modularity_range
						catch err
							@error "Era $era failed" exception = (err, catch_backtrace())
							push!(skipped, era)
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
			return skipped
	end

############
#   MAIN   #
############

#	Printf for Era File Names
	using Printf

#	Refuse an Interactive Run
	if isinteractive()
		error("champ_sweep_new_eras.jl writes shared output files and must be run " *
			"from a terminal: julia --threads auto community_detection_tools/champ_sweep_new_eras.jl")
	end

#	Check for Profile-Only Mode
	profile_only = "--profile-only" in ARGS
	era_args = filter(a -> a != "--profile-only", ARGS)

#	Era Selection from the Command Line
	eras = isempty(era_args) ? collect(1:N_ERAS) : parse.(Int, era_args)
	summary_name = isempty(era_args) ? "champ_sweep_summary" :
		"champ_sweep_summary_eras_" * join(eras, "_")

#	Step 1: Profile All Eras
	@info "Profiling era networks" PAJEK_DIR N_ERAS
	profiles = profile_eras(PAJEK_DIR, N_ERAS)
	mkpath(OUT_DIR)
	profile_path = write_profiles(profiles, OUT_DIR)
	@info "Era profiles written" path = profile_path

#	Display Profile
	for row in eachrow(profiles)
		status = if !row.file_exists
			"MISSING"
		elseif row.n_arcs < MIN_VIABLE_ARCS
			"TOO SMALL"
		else
			"viable"
		end
		@info @sprintf("  Era %2d: %6d vertices, %7d arcs  [%s]",
			row.era, row.n_vertices, row.n_arcs, status)
	end

	viable = filter(r -> r.file_exists && r.n_arcs >= MIN_VIABLE_ARCS, profiles)
	@info "Viable eras: $(nrow(viable)) of $(N_ERAS)"

#	Step 2: CHAMP Sweep (unless profile-only)
	if profile_only
		@info "Profile-only mode; stopping before sweep"
	else
		#	Warn on a Single Thread
			if Threads.nthreads() == 1
				@warn "Running on one thread; start Julia with --threads auto"
			end

		#	Restrict to Viable Eras in the Requested Set
			sweep_eras = intersect(eras, viable.era)
			if isempty(sweep_eras)
				@error "No viable eras to sweep in the requested set" eras
			else
				@info "CHAMP sweeps" sweep_eras Threads.nthreads() RESOLUTION_RANGE N_RESOLUTIONS OUT_DIR
				skipped = run_new_era_sweeps(sweep_eras, profiles, summary_name)
				if !isempty(skipped)
					@warn "Eras that did not complete" skipped
				end
			end
	end
