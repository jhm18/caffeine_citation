#	champ_sweep_eras.jl -- Per-Era CHAMP Sweeps for the Path A Integration Window
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	22 September 2026
#
#	WHAT THIS DOES. Runs champ_era_sweep on each era network, 12 through 23, and
#	writes the evidence the scale check and the pooled gamma will be decided from.
#	It selects nothing itself: each era's own gamma is recorded, not applied.
#
#	Settings: directed, unweighted, weak components of fewer than four vertices
#	dropped before the sweep, 5 Leiden runs per gamma.
#
#	THE WINDOW. The first run used gamma 0.1 to 1.8. These era networks are sparse
#	rather than highly clustered, and seven of twelve eras selected the second grid
#	point, so the elbow lay at or below the floor. The window now runs from 0.01 to
#	1.0 with 40 points, a step near 0.025 where the envelope bends. Change
#	RESOLUTION_RANGE and N_RESOLUTIONS below; each window writes to its own
#	subdirectory, so runs on different windows never mix.
#
#	ONE RUN AT A TIME. The first summary was overwritten by a second, concurrent
#	run started from the VS Code REPL. Three guards now prevent that:
#		1. The script refuses to run interactively; run it from a terminal.
#		2. A lock file in the output directory names the running process; a second
#		   run on the same window stops with an error while the first is alive.
#		3. The summary is rewritten after every era, and every row carries the run
#		   stamp, process id, and thread count of the run that wrote it.
#
#	Run inside the container. The sweep is threaded across gamma values, so give
#	Julia threads:
#		julia --threads auto scripts/julia/champ_sweep_eras.jl
#
#	One era or several, for a first look:
#		julia --threads auto scripts/julia/champ_sweep_eras.jl 12
#		julia --threads auto scripts/julia/champ_sweep_eras.jl 12 19 23
#
#	Outputs, in pajek_files/champ_sweep/gamma_<min>_<max>_n<points>/:
#		era<N>_sweep.tsv            one row per gamma: A, P, modularity, count, dominant
#		era<N>_local_gamma.clu      the partition at era N's own gamma, every vertex
#		champ_sweep_summary.tsv     one row per era of diagnostics (full window run)
#		champ_sweep_summary_eras_<list>.tsv   the same, when eras are named
#		sweep.lock                  present only while a run is writing

#   Activating the Environment
    using Pkg
    Pkg.activate("/workspace/caffeine_citation/community_detection_tools")
    Pkg.status()

#	Packages
	using DataFrames
	using community_detection_tools

##############
#   CONFIG   #
##############

#	Project Layout
	const PROJECT_DIR = get(ENV, "CAFFEINE_DIR", "/workspace/caffeine_citation")
	const PAJEK_DIR   = joinpath(PROJECT_DIR, "pajek_files")

#	Integration Window
	const DEFAULT_ERAS = 12:23

#	Sweep Settings
	const MIN_COMPONENT_SIZE = 4
	const RESOLUTION_RANGE   = (0.01, 1.0)
	const N_RESOLUTIONS      = 40
	const N_RUNS_PER_GAMMA   = 5

#	Output Directory, One per Window
	const GRID_TAG  = "gamma_$(RESOLUTION_RANGE[1])_$(RESOLUTION_RANGE[2])_n$(N_RESOLUTIONS)"
	const OUT_DIR   = joinpath(PAJEK_DIR, "champ_sweep", GRID_TAG)
	const LOCK_FILE = joinpath(OUT_DIR, "sweep.lock")

###############
#   HELPERS   #
###############

#	Helper Function for run_era_sweeps: Era Network Resolution
	function resolve_era_net(pajek_dir::AbstractString, era::Int)
		"""
		Args:
			pajek_dir::AbstractString: root of the pajek_files tree
			era::Int: era number
		Returns:
			String: path to the era's .net file, or "" when absent
		Notes:
			Eras 18 to 21 keep era<N>.net at the top of pajek_files; the others keep
			it inside Era<N>/. The directory is tried first, so a run is
			reproducible if both ever exist.
		"""

		#	Candidate Networks, in Priority Order
			candidates = [joinpath(pajek_dir, "Era$(era)", "era$(era).net"),
			              joinpath(pajek_dir, "era$(era).net")]

		#	Select the First That Exists
			hit = findfirst(isfile, candidates)

		#	Assembling Result
			return hit === nothing ? "" : candidates[hit]
	end

#	Helper Function for run_era_sweeps: Acquire the Output Lock
	function acquire_lock(lock_file::AbstractString)
		"""
		Args:
			lock_file::AbstractString: lock path inside the output directory
		Returns:
			Nothing
		Notes:
			The lock holds the process id of the run writing the directory. A lock
			whose process is still alive stops this run; a lock left by a process
			that has exited, after a crash or a kill, is reported and replaced.
			Liveness is read from /proc, which the Linux container provides.
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

#	Sweep the Requested Eras
	function run_era_sweeps(eras::Vector{Int}, summary_name::AbstractString)
		"""
		Args:
			eras::Vector{Int}: eras to sweep
			summary_name::AbstractString: file stem for the summary table
		Returns:
			Vector{Int}: eras that failed
		Notes:
			An era that cannot be resolved or that throws is logged and skipped, so
			one bad era does not cost the rest. The summary is rewritten after each
			completed era, so the file on disk always describes this run and only
			this run. The lock is released however the loop ends.
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
						net_file = resolve_era_net(PAJEK_DIR, era)
						if isempty(net_file)
							@warn "Era $era: no network found; skipping"
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
							write_champ_sweep(res, "era$(era)"; directory = OUT_DIR)
							push!(rows, merge((era = era, seconds = round(seconds; digits = 1)),
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
		error("champ_sweep_eras.jl writes shared output files and must be run from a " *
		      "terminal: julia --threads auto champ_sweep_eras.jl")
	end

#	Warn on a Single Thread
	if Threads.nthreads() == 1
		@warn "Running on one thread; start Julia with --threads auto"
	end

#	Era Selection from the Command Line
	eras = isempty(ARGS) ? collect(DEFAULT_ERAS) : parse.(Int, ARGS)
	summary_name = isempty(ARGS) ? "champ_sweep_summary" :
	               "champ_sweep_summary_eras_" * join(eras, "_")

#	Run
	@info "CHAMP sweeps" eras Threads.nthreads() RESOLUTION_RANGE N_RESOLUTIONS OUT_DIR
	failed = run_era_sweeps(eras, summary_name)
	if !isempty(failed)
		@warn "Eras that did not complete" failed
	end
