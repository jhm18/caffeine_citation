#	champ_sweep_new_eras.jl -- Sweeps, Probe and Calibration for the 7-Year Era Networks
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	20 October 2026
#
#	WHAT THIS DOES. Profiles, sweeps, probes and calibrates the 7-year era networks
#	written by PathA_Revised_Eras.R, following granularity_design.md (fourth
#	draft). Four stages, run separately:
#
#		1. PROFILING. Calls profile_era_network on every era file and reports
#		   components, giant-component share, the citing and cited counts, and the
#		   depth and composition measures. Costs seconds, is what the viability
#		   screen is decided from, and runs at the start of every mode.
#
#		2. CHAMP SWEEP (default). Runs champ_era_sweep on each viable era and
#		   writes its per-gamma table -- hull coefficients, the admissible set and
#		   its intervals, and the citer-weighted granularity at every gamma, which
#		   is the calibration curve -- plus its diagnostics and the elbow partition.
#
#		3. PROBE (--probe). Runs granularity_probe on the requested eras: single
#		   Leiden starts across seeds at a few gammas, reporting monotonicity of
#		   k_eff, its run-to-run variation, and membership agreement (AMI) across
#		   seeds. The design (sec. 9.1) requires this before any partition is
#		   written, and makes agreement across seeds a decision point.
#
#		4. CALIBRATION (--calibrate). Reads the sweeps on disk, derives one gamma
#		   per era from a declared scaling rule and anchor, writes the partition at
#		   that gamma tagged by specification, and records target and achieved
#		   granularity side by side. Nothing here uses the admissible set, the
#		   elbow or gamma_star.
#
#	WHAT CHANGED FROM THE PREVIOUS VERSION.
#
#		a.	THE SWEEP CARRIES GRANULARITY. era<NN>_sweep.tsv now holds citer-weighted
#			k_eff and g at every gamma, the m/P proxy beside them, the exact
#			admissible set and its intervals, and the largest gamma * K_out_c / m.
#			Sweeps written before this change have no calibration curve, and
#			--calibrate refuses them.
#
#		b.	THE SELECTED PARTITION IS NO LONGER THE ELBOW. era<NN>_local_gamma.clu
#			is still written as an envelope diagnostic, but the partitions the
#			analysis uses are era<NN>_<spec>.clu under specifications/<spec>/,
#			each with a _meta.tsv sidecar recording its gamma, seed, target and
#			achieved k_eff, and the detected and restored community counts.
#
#		c.	THE PROBE AND THE CALIBRATION ARE NEW MODES, below.
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
#	Validation probe (default eras 11, 13 and 17; the design's minimum):
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl --probe
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl --probe 11 17 \
#		      --probe-gammas 0.5,1,2,3,4 --seeds 0,1,2,3,4,5,6,7,8,9
#
#	Calibration (needs sweeps on disk; writes one partition per era):
#		julia --threads auto community_detection_tools/champ_sweep_new_eras.jl --calibrate \
#		      --alpha 1.0 --anchor-g 120 --anchor-era 14 --tolerance 0.10
#
#	Outputs, in pajek_files/champ_sweep_7yr/gamma_<min>_<max>_n<points>/:
#		era<NN>_sweep.tsv                one row per gamma, with the calibration curve
#		era<NN>_local_gamma.clu          the elbow partition, an envelope diagnostic
#		champ_sweep_summary.tsv          one row per era of diagnostics
#		era_profiles.tsv                 structure of every era, swept or not
#		era<NN>_probe_runs.tsv           probe: one row per gamma x seed
#		era<NN>_probe_agreement.tsv      probe: one row per gamma x seed pair
#		era<NN>_probe_summary.tsv        probe: one row per gamma
#		specifications/<spec>/
#		    era<NN>_<spec>.clu           the partition at the calibrated gamma
#		    era<NN>_<spec>_meta.tsv      its gamma, seed, target and achieved k_eff
#		    calibration_<spec>.tsv       one row per era: target, achieved, flags
#		sweep.lock                       present only while a run is writing

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
	const PROBE_ERAS    = [11, 13, 17]

#	Sweep Settings
	const MIN_COMPONENT_SIZE = 4
	const RESOLUTION_RANGE   = (0.1, 6.0)
	const N_RESOLUTIONS      = 60
	const N_RUNS_PER_GAMMA   = 5

#	Probe Defaults
	const PROBE_GAMMAS = [0.5, 1.0, 2.0, 3.0, 4.0]
	const PROBE_SEEDS  = collect(0:9)

#	Calibration Defaults
#	The working anchor of the design (sec. 6.3): g = 120 citing articles per
#	effective community at era 14, with alpha = 1 the provisional primary
#	specification (sec. 7.3). The tolerance is a placeholder until the probe
#	justifies one (sec. 9.1, requirement 5).
	const DEFAULT_ALPHA      = 1.0
	const DEFAULT_ANCHOR_G   = 120.0
	const DEFAULT_ANCHOR_ERA = 14
	const DEFAULT_TOLERANCE  = 0.10
	const PARTITION_SEED     = 0

#	Viability Screen
#	A resolution parameter can only choose among partitions within a connected
#	component, so both the size of the largest component and its share of the
#	network have to clear a floor. Era 18 fails on both counts while holding more
#	arcs than era 10, which is the case an arc threshold could not see.
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

#	Helper Function for profile_eras: A Row of Zeros for an Absent Era
	function absent_profile_row(era::Int, net_file::AbstractString)
		"""
		Args:
			era::Int: era number
			net_file::AbstractString: the path that was looked for
		Returns:
			NamedTuple: the fields of profile_era_network, zeroed, with file_exists = false
		Notes:
			Kept in one place so the zero row and the real row cannot drift apart
			in their fields, which would break the stacking into one table.
		"""

		#	Assembling Result
			return (era = era, file_exists = false, net_file = String(net_file),
			        n_vertices = 0, n_arcs = 0, n_duplicate_arcs = 0, n_self_loops = 0,
			        n_components = 0, giant_component_size = 0, giant_component_share = 0.0,
			        n_kept_vertices = 0, n_kept_arcs = 0, n_kept_components = 0,
			        n_restored_components = 0, n_citing = 0, n_cited = 0, n_both_degree = 0,
			        n_cited_only_multi = 0, n_arcs_two_step = 0,
			        mean_in_degree_cited_only = 0.0, mean_degree = 0.0)
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
						push!(rows, absent_profile_row(e, joinpath(pajek_dir, @sprintf("era%02d.net", e))))
						continue
					end

				#	Profile
					try
						p = profile_era_network(net_file;
												min_component_size = min_component_size)
						push!(rows, merge((era = e, file_exists = true), p))
					catch err
						@error "Era $e: could not be profiled" net_file exception = err
						push!(rows, absent_profile_row(e, net_file))
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

#	Helper Function for the Writing Modes: Acquire the Output Lock
	function acquire_lock(lock_file::AbstractString)
		"""
		Args:
			lock_file::AbstractString: lock path inside the output directory
		Returns:
			Nothing
		Notes:
			Prevents concurrent runs from overwriting each other's output, which
			happened once: a second run started from the VS Code REPL finished later
			and wrote its own summary over the good one. A stale lock from a dead
			process is replaced with a warning.
		"""

		#	Check for a Live Lock
			if isfile(lock_file)
				pid = tryparse(Int, strip(read(lock_file, String)))
				if pid !== nothing && pid != getpid() && isdir("/proc/$(pid)")
					throw(ErrorException("Another run (process $pid) is writing to " *
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

#	Helper Function for Every Mode: Locate an Era's Network
	function locate_net(era::Int, profiles::DataFrame)
		"""
		Args:
			era::Int: era number
			profiles::DataFrame: output of profile_eras
		Returns:
			String: the path, or "" when there is none
		Notes:
			Taken from the profile table, so the file swept is the file profiled;
			falls back to the directory when an era was requested that the profile
			pass never saw.
		"""

		#	Look Up
			hit = findfirst(==(era), profiles.era)
			net_file = (hit !== nothing && profiles.file_exists[hit]) ?
					   String(profiles.net_file[hit]) :
					   resolve_new_era_net(PAJEK_DIR, era)

		#	Assembling Result
			return (isempty(net_file) || !isfile(net_file)) ? "" : net_file
	end

#	Helper Function for the Command Line: Value of a --flag
	function option_value(args::Vector{String}, flag::String, default)
		"""
		Args:
			args::Vector{String}: ARGS
			flag::String: the option, e.g. "--alpha"
			default: returned when the flag is absent; its type decides the parse
		Returns:
			the parsed value
		Notes:
			A missing value after the flag is an error, not a silent default.
		"""

		#	Find
			ix = findfirst(==(flag), args)
			ix === nothing && return default
			if ix == length(args) || startswith(args[ix + 1], "--")
				error("$flag needs a value")
			end
			raw = args[ix + 1]

		#	Parse by the Default's Type
			if default isa Float64
				return parse(Float64, raw)
			elseif default isa Int
				return parse(Int, raw)
			elseif default isa Vector{Float64}
				return parse.(Float64, split(raw, ','))
			elseif default isa Vector{Int}
				return parse.(Int, split(raw, ','))
			else
				return raw
			end
	end

#	Helper Function for the Command Line: Positional Arguments
	function positional_args(args::Vector{String})
		"""
		Args:
			args::Vector{String}: ARGS
		Returns:
			Vector{String}: the arguments that are neither flags nor flag values
		"""

		#	Walk, Skipping Each Valued Flag and Its Value
			valued = ("--alpha", "--anchor-g", "--anchor-era", "--tolerance",
			          "--seeds", "--probe-gammas", "--spec")
			out = String[]
			skip = false
			for a in args
				if skip
					skip = false
				elseif a in valued
					skip = true
				elseif !startswith(a, "--")
					push!(out, a)
				end
			end

		#	Assembling Result
			return out
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
						net_file = locate_net(era, profiles)
						if isempty(net_file)
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
							@info "Era $era done" seconds d.W d.n_restored_components d.k_eff_citer_min d.k_eff_citer_max d.k_eff_citer_monotone d.max_penalty_share d.n_dominant d.gamma_star
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

######################
#   PROBE EXECUTION  #
######################

#	Probe the Requested Eras
	function run_granularity_probes(eras::Vector{Int}, profiles::DataFrame,
	                                gammas::Vector{Float64}, seeds::Vector{Int})
		"""
		Args:
			eras::Vector{Int}: eras to probe
			profiles::DataFrame: output of profile_eras, for the file paths
			gammas::Vector{Float64}: resolutions to probe
			seeds::Vector{Int}: Leiden seeds per resolution
		Returns:
			Vector{Int}: eras that failed
		Notes:
			Writes the three probe tables per era and prints the per-gamma summary,
			since the summary is what the sec. 9.1 decision is read from. The
			probe runs one single-start Leiden per seed, so its cost is
			length(gammas) * length(seeds) runs per era, against the sweep's
			N_RESOLUTIONS * N_RUNS_PER_GAMMA.
		"""

		#	Output Directory and Lock
			mkpath(OUT_DIR)
			acquire_lock(LOCK_FILE)

		#	Iterating Over Eras
			failed = Int[]
			try
				for era in eras
					net_file = locate_net(era, profiles)
					if isempty(net_file)
						@warn "Era $era: network file not found; skipping"
						push!(failed, era)
						continue
					end
					try
						seconds = @elapsed probe = granularity_probe(net_file;
							gammas             = gammas,
							seeds              = seeds,
							min_component_size = MIN_COMPONENT_SIZE)
						paths = write_granularity_probe(probe, @sprintf("era%02d", era);
						                                directory = OUT_DIR)
						@info "Era $era probed" seconds probe.monotone paths.summary
						for row in eachrow(probe.summary)
							@info @sprintf("  gamma %5.2f: k_eff %7.2f (cv %5.3f, range %7.2f-%7.2f)  proxy/measured %5.3f  AMI mean %5.3f min %5.3f  max penalty %5.3f",
								row.gamma, row.k_eff_citer_mean, row.k_eff_citer_cv,
								row.k_eff_citer_min, row.k_eff_citer_max, row.proxy_ratio_mean,
								row.ami_mean, row.ami_min, row.max_penalty_share)
						end
					catch err
						@error "Era $era probe failed" exception = (err, catch_backtrace())
						push!(failed, era)
					end
				end
			finally
				rm(LOCK_FILE; force = true)
			end

		#	Assembling Result
			return failed
	end

###########################
#   CALIBRATION EXECUTION #
###########################

#	Calibrate and Write One Specification
	function run_calibration(eras::Vector{Int}, profiles::DataFrame;
	                         alpha::Float64, anchor_g::Float64, anchor_era::Int,
	                         tolerance::Float64, spec::AbstractString)
		"""
		Args:
			eras::Vector{Int}: eras to calibrate and write
			profiles::DataFrame: output of profile_eras, for W and the file paths
			alpha::Float64: the scaling exponent, k_eff proportional to W^alpha
			anchor_g::Float64: granularity at the anchor era, citing articles per
				effective community
			anchor_era::Int: the era the anchor is set in
			tolerance::Float64: relative deviation accepted as calibrated
			spec::AbstractString: specification tag, the output subdirectory
		Returns:
			Vector{Int}: eras that failed
		Notes:
			Implements sec. 6.1 of the design:

				target k_eff(e)  =  k_ref * (W_e / W_ref)^alpha
				gamma_e          =  the gamma at which era e attains that k_eff

			with k_ref = W_ref / anchor_g and W_e the citing-article count from the
			profile. The curve inverted is the k_eff_citer column of each era's
			sweep on disk, so --calibrate needs the sweeps to have been written by
			this version of the pipeline. Each era's partition is then written at
			its gamma, and the calibration table records, per era, the target, the
			sweep's achieved value, the written partition's achieved value, and
			the flags.

			The anchor era must be among the eras calibrated, or at least have a
			profile row, because W_ref comes from it.
		"""

		#	Validation
			if !(0.0 <= alpha <= 1.0)
				@warn "alpha outside [0, 1]; the design treats only that range as interpretable" alpha
			end
			anchor_hit = findfirst(==(anchor_era), profiles.era)
			if anchor_hit === nothing || !profiles.file_exists[anchor_hit] || profiles.n_citing[anchor_hit] == 0
				error("Anchor era $anchor_era has no profile with citing articles; cannot set W_ref")
			end
			W_ref = Float64(profiles.n_citing[anchor_hit])
			k_ref = W_ref / anchor_g

		#	Output Directory and Lock
			spec_dir = joinpath(OUT_DIR, "specifications", spec)
			mkpath(spec_dir)
			acquire_lock(LOCK_FILE)
			@info "Calibration" spec alpha anchor_g anchor_era W_ref k_ref tolerance spec_dir

		#	Iterating Over Eras
			rows = NamedTuple[]
			failed = Int[]
			try
				for era in eras
					net_file = locate_net(era, profiles)
					hit = findfirst(==(era), profiles.era)
					if isempty(net_file) || hit === nothing
						@warn "Era $era: network file not found; skipping"
						push!(failed, era)
						continue
					end
					try
						#	Target From the Scaling Rule
							W_e = Float64(profiles.n_citing[hit])
							target = k_ref * (W_e / W_ref)^alpha

						#	Invert the Era's Curve
							sweep = read_champ_sweep(joinpath(OUT_DIR, @sprintf("era%02d_sweep.tsv", era)))
							cal = calibrate_resolution(sweep.gamma, sweep.k_eff_citer, target;
							                           tolerance = tolerance)

						#	Write the Partition at That Gamma
							out = write_partition_at_resolution(net_file, cal.gamma;
								prefix             = @sprintf("era%02d", era),
								spec               = spec,
								directory          = spec_dir,
								target_k_eff       = target,
								seed               = PARTITION_SEED,
								min_component_size = MIN_COMPONENT_SIZE,
								n_runs_per_gamma   = N_RUNS_PER_GAMMA)

						#	Record
							push!(rows, (era                      = era,
							             spec                     = String(spec),
							             alpha                    = alpha,
							             anchor_g                 = anchor_g,
							             anchor_era               = anchor_era,
							             W                        = Int(W_e),
							             target_k_eff             = target,
							             target_g                 = W_e / target,
							             gamma                    = cal.gamma,
							             sweep_achieved_k_eff     = cal.achieved_k_eff,
							             sweep_relative_deviation = cal.relative_deviation,
							             written_achieved_k_eff   = out.meta.achieved_k_eff_citer,
							             written_relative_deviation = out.meta.relative_deviation,
							             written_g                = out.meta.g_citer,
							             tolerance                = tolerance,
							             met                      = cal.met,
							             at_bound                 = cal.at_bound,
							             n_within_tolerance       = cal.n_within_tolerance,
							             gamma_within_min         = cal.gamma_within_min,
							             gamma_within_max         = cal.gamma_within_max,
							             curve_monotone           = cal.curve_monotone,
							             n_detected               = out.meta.n_detected,
							             n_restored               = out.meta.n_restored,
							             n_ge3                    = out.meta.n_ge3,
							             n_ge10                   = out.meta.n_ge10,
							             largest_share_citer      = out.meta.largest_share_citer,
							             size_gini_citer          = out.meta.size_gini_citer,
							             max_penalty_share        = out.meta.max_penalty_share,
							             clu                      = out.clu))
							@info @sprintf("  Era %2d: target k_eff %7.2f  gamma %5.2f  sweep %7.2f (%+5.1f%%)  written %7.2f (%+5.1f%%)  met %s  at_bound %s  within [%4.2f, %4.2f]",
								era, target, cal.gamma, cal.achieved_k_eff, 100 * cal.relative_deviation,
								out.meta.achieved_k_eff_citer, 100 * out.meta.relative_deviation,
								cal.met, cal.at_bound, cal.gamma_within_min, cal.gamma_within_max)
					catch err
						@error "Era $era calibration failed" exception = (err, catch_backtrace())
						push!(failed, era)
					end
				end
			finally
				rm(LOCK_FILE; force = true)
			end

		#	Write the Calibration Table
			if isempty(rows)
				@error "No era calibrated; no table written"
			else
				table = vcat([DataFrame([r]) for r in rows]...)
				path = joinpath(spec_dir, "calibration_$(spec).tsv")
				open(path, "w") do io
					println(io, join(names(table), '\t'))
					for row in eachrow(table)
						println(io, join((string(x) for x in row), '\t'))
					end
				end
				@info "Calibration table written" path eras_written = table.era n_met = count(table.met) n_at_bound = count(table.at_bound)
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
	args         = String.(ARGS)
	profile_only = "--profile-only" in args
	force_sweep  = "--force" in args
	probe_mode   = "--probe" in args
	calibrate    = "--calibrate" in args
	if probe_mode && calibrate
		error("--probe and --calibrate are separate runs; choose one")
	end
	era_args = positional_args(args)

#	Era Selection from the Command Line
	default_eras = probe_mode ? PROBE_ERAS : collect(ANALYTIC_ERAS)
	eras = isempty(era_args) ? default_eras : parse.(Int, era_args)
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
		@info @sprintf("  Era %2d: %6d vertices, %7d arcs, %5d components, giant %5.1f%%, %5d citing, %5d both-degree, %5d restored  [%s]",
			row.era, row.n_vertices, row.n_arcs, row.n_components,
			100 * row.giant_component_share, row.n_citing, row.n_both_degree,
			row.n_restored_components, status)
	end

	viable = filter(is_viable, profiles)
	@info "Viable eras" n = nrow(viable) eras = viable.era MIN_GIANT_VERTICES MIN_GIANT_SHARE

#	Step 2: The Requested Mode
	if profile_only
		@info "Profile-only mode; stopping before the sweep"

	elseif probe_mode
		#	Validation Probe
			if Threads.nthreads() == 1
				@warn "Running on one thread; start Julia with --threads auto"
			end
			gammas = option_value(args, "--probe-gammas", PROBE_GAMMAS)
			seeds  = option_value(args, "--seeds", PROBE_SEEDS)
			probe_eras = force_sweep ? eras : intersect(eras, viable.era)
			if isempty(probe_eras)
				@error "No viable era in the requested set" eras
			else
				@info "Granularity probe" probe_eras gammas seeds OUT_DIR
				failed = run_granularity_probes(probe_eras, profiles, gammas, seeds)
				if !isempty(failed)
					@warn "Eras that did not complete" failed
				end
			end

	elseif calibrate
		#	Calibration and Partition Output
			alpha      = option_value(args, "--alpha", DEFAULT_ALPHA)
			anchor_g   = option_value(args, "--anchor-g", DEFAULT_ANCHOR_G)
			anchor_era = option_value(args, "--anchor-era", DEFAULT_ANCHOR_ERA)
			tolerance  = option_value(args, "--tolerance", DEFAULT_TOLERANCE)
			spec       = option_value(args, "--spec",
			                          @sprintf("alpha%.2f_g%d_e%02d", alpha, round(Int, anchor_g), anchor_era))
			cal_eras = force_sweep ? eras : intersect(eras, viable.era)
			if isempty(cal_eras)
				@error "No viable era in the requested set" eras
			else
				failed = run_calibration(cal_eras, profiles;
				                         alpha = alpha, anchor_g = anchor_g,
				                         anchor_era = anchor_era, tolerance = tolerance,
				                         spec = spec)
				if !isempty(failed)
					@warn "Eras that did not complete" failed
				end
			end

	else
		#	CHAMP Sweep
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
