#	champ_tools.jl -- CHAMP Resolution Sweeps and Partition Agreement
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	20 October 2026
#
#	WHERE THIS SITS. Third of three source files included by
#	community_detection_tools.jl:
#
#		pajek_io.jl      Pajek format in and out
#		leiden_tools.jl  Leiden community detection
#		champ_tools.jl   (this file)  CHAMP sweeps and partition agreement
#
#	MUST BE INCLUDED AFTER leiden_tools.jl. champ_community_detection calls
#	leiden_community_detection at every gamma in the sweep, and reuses
#	_graph_to_sparse_matrix, _binarize_matrix and calculate_modularity from that
#	file. Only the functions CHAMP adds are carried here, so nothing is defined
#	twice.
#
#	WHAT CHANGED FOR THE GRANULARITY DESIGN (granularity_design.md, fourth draft).
#
#		a.	THE ADMISSIBLE SET IS COMPUTED EXACTLY. The old test probed one gamma per
#			competitor, midway between the crossing point and the partition's own
#			gamma, and rejected the partition if the competitor won there. A
#			competitor can win at that one point while the partition is still
#			optimal elsewhere in the range, so the test rejected admissible
#			partitions: on grid 2 it flagged 13 of 24 envelope partitions for era
#			17 (design doc, Appendix A). _admissible_set now intersects the
#			half-lines each competitor imposes on gamma and keeps the partition
#			exactly when the intersection with the swept range is non-empty. It
#			also returns that interval, which is the dominance width the design
#			reports.
#
#		b.	THE SWEEP CAN KEEP EVERY PARTITION. The calibration curve the design
#			inverts is citer-weighted k_eff as a function of gamma, which needs a
#			membership vector at every gamma, not only at the selected one.
#			keep_partitions = true returns them all.
#
#		c.	ADJUSTED MUTUAL INFORMATION. The design's validation probe (sec. 9.1)
#			compares partitions across Leiden seeds with AMI, which corrects for
#			chance and suits partitions with different cluster counts. It sits
#			beside adjusted_rand_index, which is kept.
#
#		d.	THE ELBOW IS REPORTED, NOT USED. _select_curve_intersection and
#			gamma_star are still written to the sweep table so the envelope
#			diagnostics of the design's sec. 3 can be reproduced, but the
#			calibration takes no input from them.
#
#	HOW THE ENVELOPE WORKS. Each partition recovered at some gamma contributes a
#	line Q(gamma) = (A - gamma*P) / m (directed) or / 2m_eff (undirected), with
#	intercept A and slope -P. Coarse partitions sit high and fall steeply; fine
#	partitions sit lower and fall gently. The admissible set is the upper envelope
#	of those lines over the swept range. _select_curve_intersection takes the
#	consecutive admissible pair with the largest change in slope and returns the
#	shallower side, the elbow; the design doc, sec. 3.2, shows why that elbow is
#	a function of the grid floor on these networks.
#
#	Packages
	using DataFrames
	using LinearAlgebra
	using ProgressMeter
	using Random
	using SparseArrays

##########################
#   GRAPH PREPARATION    #
##########################

#	Helper Function for degree calculations: aggregate duplicate edges
	function _aggregate_multi_edges(edges::DataFrame; agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: DataFrame with src, dst, and optionally weight columns
			agg_func::Function: aggregation function for duplicate edges (default = sum)
		Returns:
			DataFrame: edges with duplicates aggregated
		Notes:
			Handles agg_func even when no weights exist.
			When no weights exist and agg_func=maximum, creates binary presence.
		"""

		#	Check if weights exist
			has_weights = hasproperty(edges, :weight)

		#	Group and aggregate
			if has_weights
				#	Aggregate weights for duplicate edges
					grouped = combine(groupby(edges, [:src, :dst]),
					                 :weight => agg_func => :weight)
			else
				#	Handle based on agg_func
					if agg_func == maximum
						#	For maximum without weights: binary presence (any edge = 1)
							grouped = combine(groupby(edges, [:src, :dst])) do _
								DataFrame(weight = 1.0)
							end
					elseif agg_func == sum
						#	For sum without weights: count edges
							grouped = combine(groupby(edges, [:src, :dst]),
							                 nrow => :weight)
					else
						#	For other functions: apply to ones
							grouped = combine(groupby(edges, [:src, :dst])) do grp
								DataFrame(weight = agg_func(ones(nrow(grp))))
							end
					end
			end

		#	Return aggregated edges
			return grouped
	end

##########################
#   HULL COEFFICIENTS    #
##########################

#	Helper Function for champ_community_detection: Calculate Partition Coefficients (igraph-aligned)
	function _calculate_partition_coefficients(adj::SparseMatrixCSC, membership::Vector{Int})
		"""
		Args:
			adj::SparseMatrixCSC: preprocessed adjacency matrix
			membership::Vector{Int}: community assignments
		Returns:
			Tuple{Float64,Float64}: (A, P) coefficients
		Notes:
			Matches igraph's undirected modularity convention.
			For directed graphs, use _calculate_partition_coefficients_directed.
		"""
		#	Validation
			@assert size(adj,1) == size(adj,2) "_calculate_partition_coefficients: adj must be square"
			@assert length(membership) == size(adj,1) "_calculate_partition_coefficients: membership length mismatch"

		#	Remap Membership to Contiguous 1..C
			labels = sort(unique(membership))
			label_to_col = Dict{Int,Int}(lab => i for (i, lab) in enumerate(labels))
			n = size(adj, 1)
			C = length(labels)
			mapped = Vector{Int}(undef, n)
			@inbounds for i in 1:n
				mapped[i] = label_to_col[membership[i]]
			end

		#	Build Indicator Matrix S (n × C)
			S = sparse(collect(1:n), mapped, ones(Float64, n), n, C)

		#	Calculate Effective Totals (igraph-style)
			d = diag(adj)
			two_m_eff = sum(adj) + sum(d)
			if two_m_eff == 0.0
				return (0.0, 0.0)
			end
			m_eff = two_m_eff / 2.0
			k_eff = vec(sum(adj, dims=2)) .+ d

		#	Calculate A = E_eff (Internal Weight with Doubled Loops)
			E_blocks = S' * adj * S
			E_diag   = S' * spdiagm(0 => d) * S
			E_eff    = sum(diag(E_blocks)) + sum(diag(E_diag))

		#	Calculate P = Expected Edges
			K_eff = vec(S' * k_eff)
			P = sum((K_eff .^ 2) ./ (2.0 * m_eff))

		#	Return Coefficients
			return (E_eff, P)
	end

#	Helper Function for champ_community_detection: Calculate Directed Partition Coefficients
	function _calculate_partition_coefficients_directed(adj::SparseMatrixCSC, membership::Vector{Int})
		"""
		Args:
			adj::SparseMatrixCSC: directed adjacency matrix (not symmetrized)
			membership::Vector{Int}: community assignments
		Returns:
			Tuple{Float64,Float64}: (A, P) coefficients for directed graphs
		Notes:
			Uses directed null model: K_out * K_in / m
		"""
		#	Validation
			@assert size(adj,1) == size(adj,2) "_calculate_partition_coefficients_directed: adj must be square"
			@assert length(membership) == size(adj,1) "_calculate_partition_coefficients_directed: membership length mismatch"

		#	Remap Membership to Contiguous 1..C
			labels = sort(unique(membership))
			label_to_col = Dict{Int,Int}(lab => i for (i, lab) in enumerate(labels))
			n = size(adj, 1)
			C = length(labels)
			mapped = Vector{Int}(undef, n)
			@inbounds for i in 1:n
				mapped[i] = label_to_col[membership[i]]
			end

		#	Build Indicator Matrix S (n × C)
			S = sparse(collect(1:n), mapped, ones(Float64, n), n, C)

		#	Calculate Total Weight and Degrees
			m = sum(adj)
			if m == 0.0
				return (0.0, 0.0)
			end
			k_out = vec(sum(adj, dims=2))  # out-degrees
			k_in = vec(sum(adj, dims=1))   # in-degrees

		#	Calculate A = Internal Edges
			E_blocks = S' * adj * S
			A = sum(diag(E_blocks))

		#	Calculate P = Expected Edges (Directed Null Model)
			K_out = vec(S' * k_out)
			K_in = vec(S' * k_in)
			P = sum((K_out .* K_in) ./ m)

		#	Return Coefficients
			return (A, P)
	end

##########################
#   ADMISSIBLE SET       #
##########################

#	Helper Function for champ_community_detection: Admissible Set by Half-Line Intersection
	function _admissible_set(A::AbstractVector{<:Real}, P::AbstractVector{<:Real},
	                         gammas::AbstractVector{<:Real})
		"""
		Args:
			A::AbstractVector: intercept of each partition's line, the internal weight
			P::AbstractVector: minus the slope of each line, the null-model weight
			gammas::AbstractVector: the swept resolutions, which fix the range
		Returns:
			NamedTuple: (admissible, lower, upper) -- a Bool per partition, and the
				interval of gamma over which it is optimal (NaN when inadmissible)
		Notes:
			Partition i is admissible when some gamma in the swept range exists at
			which no other partition scores higher:

				{ gamma in [gmin, gmax] : A_i - gamma*P_i >= A_j - gamma*P_j for all j }
				is non-empty.

			Each competitor j imposes one half-line, because the inequality is
			(A_i - A_j) >= gamma * (P_i - P_j). A competitor with SMALLER P than i
			makes P_i - P_j positive and bounds gamma from ABOVE: i is the coarser,
			steeper line and can beat j only at small gamma. A competitor with
			LARGER P bounds gamma from below. Equal P reduces to a comparison of
			intercepts. Intersecting the half-lines with the swept range leaves an
			interval, and i is admissible exactly when it is non-empty. O(n^2) in the
			number of grid points, and exact.

			This replaces a test that probed a single gamma per competitor and
			rejected admissible partitions (design doc, Appendix A).

			Two grid points that recovered the same partition have identical lines.
			Both are kept, with the same interval, so the count of admissible rows
			can exceed the count of distinct admissible partitions; the sweep table
			carries A and P, from which duplicates are visible.
		"""

		#	Validation
			n = length(A)
			if length(P) != n || length(gammas) != n
				throw(ArgumentError("_admissible_set: A, P and gammas differ in length"))
			end
			if n == 0
				return (admissible = Bool[], lower = Float64[], upper = Float64[])
			end

		#	Tolerance, Relative to the Scale of the Coefficients
			scale = max(1.0, maximum(abs, A), maximum(abs, P))
			tol   = 1e-9 * scale

		#	Swept Range
			γmin, γmax = extrema(gammas)

		#	Intersect the Half-Lines for Each Partition
			admissible = falses(n)
			lower = fill(NaN, n)
			upper = fill(NaN, n)
			for i in 1:n
				lo = Float64(γmin)
				hi = Float64(γmax)
				beaten = false
				for j in 1:n
					i == j && continue
					dA = A[i] - A[j]
					dP = P[i] - P[j]
					if abs(dP) <= tol
						#	Parallel lines: the higher intercept wins everywhere
						if A[j] > A[i] + tol
							beaten = true
							break
						end
					elseif dP > 0
						#	Competitor has smaller P: i wins only below the crossing
						hi = min(hi, dA / dP)
					else
						#	Competitor has larger P: i wins only above the crossing
						lo = max(lo, dA / dP)
					end
				end
				if !beaten && lo <= hi + 1e-12
					admissible[i] = true
					lower[i] = lo
					upper[i] = max(lo, hi)
				end
			end

		#	Assembling Result
			return (admissible = admissible, lower = lower, upper = upper)
	end

#	Helper Function for champ_community_detection: Curve-Intersection Selection
	function _select_curve_intersection(result::NamedTuple)
		"""
		Args:
			result::NamedTuple: CHAMP sweep result with :gammas, :A_coeffs, :P_coeffs,
				:n_communities_per_gamma, :dominant
		Returns:
			Int: index into result.gammas of the selected partition
		Notes:
			The canonical CHAMP elbow: among admissible partitions sorted by gamma,
			excluding the trivial one-community partition, take the consecutive
			pair with the largest change in P and return the member with the
			smaller P, which sits on the shallower side of the bend.

			On these citation networks P falls fastest at the left edge of the
			range, so the largest change is the first one and the selected gamma
			tracks the grid floor (design doc, sec. 3.2). The value is reported as
			gamma_star for the envelope diagnostics; the granularity calibration
			does not use it.

			Falls back to the first non-trivial admissible partition when fewer than
			two exist, and to index 1 when none does.
		"""

		#	Extract Fields
			gammas     = result.gammas
			P_coeffs   = result.P_coeffs
			n_comms_pg = result.n_communities_per_gamma
			dominant   = result.dominant

		#	Identify Dominant Indices Excluding Trivial
			dom_ix = findall(dominant)
			if isempty(dom_ix)
				dom_ix = collect(eachindex(gammas))
			end
			filter!(i -> n_comms_pg[i] > 1, dom_ix)

		#	Fall Back If Insufficient Dominant Partitions
			if isempty(dom_ix)
				return 1
			end
			if length(dom_ix) < 2
				return dom_ix[1]
			end

		#	Sort by γ for Adjacent-Pair Differencing
			sort!(dom_ix; by = i -> gammas[i])

		#	Find Consecutive Pair with Largest Slope Difference
			best_pair_lo = dom_ix[1]
			best_pair_hi = dom_ix[2]
			best_diff    = abs(P_coeffs[dom_ix[2]] - P_coeffs[dom_ix[1]])

			for k in 2:(length(dom_ix) - 1)
				lo   = dom_ix[k]
				hi   = dom_ix[k + 1]
				diff = abs(P_coeffs[hi] - P_coeffs[lo])
				if diff > best_diff
					best_diff    = diff
					best_pair_lo = lo
					best_pair_hi = hi
				end
			end

		#	Return the Shallower-Slope Side of the Bend
			return P_coeffs[best_pair_hi] < P_coeffs[best_pair_lo] ? best_pair_hi : best_pair_lo
	end

##########################
#   CHAMP                #
##########################

#	CHAMP: Convex Hull of Admissible Modularity Partitions
	function champ_community_detection(edges::DataFrame;
	                                  nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}}=nothing,
	                                  resolution::Union{Float64,Nothing}=nothing,
	                                  resolution_range::Tuple{Float64,Float64}=(0.1, 1.8),
	                                  n_resolutions::Int=30,
	                                  n_runs_per_gamma::Int=5,
	                                  n_iterations_per_run::Int=10,
	                                  weighted::Bool=false,
	                                  directed::Bool=false,
	                                  agg_func::Union{Function,Nothing}=nothing,
	                                  seed::Union{Int,Nothing}=nothing,
	                                  keep_partitions::Bool=false,
	                                  show_progress::Bool=true)
		"""
		Args:
			edges::DataFrame: :src, :dst, optional :weight
			nodes::Union{Nothing,DataFrame,Vector}: node universe (optional)
			resolution::Union{Float64,Nothing}: single γ or sweep if nothing
			resolution_range::Tuple: γ range for sweep (default = (0.1, 1.8))
			n_resolutions::Int: number of γ values in sweep (default = 30)
			n_runs_per_gamma::Int: Leiden runs per γ (default = 5)
			n_iterations_per_run::Int: max iterations per run (default = 10)
			weighted::Bool: treat graph as weighted (default = false)
			directed::Bool: treat graph as directed (default = false)
			agg_func::Function: edge aggregation (default = sum if weighted)
			seed::Union{Int,Nothing}: RNG seed
			keep_partitions::Bool: return the membership at every γ (default = false)
			show_progress::Bool: display progress bars (default = true)
		Returns:
			NamedTuple: (membership, resolution_used, modularity, n_communities, node_names,
			             gammas, A_coeffs, P_coeffs, modularities, n_communities_per_gamma,
			             dominant, dominance_lower, dominance_upper, best_index, memberships)
		Notes:
			The γ sweep is parallelized with Threads.@threads. Each γ task calls
			leiden_community_detection with parallel_runs=false to avoid nested
			threading.

			memberships is a Vector{Vector{Int}} with one entry per γ when
			keep_partitions is true, and empty otherwise. The granularity design
			needs every partition, since its calibration curve is citer-weighted
			k_eff at each γ.

			best_index is the CHAMP elbow, reported for the envelope diagnostics.
		"""

		#	Validation
			@assert hasproperty(edges, :src) && hasproperty(edges, :dst) "edges must have :src and :dst"
			if nrow(edges) == 0
				return (membership=Int[], resolution_used=0.0, modularity=0.0,
				       n_communities=0, node_names=String[],
				       gammas=Float64[], A_coeffs=Float64[], P_coeffs=Float64[],
				       modularities=Float64[], n_communities_per_gamma=Int[],
				       dominant=Bool[], dominance_lower=Float64[], dominance_upper=Float64[],
				       best_index=0, memberships=Vector{Int}[])
			end

		#	Set Aggregation Strategy
			if isnothing(agg_func)
				agg_func = (weighted && hasproperty(edges, :weight)) ? sum : maximum
			end

		#	Aggregate Multi-Edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Build Base Adjacency
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _graph_to_sparse_matrix(clean_edges;
			                                                        nodes=nodes,
			                                                        weighted=use_weights)

		#	Binarize if Unweighted
			if !weighted
				adj = map!(x -> x == 0.0 ? 0.0 : 1.0, copy(adj), adj)
			end

		#	Symmetrize for Undirected
			if !directed
				if weighted
					adj = 0.5 .* (adj + adj')
				else
					adj = max.(adj, adj')
				end
			end

		#	Extract Node Names
			if idx_to_node isa DataFrame
				df = idx_to_node
				cols = names(df)
				if :label in cols
					node_names = string.(df.label)
				elseif :id in cols
					node_names = string.(df.id)
				else
					firstcol = cols[1]
					node_names = string.(df[!, firstcol])
				end
			else
				node_names = string.(idx_to_node)
			end

		#	Define Resolution Grid
			gammas = (resolution === nothing) ?
				collect(range(resolution_range[1], resolution_range[2]; length=n_resolutions)) :
				[resolution]

		#	Storage for Partitions
			all_partitions = Vector{NamedTuple}(undef, length(gammas))
			all_coeffs     = Vector{Tuple{Float64,Float64}}(undef, length(gammas))

		#	Progress Bar Setup
			prog = nothing
			prog_lock = ReentrantLock()
			if show_progress
				desc = "CHAMP γ sweep ($(Threads.nthreads()) threads)"
				prog = Progress(length(gammas), desc = desc, enabled = true)
			end

		#	Phase 1: Resolution Sweep (Threaded)
			Threads.@threads for ix in 1:length(gammas)
				γ = gammas[ix]

				#	Run Leiden at This Resolution (Serial Inside)
					res = leiden_community_detection(clean_edges;
					                                  nodes        = nodes,
					                                  resolution   = γ,
					                                  n_iterations = n_iterations_per_run,
					                                  n_runs       = n_runs_per_gamma,
					                                  weighted     = weighted,
					                                  directed     = directed,
					                                  seed         = seed,
					                                  parallel_runs = false,
					                                  show_progress = false)

				#	Calculate Coefficients
					if directed
						Aeff, Peff = _calculate_partition_coefficients_directed(adj, res.membership)
					else
						Aeff, Peff = _calculate_partition_coefficients(adj, res.membership)
					end

				#	Store Results in Pre-Allocated Slots
					all_partitions[ix] = (
						membership    = res.membership,
						gamma         = γ,
						modularity    = res.modularity,
						n_communities = res.n_communities,
					)
					all_coeffs[ix] = (Aeff, Peff)

				#	Update Progress Bar
					if show_progress
						lock(prog_lock) do
							next!(prog)
						end
					end
			end

		#	Phase 2: Admissible Set
			A_coeffs = [c[1] for c in all_coeffs]
			P_coeffs = [c[2] for c in all_coeffs]
			if length(gammas) > 1
				adm = _admissible_set(A_coeffs, P_coeffs, gammas)
				dominant        = adm.admissible
				dominance_lower = adm.lower
				dominance_upper = adm.upper
			else
				dominant        = trues(1)
				dominance_lower = [gammas[1]]
				dominance_upper = [gammas[1]]
			end

		#	Build Intermediate Result for Selection Helper
			intermediate = (
				gammas                  = gammas,
				A_coeffs                = A_coeffs,
				P_coeffs                = P_coeffs,
				modularities            = [p.modularity for p in all_partitions],
				n_communities_per_gamma = [p.n_communities for p in all_partitions],
				dominant                = dominant
			)

		#	Phase 3: Elbow of the Envelope, Reported Only
			best_ix = _select_curve_intersection(intermediate)

		#	Return Best Partition with Full Sweep Data
			best = all_partitions[best_ix]
			memberships = keep_partitions ? [p.membership for p in all_partitions] : Vector{Int}[]
			return (
				membership              = best.membership,
				resolution_used         = best.gamma,
				modularity              = best.modularity,
				n_communities           = best.n_communities,
				node_names              = node_names,
				gammas                  = gammas,
				A_coeffs                = intermediate.A_coeffs,
				P_coeffs                = intermediate.P_coeffs,
				modularities            = intermediate.modularities,
				n_communities_per_gamma = intermediate.n_communities_per_gamma,
				dominant                = collect(dominant),
				dominance_lower         = dominance_lower,
				dominance_upper         = dominance_upper,
				best_index              = best_ix,
				memberships             = memberships
			)
	end
	@doc raw"""
	**Description**
	Implements CHAMP (Convex Hull of Admissible Modularity Partitions). Performs a
	sweep across resolution values γ, computes each partition's hull coefficients,
	identifies the admissible set on the upper envelope of the modularity-vs-γ
	lines together with each admissible partition's dominance interval, and
	reports the partition at the elbow of that envelope. The elbow is a diagnostic
	here, not a selection: the granularity design calibrates γ per era from the
	citer-weighted effective community count instead.

	Supports directed and undirected graphs with optional edge weights. The γ
	sweep is parallelized across CPU threads.

	**Usage**
	`champ_community_detection(edges; nodes=nothing, resolution=nothing,
	                          resolution_range=(0.1,1.8), n_resolutions=30,
	                          weighted=false, directed=false, n_runs_per_gamma=5,
	                          n_iterations_per_run=10, seed=nothing,
	                          keep_partitions=false, show_progress=true)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, optional `:weight`.
	- `nodes::Union{Nothing,DataFrame,Vector}`: Node universe (includes isolates if provided).
	- `resolution::Float64|nothing`: If supplied, run Leiden at this single γ and skip the sweep. If `nothing` (default), perform a full resolution sweep.
	- `resolution_range::Tuple`: Range `(γ_min, γ_max)` for the sweep (default `(0.1, 1.8)`).
	- `n_resolutions::Int`: Number of γ values in the sweep grid (default `30`).
	- `n_runs_per_gamma::Int`: Number of independent Leiden multi-starts per γ value (default `5`).
	- `n_iterations_per_run::Int`: Maximum Leiden iterations per run (default `10`).
	- `weighted::Bool`: Use edge weights from the `:weight` column (default `false`).
	- `directed::Bool`: Treat the graph as directed (default `false`).
	- `agg_func::Function`: Edge aggregation when input has multi-edges (default `sum` if weighted, else `maximum`).
	- `seed::Int`: Random seed for reproducibility. Per-thread RNGs are derived from this seed.
	- `keep_partitions::Bool`: Return the membership vector at every γ (default `false`).
	- `show_progress::Bool`: Display progress bar during the γ sweep (default `true`).

	**Details**
	CHAMP performs three phases:

	1. **Sweep**: Run Leiden at each γ in the resolution grid. For each resulting partition, compute the two hull coefficients:
	   - `A` — total edge weight within communities (the modularity "internal" term)
	   - `P` — expected within-community weight under the null model (the modularity "penalty" term)
	   The modularity at any γ for this partition equals `(A - γP) / m_total`, so the partition's modularity is a linear function of γ with intercept `A` and slope `-P`.
	2. **Admissible set**: Partition `i` is admissible when some γ in the swept range exists at which no other partition scores higher. Each competitor `j` constrains γ to a half-line, since `A_i - γP_i ≥ A_j - γP_j` is equivalent to `(A_i - A_j) ≥ γ(P_i - P_j)`: a competitor with smaller `P` bounds γ from above, one with larger `P` from below. The intersection of those half-lines with the swept range is the partition's dominance interval, returned as `dominance_lower` and `dominance_upper`; the partition is admissible exactly when the interval is non-empty.
	3. **Elbow**: Among admissible partitions sorted by γ, excluding the trivial single-community partition, the consecutive pair with the largest change in `P` marks the bend, and the member with smaller `P` is reported as `best_index`. On the era citation networks this elbow tracks the grid floor, so it is recorded for the envelope diagnostics and not used for selection.

	**Modularity Conventions**

	Coefficients match igraph's modularity calculation:
	- **Undirected**: `Q(γ) = (A - γP) / (2 m_eff)` where `m_eff = sum(adj) / 2 + sum(diag(adj)) / 2` to account for self-loops igraph-style.
	- **Directed**: `Q(γ) = (A - γP) / m` with directed null model `K_out · K_in / m`.

	**Value**

	NamedTuple containing:
	- `membership::Vector{Int}`: Community assignments for the elbow partition.
	- `resolution_used::Float64`: γ value of the elbow partition.
	- `modularity::Float64`: Modularity score of the elbow partition at its γ.
	- `n_communities::Int`: Number of communities in the elbow partition.
	- `node_names::Vector{String}`: Original node identifiers in adjacency-matrix order.
	- `gammas::Vector{Float64}`: All γ values in the sweep.
	- `A_coeffs::Vector{Float64}`: A coefficient for each γ partition.
	- `P_coeffs::Vector{Float64}`: P coefficient for each γ partition.
	- `modularities::Vector{Float64}`: Modularity for each γ partition (computed at each partition's own γ).
	- `n_communities_per_gamma::Vector{Int}`: Community count for each γ partition.
	- `dominant::Vector{Bool}`: `true` for partitions in the admissible set.
	- `dominance_lower::Vector{Float64}`, `dominance_upper::Vector{Float64}`: the interval of γ over which each admissible partition is optimal; `NaN` for inadmissible ones.
	- `best_index::Int`: Index into `gammas` of the elbow partition.
	- `memberships::Vector{Vector{Int}}`: One membership per γ when `keep_partitions` is true, otherwise empty.

	**References**
	1. Weir WH, Emmons S, Gibson R, Taylor D, Mucha PJ (2017) "Post-processing partitions to identify domains of modularity optimization." *Algorithms* 10(3):93. doi:10.3390/a10030093
	2. Github implementation: https://github.com/wweir827/CHAMP

	**See Also**
	`leiden_community_detection`, `calculate_modularity`, `champ_era_sweep`
	""" champ_community_detection

##########################
#   PARTITION AGREEMENT  #
##########################

#	Helper Function for adjusted_rand_index and adjusted_mutual_information: Contingency Table
	function _contingency(partition1::Vector{Int}, partition2::Vector{Int})
		"""
		Args:
			partition1::Vector{Int}: first partition
			partition2::Vector{Int}: second partition, same length
		Returns:
			Tuple: (counts::Dict{Tuple{Int,Int},Int}, a::Vector{Int}, b::Vector{Int})
				-- the non-zero cells keyed by remapped (row, col), and the marginals
		Notes:
			Kept sparse because two partitions of a few thousand citing articles
			into hundreds of communities have a contingency table that is almost
			entirely zero.
		"""

		#	Validation
			n = length(partition1)
			if n != length(partition2)
				throw(ArgumentError("partitions must have the same length"))
			end

		#	Remap Labels
			map1 = Dict{Int,Int}()
			map2 = Dict{Int,Int}()
			counts = Dict{Tuple{Int,Int},Int}()
			a = Int[]
			b = Int[]
			for i in 1:n
				r = get!(map1, partition1[i]) do
					push!(a, 0); length(a)
				end
				c = get!(map2, partition2[i]) do
					push!(b, 0); length(b)
				end
				a[r] += 1
				b[c] += 1
				counts[(r, c)] = get(counts, (r, c), 0) + 1
			end

		#	Assembling Result
			return (counts, a, b)
	end

#	Adjusted Rand Index Calculation
	function adjusted_rand_index(partition1::Vector{Int}, partition2::Vector{Int})
		"""
		Args:
			partition1::Vector{Int}: first partition/clustering
			partition2::Vector{Int}: second partition/clustering
		Returns:
			Float64: ARI score between -1 and 1 (1 = perfect agreement)
		Notes:
			Calculates Adjusted Rand Index between two partitions.
			Corrects for chance agreement in clustering comparisons.
		"""

		#	Validation
			n = length(partition1)
			if n != length(partition2)
				throw(ArgumentError("Partitions must have same length"))
			end
			if n == 0
				return 1.0
			end

		#	Contingency Table and Marginals
			counts, a, b = _contingency(partition1, partition2)

		#	Pair Counts
			comb_nij = sum(c * (c - 1) for c in values(counts)) / 2
			comb_ai  = sum(x * (x - 1) for x in a) / 2
			comb_bj  = sum(x * (x - 1) for x in b) / 2
			total_comb = n * (n - 1) / 2

		#	Expected and Maximum Index
			expected_index = (comb_ai * comb_bj) / total_comb
			max_index = (comb_ai + comb_bj) / 2

		#	Handle Edge Cases
			if max_index == expected_index
				return comb_nij == expected_index ? 1.0 : 0.0
			end

		#	Adjusted Rand Index
			return (comb_nij - expected_index) / (max_index - expected_index)
	end

#	Helper Function for adjusted_mutual_information: Log Factorials 0..N
	function _log_factorials(N::Int)
		"""
		Args:
			N::Int: largest factorial needed
		Returns:
			Vector{Float64}: lf[k + 1] = log(k!) for k in 0..N
		Notes:
			A cumulative sum of logs, so the hypergeometric terms of the expected
			mutual information can be formed without a special-functions dependency.
		"""

		#	Accumulate
			lf = zeros(Float64, N + 1)
			for k in 1:N
				lf[k + 1] = lf[k] + log(k)
			end

		#	Assembling Result
			return lf
	end

#	Adjusted Mutual Information Calculation
	function adjusted_mutual_information(partition1::Vector{Int}, partition2::Vector{Int};
	                                     normalization::Symbol = :max)
		"""
		Args:
			partition1::Vector{Int}: first partition
			partition2::Vector{Int}: second partition, same length
			normalization::Symbol: :max (default), :arithmetic, :geometric or :min --
				which mean of the two entropies bounds the mutual information
		Returns:
			Float64: AMI, 1 for identical partitions, about 0 for independent ones
		Notes:
			AMI = (MI - E[MI]) / (norm(H1, H2) - E[MI]), with E[MI] the expectation
			of the mutual information under the permutation model, the
			hypergeometric sum of Vinh, Epps and Bailey (2010). The default
			normalization is the maximum entropy, the AMI_max of that paper; the
			:arithmetic form matches scikit-learn's default.

			The expected term is a sum over contingency cells and over the feasible
			values of each cell, so its cost is sum_ij min(a_i, b_j). On the citing
			articles of one era, a few thousand vertices in a few hundred
			communities, it runs in well under a second.

			Two partitions that are both trivial (one community each) return 1.0;
			if the normalizing denominator is zero otherwise, 0.0 is returned.
		"""

		#	Validation
			N = length(partition1)
			if N != length(partition2)
				throw(ArgumentError("adjusted_mutual_information: partitions must have the same length"))
			end
			if N == 0
				return 1.0
			end
			if !(normalization in (:max, :arithmetic, :geometric, :min))
				throw(ArgumentError("adjusted_mutual_information: unknown normalization $normalization"))
			end

		#	Contingency Table and Marginals
			counts, a, b = _contingency(partition1, partition2)
			Nf = Float64(N)

		#	Entropies
			H1 = -sum((x / Nf) * log(x / Nf) for x in a)
			H2 = -sum((x / Nf) * log(x / Nf) for x in b)
			if H1 == 0.0 && H2 == 0.0
				return 1.0
			end

		#	Mutual Information
			MI = 0.0
			for ((r, c), nij) in counts
				MI += (nij / Nf) * log(Nf * nij / (a[r] * b[c]))
			end

		#	Expected Mutual Information Under the Permutation Model
			lf = _log_factorials(N)
			EMI = 0.0
			for (r, ai) in enumerate(a), (c, bj) in enumerate(b)
				lo = max(1, ai + bj - N)
				hi = min(ai, bj)
				lo > hi && continue
				base = lf[ai + 1] + lf[bj + 1] + lf[N - ai + 1] + lf[N - bj + 1] - lf[N + 1]
				for nij in lo:hi
					logp = base - lf[nij + 1] - lf[ai - nij + 1] - lf[bj - nij + 1] -
					       lf[N - ai - bj + nij + 1]
					EMI += (nij / Nf) * log(Nf * nij / (ai * bj)) * exp(logp)
				end
			end

		#	Normalization
			bound = normalization == :max        ? max(H1, H2) :
			        normalization == :min        ? min(H1, H2) :
			        normalization == :geometric  ? sqrt(H1 * H2) :
			                                       (H1 + H2) / 2
			denom = bound - EMI
			if abs(denom) < 1e-12
				return abs(MI - EMI) < 1e-12 ? 1.0 : 0.0
			end

		#	Assembling Result
			return (MI - EMI) / denom
	end
	@doc raw"""
	**Description**
	Adjusted mutual information between two partitions of the same vertex set.
	Corrects the mutual information for the agreement expected by chance under
	the permutation model, so partitions with different numbers of clusters can be
	compared on an equal footing. The granularity design's validation probe uses
	it to measure how far Leiden partitions at one γ agree across seeds.

	**Usage**
	`adjusted_mutual_information(partition1, partition2; normalization=:max)`

	**Arguments**
	- `partition1::Vector{Int}`, `partition2::Vector{Int}`: Community labels, one per vertex, in the same vertex order.
	- `normalization::Symbol`: Which mean of the two entropies bounds the mutual information: `:max` (default, Vinh et al.'s AMI_max), `:arithmetic` (scikit-learn's default), `:geometric` or `:min`.

	**Details**
	`AMI = (MI − E[MI]) / (norm(H₁, H₂) − E[MI])`, where `E[MI]` is the
	hypergeometric expectation of Vinh, Epps and Bailey (2010). The expectation is
	a sum over contingency cells and over the feasible values of each cell; its
	cost is `Σ_ij min(a_i, b_j)`, which on the citing articles of one era is
	negligible. Log factorials are accumulated directly, so the function has no
	special-functions dependency.

	**Value**
	A `Float64`: 1 for identical partitions, near 0 for independent ones, and
	negative when agreement is below chance.

	**Examples**
	```julia
		#	Agreement of two Leiden runs on the citing articles of one era
			ami = adjusted_mutual_information(run_a[citing], run_b[citing])
	```

	**References**
	Vinh NX, Epps J, Bailey J (2010) "Information theoretic measures for clusterings
	comparison: variants, properties, normalization and correction for chance."
	*Journal of Machine Learning Research* 11:2837–2854.

	**See Also**
	`adjusted_rand_index`, `granularity_probe`
	""" adjusted_mutual_information

###############
#   EXPORTS   #
###############

#	CHAMP Sweeps and Partition Agreement
	export champ_community_detection,
		   adjusted_rand_index,
		   adjusted_mutual_information
