#	champ_tools.jl -- CHAMP Resolution Selection
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHERE THIS SITS. Third of three source files included by
#	community_detection_tools.jl:
#
#		pajek_io.jl      Pajek format in and out
#		leiden_tools.jl  Leiden community detection
#		champ_tools.jl   (this file)  CHAMP resolution selection
#
#	MUST BE INCLUDED AFTER leiden_tools.jl. champ_community_detection calls
#	leiden_community_detection at every gamma in the sweep, and reuses
#	_graph_to_sparse_matrix, _binarize_matrix and calculate_modularity from that
#	file. Only the five functions CHAMP adds are carried here, so nothing is
#	defined twice.
#
#	Pulled verbatim from Large_Graph_Similarity.jl. The dependency closure of
#	champ_community_detection was computed rather than assembled by hand; these are
#	the members of it not already supplied by leiden_tools.jl:
#
#		_aggregate_multi_edges                       collapse duplicate ties
#		_calculate_partition_coefficients            the (A, P) pair, undirected
#		_calculate_partition_coefficients_directed   the (A, P) pair, directed
#		_select_curve_intersection                   the elbow of the upper hull
#		champ_community_detection                    the sweep and the selection
#
#	adjusted_rand_index is carried alongside. It is not a CHAMP dependency, but it
#	is the instrument for comparing one partition against another on a fixed node
#	set -- a CHAMP solution against the legacy Pajek partition for the same era,
#	say -- which the scale assessment will want.
#
#	HOW THE SELECTION WORKS. Each partition recovered at some gamma contributes a
#	line Q(gamma) = (A - gamma*P) / 2m, with intercept A and slope -P. Coarse
#	partitions sit high and fall steeply; fine partitions sit lower and fall
#	gently. Dominance analysis keeps only the partitions that are optimal
#	somewhere in the swept range, which is the upper envelope of those lines.
#	_select_curve_intersection then takes the consecutive admissible pair with the
#	largest change in slope and returns the shallower side -- the elbow, past which
#	additional resolution stops buying structural information.
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

#	Helper Function for champ_community_detection: Curve-Intersection Selection
	function _select_curve_intersection(result::NamedTuple)
		"""
		Args:
			result::NamedTuple: CHAMP sweep result with :gammas, :A_coeffs, :P_coeffs,
				:n_communities_per_gamma, :dominant
		Returns:
			Int: index into result.gammas of the selected partition
		Notes:
			Implements the canonical CHAMP optimization criterion: identify the
			partition at the bend of the upper envelope on the convex-hull plot.
			The bend is where two consecutive admissible partitions have the largest
			difference in slope, because that is where the envelope changes direction
			most sharply.

			Procedure:
			1. Restrict to dominant partitions, sorted by γ.
			2. Exclude trivial single-community partitions, which always dominate at
			   low γ but represent absence of structure rather than structure.
			3. For each consecutive pair (i, i+1) of dominant partitions, compute the
			   absolute slope difference |slope[i+1] - slope[i]| where slope = -P.
			4. Find the pair with the largest slope difference.
			5. Return the partition with the smaller P (i.e., the one on the
			   shallower-slope side of the bend), since this is the partition
			   discovered just after the regime change to consolidated communities.

			On networks with a clear structural scale, the selected γ corresponds
			to the visual elbow where the upper envelope visibly transitions from
			steep (low-γ, many small communities) to shallow (high-γ, fewer large
			communities) line slopes.

			Falls back to the middle dominant partition if too few dominant
			partitions exist to compute a bend (< 2 after filtering trivials).
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
			#	Slope of line i is -P_coeffs[i]. Largest |slope_change| = largest |P_change|.
			#	The bend is the pair (i, i+1) where the line slopes change most sharply.
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
			#	The partition with smaller P is on the shallower side, just after
			#	the regime change to consolidated community structure.
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
			show_progress::Bool: display progress bars (default = true)
		Returns:
			NamedTuple: (membership, resolution_used, modularity, n_communities, node_names,
			             gammas, A_coeffs, P_coeffs, modularities, n_communities_per_gamma,
			             dominant, best_index)
		Notes:
			The γ sweep is parallelized with Threads.@threads. Each γ task calls
			leiden_community_detection with parallel_runs=false to avoid nested
			threading.

			Selection identifies the partition at the bend of the convex-hull upper
			envelope — the γ at which the steep-slope (low-γ) and shallow-slope
			(high-γ) regimes transition most rapidly. This is the canonical CHAMP
			optimum: the γ value beyond which additional resolution stops yielding
			meaningful structural information.
		"""

		#	Validation
			@assert hasproperty(edges, :src) && hasproperty(edges, :dst) "edges must have :src and :dst"
			if nrow(edges) == 0
				return (membership=Int[], resolution_used=0.0, modularity=0.0,
				       n_communities=0, node_names=String[],
				       gammas=Float64[], A_coeffs=Float64[], P_coeffs=Float64[],
				       modularities=Float64[], n_communities_per_gamma=Int[],
				       dominant=Bool[], best_index=0)
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

		#	Phase 2: Dominance Analysis
			dominant = trues(length(gammas))

			if length(gammas) > 1
				nP = length(all_partitions)
				γmin, γmax = minimum(gammas), maximum(gammas)

				#	Check Dominance Relationships
					for i in 1:nP
						Ai, Pi = all_coeffs[i]
						for j in 1:nP
							i == j && continue
							Aj, Pj = all_coeffs[j]

							if !isapprox(Pi, Pj; atol=1e-12)
								γcross = (Aj - Ai) / (Pi - Pj + 1e-12)
								if γmin - 1e-6 < γcross < γmax + 1e-6
									γtest = clamp((γcross + all_partitions[i].gamma)/2, γmin, γmax)
									if (Aj - γtest*Pj) > (Ai - γtest*Pi) + 1e-12
										dominant[i] = false
										break
									end
								elseif (Aj - all_partitions[i].gamma*Pj) >
								       (Ai - all_partitions[i].gamma*Pi) + 1e-12
									dominant[i] = false
									break
								end
							else
								if Aj > Ai + 1e-12
									dominant[i] = false
									break
								end
							end
						end
					end
			end

		#	Build Intermediate Result for Selection Helper
			intermediate = (
				gammas                  = gammas,
				A_coeffs                = [c[1] for c in all_coeffs],
				P_coeffs                = [c[2] for c in all_coeffs],
				modularities            = [p.modularity for p in all_partitions],
				n_communities_per_gamma = [p.n_communities for p in all_partitions],
				dominant                = dominant
			)

		#	Phase 3: Select Partition at the Convex-Hull Bend
			best_ix = _select_curve_intersection(intermediate)

		#	Return Best Partition with Full Sweep Data
			best = all_partitions[best_ix]
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
				dominant                = dominant,
				best_index              = best_ix
			)
	end
	@doc raw"""
	**Description**
	Implements CHAMP (Convex Hull of Admissible Modularity Partitions) to identify
	the resolution-stable community partition of a graph. Performs a sweep across
	resolution values γ, identifies the admissible set of partitions on the upper
	envelope of the modularity-vs-γ convex hull, and selects the partition at the
	bend of that envelope — the γ value at which the trade-off between
	community-count fineness and modularity quality is optimal.

	Supports directed and undirected graphs with optional edge weights. The γ
	sweep is parallelized across CPU threads for substantial speedup on multi-core
	machines.

	**Usage**
	`champ_community_detection(edges; nodes=nothing, resolution=nothing,
	                          resolution_range=(0.1,1.8), n_resolutions=30,
	                          weighted=false, directed=false, n_runs_per_gamma=5,
	                          n_iterations_per_run=10, seed=nothing,
	                          show_progress=true)`

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
	- `show_progress::Bool`: Display progress bar during the γ sweep (default `true`).

	**Details**
	CHAMP performs three phases:

	1. **Sweep**: Run Leiden at each γ in the resolution grid. For each resulting partition, compute the two CHAMP coefficients:
	   - `A` — total edge weight within communities (the modularity "internal" term)
	   - `P` — expected within-community weight under the null model (the modularity "penalty" term)
	   The modularity at any γ for this partition equals `(A - γP) / m_total`, so the partition's modularity is a linear function of γ with intercept `A` and slope `-P`.
	2. **Dominance**: For each partition i, test whether some other partition j has a higher `A - γP` value across the entire resolution range. Partitions that survive (are optimal somewhere in the range) form the *admissible set* on the convex hull.
	3. **Selection at the convex-hull bend**: Identify the partition at the elbow of the upper envelope. Each admissible partition contributes a line with slope `-P`; the steep slopes (large P) come from low-γ partitions with many small communities, and the shallow slopes (small P) come from high-γ partitions with fewer larger communities. The bend of the envelope is the γ at which slope transitions most rapidly from steep to shallow. Trivial single-community partitions are excluded from selection because they always lie on the convex hull at low γ but represent the absence of community structure rather than its presence.

	The γ sweep is parallelized using `Threads.@threads`. Each γ task calls `leiden_community_detection` with `parallel_runs=false` to prevent nested thread oversubscription.

	**Modularity Conventions**

	Coefficients match igraph's modularity calculation:
	- **Undirected**: `Q(γ) = (A - γP) / (2 m_eff)` where `m_eff = sum(adj) / 2 + sum(diag(adj)) / 2` to account for self-loops igraph-style.
	- **Directed**: `Q(γ) = (A - γP) / m` with directed null model `K_out · K_in / m`.

	**Threading**

	The γ sweep is parallelized using `Threads.@threads`. To control thread count, launch Julia with `julia --threads N` or set `JULIA_NUM_THREADS`. Speedup scales approximately linearly with thread count when `n_resolutions ≥ n_threads`.

	**Value**

	NamedTuple containing:
	- `membership::Vector{Int}`: Community assignments for the selected partition.
	- `resolution_used::Float64`: γ value of the selected partition.
	- `modularity::Float64`: Modularity score of the selected partition at its γ.
	- `n_communities::Int`: Number of communities in the selected partition.
	- `node_names::Vector{String}`: Original node identifiers in adjacency-matrix order.
	- `gammas::Vector{Float64}`: All γ values in the sweep.
	- `A_coeffs::Vector{Float64}`: A coefficient for each γ partition.
	- `P_coeffs::Vector{Float64}`: P coefficient for each γ partition.
	- `modularities::Vector{Float64}`: Modularity for each γ partition (computed at each partition's own γ).
	- `n_communities_per_gamma::Vector{Int}`: Community count for each γ partition.
	- `dominant::Vector{Bool}`: `true` for partitions in the admissible set (on the convex hull).
	- `best_index::Int`: Index into `gammas` of the selected partition.

	The sweep data fields enable post-hoc visualization (see `plot_champ_sweep`) for users who want to verify the selected partition against the full sweep.

	**Examples**

	```julia
			#	Default analysis on a directed weighted network
				result = champ_community_detection(edges;
												weighted = true,
												directed = true)
				println("Selected γ: $(result.resolution_used)")
				println("Modularity: $(result.modularity)")

			#	Custom resolution range
				result = champ_community_detection(edges;
												resolution_range = (0.2, 2.0),
												n_resolutions    = 40,
												weighted         = true,
												directed         = true)

			#	Single-resolution Leiden via CHAMP (skips sweep, runs Leiden once)
				result = champ_community_detection(edges;
												resolution = 1.0,
												weighted   = true)
	```

	**References**
	1. Weir WH, Emmons S, Gibson R, Taylor D, Mucha PJ (2017) "Post-processing partitions to identify domains of modularity optimization." *Algorithms* 10(3):93. doi:10.3390/a10030093
	2. Github implementation: https://github.com/wweir827/CHAMP

	**See Also**
	`leiden_community_detection`, `calculate_modularity`, `plot_champ_sweep`
	""" champ_community_detection

##########################
#   PARTITION AGREEMENT  #
##########################

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
		
		#	Build contingency table
			labels1 = unique(partition1)
			labels2 = unique(partition2)
			n_clusters1 = length(labels1)
			n_clusters2 = length(labels2)
			
		#	Create mapping for efficient indexing
			map1 = Dict(label => i for (i, label) in enumerate(labels1))
			map2 = Dict(label => i for (i, label) in enumerate(labels2))
			
		#	Build contingency matrix
			contingency = zeros(Int, n_clusters1, n_clusters2)
			for i in 1:n
				row = map1[partition1[i]]
				col = map2[partition2[i]]
				contingency[row, col] += 1
			end
		
		#	Calculate marginals
			sum_rows = sum(contingency, dims=2)
			sum_cols = sum(contingency, dims=1)
		
		#	Calculate index components
			sum_nij_2 = sum(contingency .^ 2)
			sum_ai_2 = sum(sum_rows .^ 2)
			sum_bj_2 = sum(sum_cols .^ 2)
			
		#	Calculate combinations
			comb_nij = (sum_nij_2 - n) / 2
			comb_ai = (sum_ai_2 - n) / 2
			comb_bj = (sum_bj_2 - n) / 2
			
		#	Total combinations
			total_comb = n * (n - 1) / 2
		
		#	Expected index
			expected_index = (comb_ai * comb_bj) / total_comb
		
		#	Maximum index
			max_index = (comb_ai + comb_bj) / 2
		
		#	Handle edge cases
			if max_index == expected_index
				if comb_nij == expected_index
					return 1.0
				else
					return 0.0
				end
			end
		
		#	Adjusted Rand Index
			ari = (comb_nij - expected_index) / (max_index - expected_index)
		
		return ari
	end

###############
#   EXPORTS   #
###############

#	CHAMP Resolution Selection
	export champ_community_detection,
		   adjusted_rand_index
