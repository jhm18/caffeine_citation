#	Test_champ_tools.jl -- Test Suite for the CHAMP Component
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHAT THIS PROTECTS. champ_community_detection and adjusted_rand_index,
#	reached through community_detection_tools.
#
#	Three groups. The contract tests assert the shape and internal consistency of
#	what CHAMP returns, including the fixed-resolution path that the pooled gamma*
#	workflow depends on. The agreement tests check adjusted_rand_index against
#	partitions whose agreement is known by construction. The corpus tests run a
#	real sweep on an era network and report the flat-hull diagnostic, which is what
#	tells us whether a resolution parameter has anything to decide on this data.
#
#	Run inside the container:
#	julia Test_champ_tools.jl
#
#	CHAMP threads its gamma sweep, so julia -t auto is materially faster than the
#	single thread the other suites have been running under.
#
#	Environment:
#	CAFFEINE_DIR   project root (default /workspace/caffeine_citation)
#	CHAMP_ERA      era to use for the corpus tests (default 12)
#	CHAMP_GAMMAS   gamma values in the corpus sweep (default 8)

#   Activating the Environment
    using Pkg
    Pkg.activate("/workspace/caffeine_citation/community_detection_tools")
    Pkg.status()

#	Packages
	using Test
	using DataFrames
    using LinearAlgebra
	using ProgressMeter
	using Random
	using SparseArrays
    using community_detection_tools

#####################
#   CONFIGURATION   #
#####################

#	Project Layout
	const PROJECT_DIR = get(ENV, "CAFFEINE_DIR", "/workspace/caffeine_citation")

#	Locate the Pajek Files
	const PAJEK_DIR = let
		candidates = [joinpath(PROJECT_DIR, "pajek_files"),
					  joinpath(@__DIR__, "..", "pajek_files"),
					  joinpath(@__DIR__, "pajek_files")]
		hit = findfirst(isdir, candidates)
		hit === nothing ? "" : normpath(candidates[hit])
	end

#	Corpus Scope
	const CORPUS_ERA    = parse(Int, get(ENV, "CHAMP_ERA", "12"))
	const CORPUS_GAMMAS = parse(Int, get(ENV, "CHAMP_GAMMAS", "8"))

#	Report What Was Found
	@info "Test_champ_tools" PAJEK_DIR CORPUS_ERA CORPUS_GAMMAS nthreads=Threads.nthreads()

###############
#   HELPERS   #
###############

#	Helper Function for the Contract Tests: Symmetric Edge List
	function undirected_edges(pairs::Vector{Tuple{Int,Int}})
		"""
		Args:
			pairs::Vector{Tuple{Int,Int}}: undirected node pairs
		Returns:
			DataFrame: edge list with :src and :dst, both directions present
		Notes:
			Both directions are written so the adjacency is symmetric before the
			module symmetrizes it, which keeps the assertions about CHAMP rather
			than about an implementation detail of the symmetrizer.
		"""

		#	Emit Both Directions
			src = String[]
			dst = String[]
			for (i, j) in pairs
				push!(src, string(i)); push!(dst, string(j))
				push!(src, string(j)); push!(dst, string(i))
			end

		#	Assemble result
			return DataFrame(src = src, dst = dst)
	end

#	Helper Function for the Contract Tests: Sparse Adjacency From an Edge List
	function edge_adjacency(edges::DataFrame, n::Int)
		"""
		Args:
			edges::DataFrame: edge list with :src and :dst as string node ids
			n::Int: number of nodes, indexed 1..n
		Returns:
			SparseMatrixCSC: adjacency, built independently of the module
		Notes:
			Used to recompute modularity on a partition CHAMP returned, so the
			reported score is checked against calculate_modularity rather than
			taken on trust.
		"""

		#	Index and Assemble
			i = parse.(Int, String.(edges.src))
			j = parse.(Int, String.(edges.dst))
			return sparse(i, j, ones(length(i)), n, n)
	end

#	Helper Function for the Contract Tests: Planted Block Structure
	function planted_blocks(block_size::Int, n_blocks::Int)
		"""
		Args:
			block_size::Int: nodes per block
			n_blocks::Int: number of blocks
		Returns:
			Tuple{DataFrame,Vector{String},Vector{Int}}: (edges, node ids, truth)
		Notes:
			Each block is a clique; consecutive blocks are joined by a single
			bridge. The planted partition is unambiguous, so failing to recover it
			is a real failure rather than a borderline one.
		"""

		#	Cliques Within Blocks
			pairs = Tuple{Int,Int}[]
			truth = Int[]
			for b in 1:n_blocks
				lo = (b - 1) * block_size + 1
				hi = b * block_size
				for i in lo:hi, j in (i + 1):hi
					push!(pairs, (i, j))
				end
				append!(truth, fill(b, block_size))
			end

		#	One Bridge Between Consecutive Blocks
			for b in 1:(n_blocks - 1)
				push!(pairs, (b * block_size, b * block_size + 1))
			end

		#	Assemble result
			nodes = [string(i) for i in 1:(block_size * n_blocks)]
			return (undirected_edges(pairs), nodes, truth)
	end

#	Helper Function for the Corpus Tests: Era File Resolution
	function resolve_era_net(pajek_dir::AbstractString, era::Int)
		"""
		Args:
			pajek_dir::AbstractString: root of the pajek_files tree
			era::Int: era number
		Returns:
			String: path to the era's .net file, or "" when absent
		Notes:
			The network sits inside Era<N>/ for most eras and at the top level for
			eras 18 to 21.
		"""

		#	Candidates, in Priority Order
			candidates = [joinpath(pajek_dir, "Era$(era)", "era$(era).net"),
						  joinpath(pajek_dir, "era$(era).net")]

		#	Assemble result
			hit = findfirst(isfile, candidates)
			return hit === nothing ? "" : candidates[hit]
	end

#############
#   TESTS   #
#############

@testset "champ_tools" begin

	#   RETURN CONTRACT
		@testset "return contract" begin

			#	An empty edge list returns the documented empty result rather
			#	than throwing partway through the sweep
				empty_res = champ_community_detection(DataFrame(src = String[], dst = String[]);
													  show_progress = false)
				@test empty_res.n_communities == 0
				@test isempty(empty_res.membership)
				@test isempty(empty_res.gammas)
				@test empty_res.best_index == 0

			#	Every documented field is present on a real result
				tri = undirected_edges([(1,2), (2,3), (1,3), (4,5), (5,6), (4,6)])
				nodes6 = [string(i) for i in 1:6]
				res = champ_community_detection(tri; nodes = nodes6,
												resolution_range = (0.1, 1.8),
												n_resolutions = 6, n_runs_per_gamma = 2,
												seed = 3, show_progress = false)
				for f in (:membership, :resolution_used, :modularity, :n_communities,
						  :node_names, :gammas, :A_coeffs, :P_coeffs, :modularities,
						  :n_communities_per_gamma, :dominant, :best_index)
					@test hasproperty(res, f)
				end

			#	The sweep is the size requested, and every per-gamma vector agrees
				k = length(res.gammas)
				@test k == 6
				@test length(res.A_coeffs) == k
				@test length(res.P_coeffs) == k
				@test length(res.modularities) == k
				@test length(res.n_communities_per_gamma) == k
				@test length(res.dominant) == k

			#	The grid spans the requested range
				@test res.gammas[1] ≈ 0.1
				@test res.gammas[end] ≈ 1.8
				@test issorted(res.gammas)

			#	The selection points at a real member of the sweep
				@test 1 <= res.best_index <= k
				@test res.gammas[res.best_index] ≈ res.resolution_used
				@test res.n_communities == res.n_communities_per_gamma[res.best_index]

			#	At least one partition survives dominance analysis
				@test any(res.dominant)

			#	Hull coefficients are finite and non-negative
				@test all(isfinite, res.A_coeffs)
				@test all(isfinite, res.P_coeffs)
				@test all(res.A_coeffs .>= 0)
				@test all(res.P_coeffs .>= 0)

			#	The node universe is honoured, in the order supplied
				@test length(res.membership) == 6
				@test String.(res.node_names) == nodes6
		end

	#   FIXED RESOLUTION
		@testset "fixed resolution" begin

			#	Supplying a resolution bypasses the sweep entirely. This is the
			#	path the pooled gamma* workflow runs on: estimate once, then apply
			#	the same gamma to every era.
				tri = undirected_edges([(1,2), (2,3), (1,3), (4,5), (5,6), (4,6)])
				nodes6 = [string(i) for i in 1:6]
				fixed = champ_community_detection(tri; nodes = nodes6, resolution = 0.5,
												  n_runs_per_gamma = 2, seed = 5,
												  show_progress = false)
				@test length(fixed.gammas) == 1
				@test fixed.gammas[1] ≈ 0.5
				@test fixed.resolution_used ≈ 0.5
				@test fixed.best_index == 1
				@test length(fixed.membership) == 6

			#	Two disjoint triangles are recovered at that resolution
				@test fixed.n_communities == 2
				@test fixed.membership[1] == fixed.membership[2] == fixed.membership[3]
				@test fixed.membership[4] == fixed.membership[5] == fixed.membership[6]
				@test fixed.membership[1] != fixed.membership[4]

			#	The reported modularity is the modularity of the returned
			#	partition at the resolution used, recomputed independently
				adj6 = edge_adjacency(tri, 6)
				@test calculate_modularity(adj6, fixed.membership;
										   weighted = false, directed = false,
										   γ = fixed.resolution_used) ≈ fixed.modularity

			#	A fixed seed is reproducible
				again = champ_community_detection(tri; nodes = nodes6, resolution = 0.5,
												  n_runs_per_gamma = 2, seed = 5,
												  show_progress = false)
				@test again.membership == fixed.membership
				@test again.resolution_used ≈ fixed.resolution_used
		end

	#   SELECTION ON STRUCTURE
		@testset "selection on structure" begin

			#	A planted block structure is recovered by the swept selection
				edges_p, nodes_p, truth = planted_blocks(8, 3)
				pl = champ_community_detection(edges_p; nodes = nodes_p,
											   resolution_range = (0.1, 1.8),
											   n_resolutions = 10, n_runs_per_gamma = 3,
											   seed = 13, show_progress = false)
				@test pl.n_communities == 3
				for b in 1:3
					members = findall(==(b), truth)
					@test length(unique(pl.membership[members])) == 1
				end

			#	The trivial single-community partition is never selected on a
			#	graph that has structure
				@test pl.n_communities > 1

			#	The reported modularity matches the returned partition
				adj_p = edge_adjacency(edges_p, length(nodes_p))
				@test calculate_modularity(adj_p, pl.membership;
										   weighted = false, directed = false,
										   γ = pl.resolution_used) ≈ pl.modularity

			#	A fixed seed is reproducible across a full sweep
				pl2 = champ_community_detection(edges_p; nodes = nodes_p,
												resolution_range = (0.1, 1.8),
												n_resolutions = 10, n_runs_per_gamma = 3,
												seed = 13, show_progress = false)
				@test pl2.membership == pl.membership
				@test pl2.resolution_used ≈ pl.resolution_used
		end

	#   PARTITION AGREEMENT
		@testset "adjusted_rand_index" begin

			#	A partition agrees perfectly with itself
				p = [1, 1, 2, 2, 3, 3]
				@test adjusted_rand_index(p, p) ≈ 1.0

			#	Relabelling is not disagreement
				@test adjusted_rand_index(p, [7, 7, 9, 9, 4, 4]) ≈ 1.0

			#	A coarsening disagrees, but not completely
				coarse = adjusted_rand_index(p, [1, 1, 1, 1, 2, 2])
				@test coarse < 1.0
				@test coarse > 0.0

			#	Two partitions that share no structure sit near zero
				@test adjusted_rand_index(p, fill(1, 6)) ≈ 0.0 atol=1e-9

			#	The measure is symmetric
				q = [1, 2, 1, 2, 3, 3]
				@test adjusted_rand_index(p, q) ≈ adjusted_rand_index(q, p)
		end

	#   CORPUS
		net_path = isempty(PAJEK_DIR) ? "" : resolve_era_net(PAJEK_DIR, CORPUS_ERA)
		if isempty(net_path)
			@info "Corpus tests skipped; era network not found" PAJEK_DIR CORPUS_ERA
		else
			@testset "corpus era $CORPUS_ERA" begin

				#	Read the network and present it as CHAMP expects. Ids must be
				#	strings: _graph_to_sparse_matrix calls String.(edges.src), and
				#	String(::Int) has no method.
					nodes, edges = read_net(net_path)
					n = nrow(nodes)
					el = DataFrame(src = string.(edges.person_i),
								   dst = string.(edges.person_j))
					universe = string.(nodes.id)

				#	Undirected and unweighted, as in the Leiden suite
					t0 = time()
					res = champ_community_detection(el; nodes = universe,
													resolution_range = (0.1, 1.8),
													n_resolutions = CORPUS_GAMMAS,
													n_runs_per_gamma = 2,
													n_iterations_per_run = 5,
													weighted = false, directed = false,
													seed = 20260908, show_progress = false)
					elapsed = time() - t0

				#	The contract holds at scale
					@test length(res.membership) == n
					@test length(res.gammas) == CORPUS_GAMMAS
					@test 1 <= res.best_index <= CORPUS_GAMMAS
					@test res.gammas[res.best_index] ≈ res.resolution_used
					@test any(res.dominant)

				#	The reported modularity matches the returned partition
					adj = edge_adjacency(el, n)
					@test calculate_modularity(adj, res.membership;
											   weighted = false, directed = false,
											   γ = res.resolution_used) ≈ res.modularity

				#	THE FLAT-HULL DIAGNOSTIC. If modularity barely moves across the
				#	whole swept range, and the community count barely moves with
				#	it, then the upper envelope is nearly flat and the elbow the
				#	selector reports is noise rather than structure. This is the
				#	BEND check in the form this dataset can use; it is reported
				#	rather than asserted, because a flat hull is a finding about
				#	the data, not a defect in the code.
					q_range = maximum(res.modularities) - minimum(res.modularities)
					c_range = maximum(res.n_communities_per_gamma) -
							  minimum(res.n_communities_per_gamma)
					p_range = maximum(res.P_coeffs) - minimum(res.P_coeffs)

				@info "corpus era $CORPUS_ERA" vertices=n arcs=nrow(edges) gamma_selected=round(res.resolution_used, digits=4) communities=res.n_communities modularity=round(res.modularity, digits=4) dominant_partitions=sum(res.dominant) modularity_range=round(q_range, digits=4) community_count_range=c_range P_range=round(p_range, digits=2) seconds=round(elapsed, digits=1)
				@info "sweep" gammas=round.(res.gammas, digits=3) communities=res.n_communities_per_gamma modularities=round.(res.modularities, digits=4)
			end
		end
end
