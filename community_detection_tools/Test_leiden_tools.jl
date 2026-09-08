#	Test_leiden_tools.jl -- Test Suite for the Leiden Component
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHAT THIS PROTECTS. calculate_modularity and leiden_community_detection,
#	reached through community_detection_tools rather than by including
#	leiden_tools.jl directly.
#
#	Three groups. The modularity tests use graphs whose Q is computable by hand,
#	so a regression shows up as a wrong number rather than as a vague drift. The
#	Leiden tests assert structural guarantees the algorithm is supposed to make --
#	connectivity, determinism under a fixed seed, node-universe handling -- rather
#	than pinning a particular partition, since multi-start optimization is free to
#	find any partition of equal quality. The corpus tests run against a real era
#	network and are skipped when pajek_files is absent.
#
#	Run inside the container:
#	julia Test_leiden_tools.jl
#
#	Environment:
#	CAFFEINE_DIR   project root (default /workspace/caffeine_citation)
#	LEIDEN_ERA     era to use for the corpus tests (default 12, the smallest)
#	LEIDEN_ALL     set to 1 to run the corpus tests over every era

#   Activating the Environment
    using Pkg
    Pkg.activate("/workspace/caffeine_citation/community_detection_tools")
    Pkg.status()

#	Packages
	using Test
	using DataFrames
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
	const CORPUS_ERAS = get(ENV, "LEIDEN_ALL", "0") == "1" ?
						collect(12:23) : [parse(Int, get(ENV, "LEIDEN_ERA", "12"))]

#	Report What Was Found
	@info "Test_leiden_tools" PAJEK_DIR CORPUS_ERAS nthreads=Threads.nthreads()

###############
#   HELPERS   #
###############

#	Helper Function for the Structural Tests: Undirected Adjacency List
	function neighbour_map(edges::DataFrame, n::Int)
		"""
		Args:
			edges::DataFrame: edge list with :src and :dst as string node ids
			n::Int: number of nodes, indexed 1..n
		Returns:
			Vector{Vector{Int}}: undirected neighbour lists
		Notes:
			Built independently of the module under test, so a connectivity
			assertion is not checked against the same code that produced the
			partition. Node ids are parsed from their string form.
		"""

		#	Accumulate Both Directions
			adj = [Int[] for _ in 1:n]
			for r in eachrow(edges)
				i = parse(Int, String(r.src))
				j = parse(Int, String(r.dst))
				if i != j
					push!(adj[i], j)
					push!(adj[j], i)
				end
			end

		#	Assemble result
			return adj
	end

#	Helper Function for the Structural Tests: Connected Components
	function component_labels(adj::Vector{Vector{Int}})
		"""
		Args:
			adj::Vector{Vector{Int}}: undirected neighbour lists
		Returns:
			Vector{Int}: component label per node, numbered from 1
		Notes:
			Breadth-first, iterative rather than recursive so an era network with
			tens of thousands of vertices does not exhaust the stack.
		"""

		#	Label by Breadth-First Search
			n = length(adj)
			label = zeros(Int, n)
			c = 0
			queue = Int[]
			for s in 1:n
				label[s] != 0 && continue
				c += 1
				label[s] = c
				empty!(queue)
				push!(queue, s)
				while !isempty(queue)
					v = popfirst!(queue)
					for w in adj[v]
						if label[w] == 0
							label[w] = c
							push!(queue, w)
						end
					end
				end
			end

		#	Assemble result
			return label
	end

#	Helper Function for the Structural Tests: Community Connectivity
	function communities_are_connected(adj::Vector{Vector{Int}}, membership::Vector{Int})
		"""
		Args:
			adj::Vector{Vector{Int}}: undirected neighbour lists
			membership::Vector{Int}: community label per node
		Returns:
			Bool: true when every community induces a connected subgraph
		Notes:
			This is the guarantee Leiden's refinement stage exists to provide, and
			the property that distinguishes it from Louvain. Isolated nodes count
			as connected. Nodes with no edges at all are skipped, since a community
			of isolates cannot be connected in any useful sense.
		"""

		#	Group Nodes by Community
			members = Dict{Int,Vector{Int}}()
			for (v, c) in enumerate(membership)
				push!(get!(members, c, Int[]), v)
			end

		#	Breadth-First Within Each Community
			for (_, vs) in members
				length(vs) <= 1 && continue
				inside = Set(vs)
				seen = Set([vs[1]])
				queue = [vs[1]]
				while !isempty(queue)
					v = popfirst!(queue)
					for w in adj[v]
						if w in inside && !(w in seen)
							push!(seen, w)
							push!(queue, w)
						end
					end
				end
				if length(seen) != length(vs)
					return false
				end
			end

		#	Assemble result
			return true
	end

#	Helper Function for the Modularity Tests: Symmetric Edge List
	function undirected_edges(pairs::Vector{Tuple{Int,Int}})
		"""
		Args:
			pairs::Vector{Tuple{Int,Int}}: undirected node pairs
		Returns:
			DataFrame: edge list with :src and :dst, both directions present
		Notes:
			Both directions are written so the adjacency matrix is symmetric before
			the module symmetrizes it. That makes the expected modularity
			independent of whether symmetrization averages or takes a maximum,
			which keeps the assertion about modularity rather than about an
			implementation detail of the symmetrizer.
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

#	Helper Function for the Leiden Tests: Planted Block Structure
	function planted_blocks(block_size::Int, n_blocks::Int)
		"""
		Args:
			block_size::Int: nodes per block
			n_blocks::Int: number of blocks
		Returns:
			Tuple{DataFrame,Vector{String},Vector{Int}}: (edges, node ids, truth)
		Notes:
			Each block is a clique; blocks are joined in a ring by one edge apiece.
			The planted partition is unambiguous at the default resolution, so a
			detector that fails to recover it has a real problem rather than a
			borderline one.
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
			The tree carries the network inside Era<N>/ for most eras and at the top
			level for eras 18 to 21.
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

@testset "leiden_tools" begin

	#   MODULARITY, HAND CHECKED
		@testset "calculate_modularity" begin

			#	Two disjoint triangles, split correctly.
			#	two_m = 12, internal = 12, K = (6, 6), expected = 72/12 = 6,
			#	so Q = (12 - 6) / 12 = 0.5 exactly.
				adj6 = sparse([1,2,3,1,2,3,4,5,6,4,5,6],
							  [2,3,1,3,1,2,5,6,4,6,4,5], ones(12), 6, 6)
				@test calculate_modularity(adj6, [1,1,1,2,2,2];
										   weighted = false, directed = false) ≈ 0.5

			#	Everything in one community is exactly zero by construction
				@test calculate_modularity(adj6, fill(1, 6);
										   weighted = false, directed = false) ≈ 0.0 atol=1e-12

			#	Community labels need not be contiguous or small
				@test calculate_modularity(adj6, [7,7,7,99,99,99];
										   weighted = false, directed = false) ≈
					  calculate_modularity(adj6, [1,1,1,2,2,2];
										   weighted = false, directed = false)

			#	Q is linear in gamma: Q(2) - Q(1) == Q(1) - Q(0)
				q0 = calculate_modularity(adj6, [1,1,1,2,2,2]; weighted = false, directed = false, γ = 0.0)
				q1 = calculate_modularity(adj6, [1,1,1,2,2,2]; weighted = false, directed = false, γ = 1.0)
				q2 = calculate_modularity(adj6, [1,1,1,2,2,2]; weighted = false, directed = false, γ = 2.0)
				@test (q2 - q1) ≈ (q1 - q0)
				@test q0 > q1 > q2

			#	A directed cycle held in one community carries no structure
				cyc = sparse([1,2,3], [2,3,1], ones(3), 3, 3)
				@test calculate_modularity(cyc, [1,1,1];
										   weighted = false, directed = true) ≈ 0.0 atol=1e-12

			#	An empty graph returns zero rather than a division by zero
				@test calculate_modularity(spzeros(4, 4), [1,1,2,2];
										   weighted = false, directed = false) == 0.0
		end

	#   LEIDEN STRUCTURE
		@testset "leiden_community_detection" begin

			#	Two disjoint triangles are recovered exactly
				tri = undirected_edges([(1,2), (2,3), (1,3), (4,5), (5,6), (4,6)])
				nodes6 = [string(i) for i in 1:6]
				res = leiden_community_detection(tri; nodes = nodes6, weighted = false,
												 directed = false, n_runs = 3, seed = 42,
												 show_progress = false)
				@test res.n_communities == 2
				@test res.membership[1] == res.membership[2] == res.membership[3]
				@test res.membership[4] == res.membership[5] == res.membership[6]
				@test res.membership[1] != res.membership[4]

			#	The node universe is honoured, in the order supplied
				@test length(res.membership) == 6
				@test String.(res.node_names) == nodes6

			#	A fixed seed is reproducible
				a = leiden_community_detection(tri; nodes = nodes6, weighted = false,
											   directed = false, n_runs = 3, seed = 7,
											   show_progress = false)
				b = leiden_community_detection(tri; nodes = nodes6, weighted = false,
											   directed = false, n_runs = 3, seed = 7,
											   show_progress = false)
				@test a.membership == b.membership
				@test a.modularity ≈ b.modularity

			#	An isolate is retained and given its own community
				nodes7 = [string(i) for i in 1:7]
				iso = leiden_community_detection(tri; nodes = nodes7, weighted = false,
												 directed = false, n_runs = 1, seed = 1,
												 show_progress = false)
				@test length(iso.membership) == 7
				@test count(==(iso.membership[7]), iso.membership) == 1

			#	weighted = true on a binary matrix is refused rather than guessed at.
			#	This is the trap for this project: the era .net files carry an
			#	explicit weight of 1 on every arc, so the default call throws.
				@test_throws ArgumentError leiden_community_detection(
					tri; nodes = nodes6, weighted = true, directed = true,
					n_runs = 1, show_progress = false)
				@test_throws ArgumentError leiden_community_detection(
					tri; nodes = nodes6, weighted = true, directed = false,
					n_runs = 1, show_progress = false)

			#	A planted block structure is recovered
				edges_p, nodes_p, truth = planted_blocks(8, 3)
				pl = leiden_community_detection(edges_p; nodes = nodes_p, weighted = false,
												directed = false, n_runs = 5, seed = 11,
												show_progress = false)
				@test pl.n_communities == 3
				for b in 1:3
					members = findall(==(b), truth)
					@test length(unique(pl.membership[members])) == 1
				end

			#	Every community induces a connected subgraph
				adj_p = neighbour_map(edges_p, length(nodes_p))
				@test communities_are_connected(adj_p, pl.membership)

			#	The detected partition beats the trivial one
				@test pl.modularity > 0.0
		end

	#   CORPUS
		if isempty(PAJEK_DIR)
			@info "Corpus tests skipped; pajek_files not found"
		else
			@testset "corpus" begin

				for era in CORPUS_ERAS
					net_path = resolve_era_net(PAJEK_DIR, era)
					if isempty(net_path)
						@warn "Era $era network not found; skipping"
						continue
					end

					@testset "era $era" begin

						#	Read the network and present it as Leiden expects.
						#	Ids must be strings: _graph_to_sparse_matrix calls
						#	String.(edges.src), and String(::Int) has no method.
							nodes, edges = read_net(net_path)
							n = nrow(nodes)
							el = DataFrame(src = string.(edges.person_i),
										   dst = string.(edges.person_j))
							universe = string.(nodes.id)

						#	Undirected and unweighted. Directed would take the
						#	O(nnz) in-edge scan per node, which is not viable at
						#	this size, and 94% of these vertices are targets with
						#	zero out-degree in any case.
							t0 = time()
							res = leiden_community_detection(el; nodes = universe,
															 weighted = false, directed = false,
															 n_runs = 2, n_iterations = 5,
															 seed = 20260908,
															 show_progress = false)
							elapsed = time() - t0

						#	Every vertex is assigned exactly once
							@test length(res.membership) == n
							@test all(res.membership .>= 1)
							@test String.(res.node_names) == universe

						#	No community spans two connected components, which no
						#	modularity optimizer should ever do
							adj = neighbour_map(el, n)
							comp = component_labels(adj)
							by_comm = Dict{Int,Set{Int}}()
							for (v, c) in enumerate(res.membership)
								push!(get!(by_comm, c, Set{Int}()), comp[v])
							end
							@test all(length(s) == 1 for s in values(by_comm))

						#	Leiden's refinement guarantee holds on real data
							@test communities_are_connected(adj, res.membership)

						#	The partition is at least as good as the trivial one
							@test res.modularity > 0.0

						#	Modularity of the COMPONENT partition, for comparison.
						#	Labelling each connected component as a community takes
						#	no community detection at all. If Leiden scores no
						#	better than that, then nothing was found beyond
						#	connectivity, and a resolution parameter has nothing to
						#	decide -- the hull is flat and any elbow CHAMP reports
						#	is noise. This is the BEND flat-hull diagnostic in the
						#	form this dataset can actually use.
							src_i  = parse.(Int, el.src)
							dst_i  = parse.(Int, el.dst)
							adj_sp = sparse(src_i, dst_i, ones(length(src_i)), n, n)
							q_comp = calculate_modularity(adj_sp, comp;
														  weighted = false, directed = false)
							@test res.modularity >= q_comp - 1e-9

						#	Community and component size profiles
							comm_sizes = Dict{Int,Int}()
							for c in res.membership
								comm_sizes[c] = get(comm_sizes, c, 0) + 1
							end
							comp_sizes = Dict{Int,Int}()
							for c in comp
								comp_sizes[c] = get(comp_sizes, c, 0) + 1
							end
							singletons = count(==(1), values(comm_sizes))
							largest    = maximum(values(comp_sizes))

						#	A fixed seed is reproducible at this scale too
							again = leiden_community_detection(el; nodes = universe,
															   weighted = false, directed = false,
															   n_runs = 2, n_iterations = 5,
															   seed = 20260908,
															   show_progress = false)
							@test again.membership == res.membership

						@info "era $era" vertices=n arcs=nrow(edges) components=maximum(comp) largest_component_pct=round(100 * largest / n, digits=1) communities=res.n_communities singleton_communities=singletons modularity=round(res.modularity, digits=4) modularity_of_components=round(q_comp, digits=4) gain_over_components=round(res.modularity - q_comp, digits=4) seconds=round(elapsed, digits=1)
					end
				end
			end
		end
end
