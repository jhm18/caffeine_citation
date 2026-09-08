#	Test_pajek_io.jl -- Test Suite for the Pajek I/O Component
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D., 8 September 2026
#
#	WHAT THIS PROTECTS. read_net, write_net, read_clu, write_clu, read_vec, and
#	write_vec, reached through community_detection_tools rather than by including
#	pajek_io.jl directly.
#
#	Two groups. The synthetic tests build their own files in a temporary directory
#	and need no project data, so they run anywhere and cover the round trips, the
#	parser edge cases, and every error path. The corpus tests run against the era
#	networks and are skipped when pajek_files is absent.
#
#	Run inside the container:
#	julia tests/Test_pajek_io.jl
#
#	Or against a checkout elsewhere:
#	CAFFEINE_DIR=/path/to/caffeine_citation julia tests/Test_pajek_io.jl

#   Activating the Environment
    using Pkg
    Pkg.activate("/workspace/caffeine_citation/community_detection_tools")
    Pkg.status()

#	Packages
	using Test
	using DataFrames
	using community_detection_tools

#####################
#   CONFIGURATION   #
#####################

#	Project Layout
	const PROJECT_DIR = get(ENV, "CAFFEINE_DIR", "/workspace/caffeine_citation")
	const PAJEK_DIR   = joinpath(PROJECT_DIR, "pajek_files")

#	Integration Window
	const ERAS = 12:23

###############
#   HELPERS   #
###############

#	Helper Function for the Corpus Tests: Era File Resolution
	function resolve_era(pajek_dir::AbstractString, era::Int)
		"""
		Args:
			pajek_dir::AbstractString: root of the pajek_files tree
			era::Int: era number
		Returns:
			NamedTuple: (net, clu), each a path or "" when absent
		Notes:
			The tree carries three layouts. Eras 1 to 17 hold era<N>.net inside
			Era<N>/ with era<N>_Community.clu at the top level; eras 18 to 21 hold
			both at the top level; eras 22 and 23 hold both inside Era<N>/ and use
			the _testCommunity naming. Candidates are tried in a fixed order so a
			run is reproducible.
		"""

		#	Candidate Networks, in Priority Order
			net_candidates = [joinpath(pajek_dir, "Era$(era)", "era$(era).net"),
							  joinpath(pajek_dir, "era$(era).net")]

		#	Candidate Partitions, in Priority Order
			clu_candidates = [joinpath(pajek_dir, "Era$(era)", "era$(era)_testCommunity.clu"),
							  joinpath(pajek_dir, "Era$(era)", "era$(era)_Community.clu"),
							  joinpath(pajek_dir, "era$(era)_testCommunity.clu"),
							  joinpath(pajek_dir, "era$(era)_Community.clu")]

		#	Select the First That Exists
			net_hit = findfirst(isfile, net_candidates)
			clu_hit = findfirst(isfile, clu_candidates)

		#	Assemble result
			return (net = net_hit === nothing ? "" : net_candidates[net_hit],
					clu = clu_hit === nothing ? "" : clu_candidates[clu_hit])
	end

#	Helper Function for the Synthetic Tests: Write a Literal File
	function write_lines(path::AbstractString, lines::Vector{String})
		"""
		Args:
			path::AbstractString: file to write
			lines::Vector{String}: lines, written with a trailing newline each
		Returns:
			String: the path written
		Notes:
			Used to build malformed files by hand, so the readers can be tested
			against inputs the writers would never produce.
		"""

		#	Write
			open(path, "w") do f
				for l in lines
					write(f, l, "\n")
				end
			end

		#	Assemble result
			return path
	end

#############
#   TESTS   #
#############

@testset "pajek_io" begin

	#	Create Test Directory
		scratch = mktempdir()

	#   ROUND TRIP
		@testset "round trips" begin

			#	Network
				nodes = DataFrame(id = 1:4, label = ["10", "20", "30", "40"])
				src   = [1, 1, 2, 3]
				dst   = [2, 3, 4, 4]
				path  = write_net("arcs", nodes, src, dst, "rt_net"; directory = scratch)
				@test isfile(path)
				back_nodes, back_edges = read_net(path)
				@test back_nodes.id    == nodes.id
				@test back_nodes.label == nodes.label
				@test back_edges.person_i == src
				@test back_edges.person_j == dst
				@test all(back_edges.tie_type .== "*Arcs")

			#	Weights survive by value. write_net defaults to 1.0, so an unweighted
			#	network round trips as Float64 rather than Int. Assert the value, and
			#	note the asymmetry rather than papering over it.
				@test all(back_edges.weight .== 1)

			#	Explicit weights
				wpath = write_net("edges", nodes, src, dst, "rt_wnet";
								directory = scratch, tie_weight = [2.5, 1.0, 3.0, 4.25])
				_, wedges = read_net(wpath)
				@test wedges.weight == [2.5, 1.0, 3.0, 4.25]
				@test all(wedges.tie_type .== "*Edges")

			#	Partition
				part  = [1, 1, 2, 3, 2]
				cpath = write_clu(part, "rt_clu"; directory = scratch)
				@test read_clu(cpath) == part
				@test eltype(read_clu(cpath)) <: Integer

			#	Integer vector
				ivec  = [0, 3, 7, 2]
				ipath = write_vec(ivec, "rt_ivec"; directory = scratch)
				@test read_vec(ipath) == ivec
				@test eltype(read_vec(ipath)) <: Integer

			#	Float vector
				fvec  = [0.5, 3.25, 7.0, 2.125]
				fpath = write_vec(fvec, "rt_fvec"; directory = scratch)
				@test read_vec(fpath) == fvec
				@test eltype(read_vec(fpath)) <: AbstractFloat
		end

	#   OUTPUT PATHS
		@testset "output paths" begin

			#	The extension is appended only when absent
				a = write_clu([1, 2], "ext_a"; directory = scratch)
				b = write_clu([1, 2], "ext_b.clu"; directory = scratch)
				@test endswith(a, "ext_a.clu")
				@test endswith(b, "ext_b.clu")
				@test !endswith(b, ".clu.clu")

			#	Files land in the directory given, not the working directory
				@test dirname(a) == scratch

			#	An absolute name overrides the directory
				nested = mkpath(joinpath(scratch, "nested"))
				abs_target = joinpath(nested, "absolute.clu")
				@test write_clu([1, 2], abs_target; directory = scratch) == abs_target
				@test isfile(abs_target)

			#	A missing directory is an error, not a silent write elsewhere
				@test_throws ArgumentError write_clu([1, 2], "nope";
													directory = joinpath(scratch, "absent"))
		end

	#   PARSER CASES
		@testset "parser edge cases" begin

			#	Arcs without a weight take unit weight
				p = write_lines(joinpath(scratch, "noweight.net"),
								["*Vertices 3",
								"1 \"a\"", "2 \"b\"", "3 \"c\"",
								"*Arcs", "1 2", "2 3"])
				_, e = read_net(p)
				@test e.person_i == [1, 2]
				@test all(e.weight .== 1)

			#	Display attributes on vertex and arc lines are ignored
				p = write_lines(joinpath(scratch, "display.net"),
								["*Vertices 2",
								"     1 \"8112\"    0.0000    0.0000    0.5000 ic Blue bc White",
								"     2 \"8113\"    0.0000    0.0000    0.5000",
								"*Arcs", "1 2 1 c Gray"])
				n, e = read_net(p)
				@test n.label == ["8112", "8113"]
				@test e.weight == [1]

			#	A trailing block is not folded into the edge list
				p = write_lines(joinpath(scratch, "trailing.net"),
								["*Vertices 3",
								"1 \"a\"", "2 \"b\"", "3 \"c\"",
								"*Arcs", "1 2 1", "2 3 1",
								"*Partition", "1", "1", "2"])
				_, e = read_net(p)
				@test nrow(e) == 2

			#	Blank lines are tolerated
				p = write_lines(joinpath(scratch, "blanks.clu"),
								["*Vertices 3", "1", "2", "1", ""])
				@test read_clu(p) == [1, 2, 1]
		end

	#   ERROR PATHS
		@testset "error paths" begin

			#	Missing files
				@test_throws ArgumentError read_net(joinpath(scratch, "absent.net"))
				@test_throws ArgumentError read_clu(joinpath(scratch, "absent.clu"))
				@test_throws ArgumentError read_vec(joinpath(scratch, "absent.vec"))

			#	A declared vertex count that disagrees with the vertex lines
				p = write_lines(joinpath(scratch, "badcount.net"),
								["*Vertices 5", "1 \"a\"", "2 \"b\"", "*Arcs", "1 2 1"])
				@test_throws ArgumentError read_net(p)

			#	A declared vertex count that disagrees with the assignments
				p = write_lines(joinpath(scratch, "badcount.clu"), ["*Vertices 5", "1", "2"])
				@test_throws ArgumentError read_clu(p)

			#	A file carrying both arcs and edges
				p = write_lines(joinpath(scratch, "mixed.net"),
								["*Vertices 2", "1 \"a\"", "2 \"b\"",
								"*Arcs", "1 2 1", "*Edges", "2 1 1"])
				@test_throws ArgumentError read_net(p)

			#	A network with no tie section
				p = write_lines(joinpath(scratch, "notie.net"),
								["*Vertices 2", "1 \"a\"", "2 \"b\""])
				@test_throws ArgumentError read_net(p)

			#	An unquoted vertex label cannot be told from a coordinate
				p = write_lines(joinpath(scratch, "unquoted.net"),
								["*Vertices 2", "1 a", "2 b", "*Arcs", "1 2 1"])
				@test_throws ArgumentError read_net(p)

			#	Empty inputs to the writers
				@test_throws ArgumentError write_clu(Int[], "empty"; directory = scratch)
				@test_throws ArgumentError write_vec(Float64[], "empty"; directory = scratch)
		end

	#   CORPUS
		if !isdir(PAJEK_DIR)
			@info "Corpus tests skipped; pajek_files not found" PAJEK_DIR
		else
			@testset "corpus" begin

				for era in ERAS
					paths = resolve_era(PAJEK_DIR, era)
					if isempty(paths.net) || isempty(paths.clu)
						@warn "Era $era did not resolve; skipping" paths
						continue
					end

					@testset "era $era" begin

						#	The network parses
							nodes, edges = read_net(paths.net)
							@test nrow(nodes) > 0
							@test nrow(edges) > 0

						#	Vertex indices run 1..N in order
							@test nodes.id == collect(1:nrow(nodes))

						#	Labels carry the citation node id in this project
							@test all(l -> tryparse(Int, l) !== nothing, nodes.label)

						#	Every arc endpoint is a vertex
							@test minimum(min.(edges.person_i, edges.person_j)) >= 1
							@test maximum(max.(edges.person_i, edges.person_j)) <= nrow(nodes)

						#	These era networks are directed and unweighted
							@test all(edges.tie_type .== "*Arcs")
							@test all(edges.weight .== 1)

						#	The partition covers the vertex set exactly
							partition = read_clu(paths.clu)
							@test length(partition) == nrow(nodes)
							@test eltype(partition) <: Integer
							@test minimum(partition) >= 1

						#	A partition survives a write and read unchanged
							out = write_clu(partition, "era$(era)_roundtrip"; directory = scratch)
							@test read_clu(out) == partition
					end
				end
			end
		end

		rm(scratch; recursive = true, force = true)
end
