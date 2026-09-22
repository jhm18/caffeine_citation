__precompile__(true)

module community_detection_tools

#	WHAT THIS IS. The wrapper for the paper's community dectection tools, and the
#	only entry point: test scripts and the pipeline call this module, never the
#	included source files directly.
#
#		pajek_io.jl      Pajek format in and out          (included)
#		leiden_tools.jl  Leiden community detection       (pending)
#		champ_tools.jl   CHAMP resolution selection       (pending)
#
#	include() splices each file into this module's scope, so the export statements
#	those files carry are already this module's exports. They are restated below so
#	the public surface reads in one place; a repeated export is a no-op.
#
#	Still to come, once the three components and their tests are in place: the
#	era-level orchestration, and the scale-assessment functions adapted from
#	Section 2.5 of the BEND manuscript.

#	Packages
	using DataFrames
    using LinearAlgebra
	using ProgressMeter
	using Random
	using SparseArrays

##################
#   COMPONENTS   #
##################

#	Pajek Format I/O
	include("pajek_io.jl")

#	Leiden Community Detection
	include("leiden_tools.jl")

#	CHAMP Resolution Selection
	include("champ_tools.jl")

######################
#   TEST FUNCTIONS   #
######################

#	SCALE ASSESSMENT. One CHAMP sweep per era network, run on the part of the
#	network that has room for community structure. Each era is swept on its own so
#	that its null model is computed against its own arc count; an edge-pooled graph
#	would be block diagonal and would shrink every era's effective resolution by
#	roughly the number of eras. The sweep records what the scale check and the
#	pooling rule will need. Nothing is decided here.

#	Helper Function for _weak_components: Union-Find Root with Path Halving
	function _uf_find!(parent::Vector{Int}, i::Int)
		"""
		Args:
			parent::Vector{Int}: union-find parent array, modified in place
			i::Int: vertex whose root is sought
		Returns:
			Int: the root of i's set
		Notes:
			Path halving points each visited vertex at its grandparent, which keeps
			the trees shallow without recursion.
		"""

		#	Climb to the Root
			while parent[i] != i
				parent[i] = parent[parent[i]]
				i = parent[i]
			end

		#	Assembling Result
			return i
	end

#	Helper Function for champ_era_sweep: Weakly Connected Components
	function _weak_components(n::Int, src::Vector{Int}, dst::Vector{Int})
		"""
		Args:
			n::Int: number of vertices
			src::Vector{Int}: arc sources, in 1..n
			dst::Vector{Int}: arc targets, in 1..n
		Returns:
			Tuple{Vector{Int},Vector{Int}}: (component per vertex, size per component)
		Notes:
			Direction is ignored, so these are weak components -- the sense in which a
			citation network is connected. Components are numbered in order of their
			lowest vertex index, so the numbering is reproducible from run to run.
		"""

		#	Validation
			if length(src) != length(dst)
				throw(ArgumentError("_weak_components: src and dst differ in length"))
			end
			if !isempty(src) && (min(minimum(src), minimum(dst)) < 1 ||
			                     max(maximum(src), maximum(dst)) > n)
				throw(ArgumentError("_weak_components: an arc endpoint lies outside 1..$n"))
			end

		#	Union the Endpoints of Every Arc
			parent = collect(1:n)
			@inbounds for k in eachindex(src)
				ri = _uf_find!(parent, src[k])
				rj = _uf_find!(parent, dst[k])
				if ri != rj
					parent[max(ri, rj)] = min(ri, rj)
				end
			end

		#	Number Components by First Appearance
			comp   = zeros(Int, n)
			label  = zeros(Int, n)
			n_comp = 0
			for v in 1:n
				r = _uf_find!(parent, v)
				if label[r] == 0
					n_comp += 1
					label[r] = n_comp
				end
				comp[v] = label[r]
			end

		#	Count Component Sizes
			sizes = zeros(Int, n_comp)
			for v in 1:n
				sizes[comp[v]] += 1
			end

		#	Assembling Result
			return (comp, sizes)
	end

#	Helper Function for champ_era_sweep: Full-Length Membership
	function _expand_membership(membership::Vector{Int}, kept::Vector{Int}, comp::Vector{Int})
		"""
		Args:
			membership::Vector{Int}: CHAMP labels, one per vertex in kept
			kept::Vector{Int}: the vertices CHAMP was run on, as indices into comp
			comp::Vector{Int}: weak component of every vertex in the era network
		Returns:
			Vector{Int}: one community label per vertex of the era network
		Notes:
			CHAMP labels are carried to the vertices CHAMP saw. Each component too
			small to be swept becomes a community of its own, numbered after the
			largest CHAMP label in order of component number. That is the assignment
			modularity gives so small a component at any resolution in the sweep, and
			it keeps the partition aligned with the .net file, which the R tie layer
			requires.
		"""

		#	Validation
			if length(membership) != length(kept)
				throw(ArgumentError("_expand_membership: membership and kept differ in length"))
			end
			if any(<(1), membership)
				throw(ArgumentError("_expand_membership: CHAMP labels must be positive"))
			end

		#	Carry CHAMP Labels
			n = length(comp)
			full = zeros(Int, n)
			full[kept] .= membership

		#	Label Each Dropped Component
			next_label = isempty(membership) ? 0 : maximum(membership)
			comp_label = Dict{Int,Int}()
			for v in 1:n
				if full[v] == 0
					c = comp[v]
					if !haskey(comp_label, c)
						next_label += 1
						comp_label[c] = next_label
					end
					full[v] = comp_label[c]
				end
			end

		#	Assembling Result
			return full
	end

#	Helper Function for write_champ_sweep and write_champ_summary: Tab-Separated Output
	function _write_tsv(df::DataFrame, path::AbstractString)
		"""
		Args:
			df::DataFrame: table to write
			path::AbstractString: destination file
		Returns:
			String: the path written
		Notes:
			Written by hand so the module takes on no CSV dependency. Floats are
			written at full precision; Bools as true and false, which readr reads as
			logical.
		"""

		#	Write Header and Rows
			open(path, "w") do io
				println(io, join(names(df), '\t'))
				for row in eachrow(df)
					println(io, join((string(x) for x in row), '\t'))
				end
			end

		#	Assembling Result
			return String(path)
	end

#	CHAMP Sweep for One Era Network
	function champ_era_sweep(net_file::AbstractString;
	                         min_component_size::Int = 4,
	                         resolution_range::Tuple{Float64,Float64} = (0.1, 1.8),
	                         n_resolutions::Int = 30,
	                         n_runs_per_gamma::Int = 5,
	                         n_iterations_per_run::Int = 10,
	                         seed::Union{Int,Nothing} = nothing,
	                         show_progress::Bool = true)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			min_component_size::Int: smallest weak component swept (default = 4)
			resolution_range::Tuple{Float64,Float64}: gamma range (default = (0.1, 1.8))
			n_resolutions::Int: gamma values in the sweep (default = 30)
			n_runs_per_gamma::Int: Leiden multi-starts per gamma (default = 5)
			n_iterations_per_run::Int: Leiden iterations per run (default = 10)
			seed::Union{Int,Nothing}: Leiden base seed (default = nothing)
			show_progress::Bool: CHAMP progress bar (default = true)
		Returns:
			NamedTuple: (diagnostics, sweep, membership, labels)
		Notes:
			Directed and unweighted throughout: arcs are deduplicated before CHAMP
			sees them. Weak components below min_component_size are dropped before
			the sweep and restored afterward as communities of their own.

			The kept-graph adjacency is rebuilt here, in CHAMP's node order, so the
			component-partition baseline is scored against the same null model as
			the CHAMP partition.
		"""

		#	Validation
			if min_component_size < 1
				throw(ArgumentError("champ_era_sweep: min_component_size must be ≥ 1"))
			end
			if !(0.0 < resolution_range[1] < resolution_range[2])
				throw(ArgumentError("champ_era_sweep: resolution_range must satisfy 0 < min < max"))
			end
			if n_resolutions < 2
				throw(ArgumentError("champ_era_sweep: n_resolutions must be ≥ 2"))
			end

		#	Read the Era Network
			nodes, edges = read_net(net_file)
			n = nrow(nodes)
			if nrow(edges) > 0 && !occursin(r"^\*arcs"i, edges.tie_type[1])
				throw(ArgumentError("champ_era_sweep: $net_file is not a directed (*Arcs) network"))
			end

		#	Distinct Arcs
			arcs = unique(DataFrame(src = edges.person_i, dst = edges.person_j))
			n_duplicate_arcs = nrow(edges) - nrow(arcs)
			n_self_loops = count(arcs.src .== arcs.dst)

		#	Component Selection
			comp, sizes = _weak_components(n, arcs.src, arcs.dst)
			keep_v = sizes[comp] .>= min_component_size
			kept = findall(keep_v)
			kept_arcs = arcs[keep_v[arcs.src], :]
			if isempty(kept) || nrow(kept_arcs) == 0
				throw(ArgumentError("champ_era_sweep: no component of $min_component_size or more " *
				                    "vertices carries an arc in $net_file"))
			end

		#	CHAMP Input: Pajek Indices as String Identifiers
			node_ids = string.(kept)
			champ_edges = DataFrame(src    = string.(kept_arcs.src),
			                        dst    = string.(kept_arcs.dst),
			                        weight = ones(Float64, nrow(kept_arcs)))

		#	CHAMP Sweep
			result = champ_community_detection(champ_edges;
			                                   nodes                = node_ids,
			                                   resolution_range     = resolution_range,
			                                   n_resolutions        = n_resolutions,
			                                   n_runs_per_gamma     = n_runs_per_gamma,
			                                   n_iterations_per_run = n_iterations_per_run,
			                                   weighted             = false,
			                                   directed             = true,
			                                   seed                 = seed,
			                                   show_progress        = show_progress)

		#	Node Order Check
			if result.node_names != node_ids
				throw(ErrorException("champ_era_sweep: CHAMP returned vertices in an unexpected order"))
			end

		#	Build Sparse Matrix of the Kept Graph, in CHAMP Order
			n_kept = length(kept)
			pos = zeros(Int, n)
			pos[kept] .= 1:n_kept
			adj = sparse(pos[kept_arcs.src], pos[kept_arcs.dst],
			             ones(Float64, nrow(kept_arcs)), n_kept, n_kept)
			m = sum(adj)

		#	Selected Partition Against the Component Partition
			best     = result.best_index
			γ_star   = result.resolution_used
			comp_kept = comp[kept]
			Q_star = calculate_modularity(adj, result.membership;
			                              weighted = false, directed = true, γ = γ_star)
			Q_comp = calculate_modularity(adj, comp_kept;
			                              weighted = false, directed = true, γ = γ_star)

		#	Per-Gamma Sweep Table
			sweep = DataFrame(gamma         = result.gammas,
			                  A             = result.A_coeffs,
			                  P             = result.P_coeffs,
			                  modularity    = result.modularities,
			                  n_communities = result.n_communities_per_gamma,
			                  dominant      = collect(result.dominant),
			                  selected      = [i == best for i in eachindex(result.gammas)])

		#	Era Diagnostics
			diagnostics = (
				net_file               = String(net_file),
				resolution_min         = resolution_range[1],
				resolution_max         = resolution_range[2],
				n_resolutions          = n_resolutions,
				n_runs_per_gamma       = n_runs_per_gamma,
				n_vertices             = n,
				n_arcs                 = nrow(arcs),
				n_duplicate_arcs       = n_duplicate_arcs,
				n_self_loops           = n_self_loops,
				n_components           = length(sizes),
				min_component_size     = min_component_size,
				n_kept_vertices        = n_kept,
				n_kept_arcs            = nrow(kept_arcs),
				n_kept_components      = count(>=(min_component_size), sizes),
				giant_component_share  = maximum(sizes) / n,
				m                      = m,
				gamma_star             = γ_star,
				best_index             = best,
				gamma_interior         = 1 < best < length(result.gammas),
				n_dominant             = count(result.dominant),
				n_communities_star     = result.n_communities,
				modularity_star        = Q_star,
				modularity_components  = Q_comp,
				gain_over_components   = Q_star - Q_comp,
				penalty_share_star     = γ_star * result.P_coeffs[best] / m,
				modularity_min         = minimum(result.modularities),
				modularity_max         = maximum(result.modularities),
				modularity_range       = maximum(result.modularities) - minimum(result.modularities),
				n_communities_min      = minimum(result.n_communities_per_gamma),
				n_communities_max      = maximum(result.n_communities_per_gamma),
				n_communities_range    = maximum(result.n_communities_per_gamma) -
				                         minimum(result.n_communities_per_gamma)
			)

		#	Full-Length Membership at the Era's Own Gamma
			membership = _expand_membership(result.membership, kept, comp)

		#	Assembling Result
			return (diagnostics = diagnostics,
			        sweep       = sweep,
			        membership  = membership,
			        labels      = nodes.label)
	end
	@doc raw"""
	**Description**
	Run a CHAMP resolution sweep on one era network and record what the scale
	assessment needs: the per-gamma hull coefficients, the era's own selected
	resolution, and a set of diagnostics describing whether the modularity
	envelope discriminates among partitions at all.

	**Usage**
	`champ_era_sweep(net_file; min_component_size=4, resolution_range=(0.1, 1.8), n_resolutions=30, n_runs_per_gamma=5, n_iterations_per_run=10, seed=nothing, show_progress=true)`

	**Arguments**
	- `net_file::AbstractString`: Path to a directed (`*Arcs`) Pajek network.
	- `min_component_size::Int`: Weak components smaller than this are not swept
	  (default `4`, which drops isolates, dyads, and triads).
	- `resolution_range::Tuple{Float64,Float64}`: Gamma range (default `(0.1, 1.8)`).
	- `n_resolutions::Int`: Gamma values in the grid (default `30`).
	- `n_runs_per_gamma::Int`: Leiden multi-starts per gamma (default `5`).
	- `n_iterations_per_run::Int`: Leiden iterations per run (default `10`).
	- `seed::Union{Int,Nothing}`: Leiden base seed. With `nothing`, run `r` is
	  seeded with `r - 1`, so the sweep is reproducible either way.
	- `show_progress::Bool`: Show the CHAMP progress bar (default `true`).

	**Details**
	The network is treated as directed and unweighted, and duplicate arcs are
	collapsed. Weak components are computed with direction ignored. CHAMP is run on
	the vertices of components at or above `min_component_size`; every smaller
	component is restored afterward as a community of its own, so the returned
	membership covers every vertex of the `.net` file in its vertex order.

	Each era is swept separately. Pooling the eras into one graph before sweeping
	would give a block-diagonal network in which each era's effective resolution is
	gamma scaled by its share of the arcs.

	The diagnostics that bear on scale are:
	- `giant_component_share`: largest weak component over all vertices. A
	  resolution parameter can only choose among partitions within a component.
	- `modularity_range`, `n_communities_range`: how far the sweep moves modularity
	  and community count. A near-flat envelope moves neither.
	- `gain_over_components`: modularity of the selected partition minus that of the
	  partition that labels each kept component, both at the selected gamma. Near
	  zero means CHAMP has recovered the components.
	- `penalty_share_star`: gamma times P over m at the selected partition; the
	  weight of the null-model term against the arcs themselves.
	- `gamma_interior`, `n_dominant`: whether the selection sits inside the grid,
	  and how many partitions survive dominance.

	**Value**
	A `NamedTuple`:
	- `diagnostics::NamedTuple`: one row of era diagnostics, as listed above plus
	  counts of vertices, arcs, components, and what was kept, and the grid the
	  sweep used, so a summary row can always be traced to its settings.
	- `sweep::DataFrame`: one row per gamma with `gamma`, `A`, `P`, `modularity`,
	  `n_communities`, `dominant`, `selected`. With `m` from the diagnostics, the
	  directed envelope is `max over dominant rows of (A - gamma * P) / m`.
	- `membership::Vector{Int}`: the selected partition over every vertex, in
	  `.net` vertex order.
	- `labels::Vector{String}`: the `.net` vertex labels, which carry node ids.

	**Examples**
	```julia
		#	Sweep one era
			res = champ_era_sweep("pajek_files/Era12/era12.net")
			res.diagnostics.gamma_star
			res.diagnostics.gain_over_components
	```

	**See Also**
	`write_champ_sweep`, `write_champ_summary`, `champ_community_detection`
	""" champ_era_sweep

#	Write One Era's Sweep
	function write_champ_sweep(sweep_result::NamedTuple, prefix::AbstractString;
	                           directory::AbstractString = pwd())
		"""
		Args:
			sweep_result::NamedTuple: output of champ_era_sweep
			prefix::AbstractString: file stem, e.g. "era12"
			directory::AbstractString: destination directory (default = pwd())
		Returns:
			NamedTuple: (sweep, clu), the two paths written
		Notes:
			The .clu holds the partition at the era's own gamma. It is a record of
			the sweep, not the partition the analysis will use, which is fixed only
			once a pooled gamma is chosen.
		"""

		#	Validation
			if !isdir(directory)
				throw(ArgumentError("write_champ_sweep: output directory does not exist: $directory"))
			end
			if !all(k -> haskey(sweep_result, k), (:sweep, :membership))
				throw(ArgumentError("write_champ_sweep: expected the output of champ_era_sweep"))
			end

		#	Write the Sweep Table
			sweep_path = _write_tsv(sweep_result.sweep, joinpath(directory, "$(prefix)_sweep.tsv"))

		#	Write the Partition
			clu_path = write_clu(sweep_result.membership, "$(prefix)_local_gamma"; directory = directory)

		#	Assembling Result
			return (sweep = sweep_path, clu = clu_path)
	end
	@doc raw"""
	**Description**
	Write one era's CHAMP sweep: the per-gamma table and the partition at the era's
	own selected gamma.

	**Usage**
	`write_champ_sweep(sweep_result, prefix; directory=pwd())`

	**Arguments**
	- `sweep_result::NamedTuple`: Output of `champ_era_sweep`.
	- `prefix::AbstractString`: File stem, such as `"era12"`.
	- `directory::AbstractString`: Destination directory (default `pwd()`); must exist.

	**Details**
	Writes `<prefix>_sweep.tsv`, one row per gamma, and `<prefix>_local_gamma.clu`,
	the selected partition over every vertex of the era network. The partition is
	named for what it is: the era's own choice, not the pooled-gamma partition the
	analysis will run on.

	**Value**
	A `NamedTuple` `(sweep, clu)` of the paths written.

	**Examples**
	```julia
		#	Sweep and write one era
			res = champ_era_sweep("pajek_files/Era12/era12.net")
			write_champ_sweep(res, "era12"; directory = "pajek_files/champ_sweep")
	```

	**See Also**
	`champ_era_sweep`, `write_champ_summary`, `write_clu`
	""" write_champ_sweep

#	Write the Cross-Era Summary
	function write_champ_summary(diagnostics::AbstractVector, name::AbstractString;
	                             directory::AbstractString = pwd())
		"""
		Args:
			diagnostics::AbstractVector: one NamedTuple per era, all with the same fields
			name::AbstractString: file stem, e.g. "champ_sweep_summary"
			directory::AbstractString: destination directory (default = pwd())
		Returns:
			String: the path written
		Notes:
			Rows are stacked one at a time so a vector with an abstract element type
			still produces one column per field.
		"""

		#	Validation
			if isempty(diagnostics)
				throw(ArgumentError("write_champ_summary: no era diagnostics to write"))
			end
			if !all(r -> r isa NamedTuple, diagnostics)
				throw(ArgumentError("write_champ_summary: every row must be a NamedTuple"))
			end
			if !isdir(directory)
				throw(ArgumentError("write_champ_summary: output directory does not exist: $directory"))
			end

		#	Stack the Rows
			summary = vcat([DataFrame([r]) for r in diagnostics]...)

		#	Assembling Result
			return _write_tsv(summary, joinpath(directory, "$(name).tsv"))
	end
	@doc raw"""
	**Description**
	Write the per-era diagnostics from a set of CHAMP sweeps as one table, one row
	per era.

	**Usage**
	`write_champ_summary(diagnostics, name; directory=pwd())`

	**Arguments**
	- `diagnostics::AbstractVector`: One `NamedTuple` per era, typically
	  `champ_era_sweep(...).diagnostics` with an era column merged in front.
	- `name::AbstractString`: File stem; `.tsv` is appended.
	- `directory::AbstractString`: Destination directory (default `pwd()`); must exist.

	**Value**
	The path written, as a `String`.

	**Examples**
	```julia
		#	Two eras into one table
			rows = [merge((era = e,), champ_era_sweep(p).diagnostics)
			        for (e, p) in [(12, "pajek_files/Era12/era12.net"),
			                       (23, "pajek_files/Era23/era23.net")]]
			write_champ_summary(rows, "champ_sweep_summary"; directory = "pajek_files/champ_sweep")
	```

	**See Also**
	`champ_era_sweep`, `write_champ_sweep`
	""" write_champ_summary

###############
#   EXPORTS   #
###############

#	Pajek Format I/O
	export read_net,
		   write_net,
		   read_clu,
		   write_clu,
		   read_vec,
		   write_vec,   
           calculate_modularity,
           leiden_community_detection,
           champ_community_detection,
		   adjusted_rand_index,
		   champ_era_sweep,
		   write_champ_sweep,
		   write_champ_summary

end # module community_detection_tools
