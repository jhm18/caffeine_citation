__precompile__(true)

module community_detection_tools

#	WHAT THIS IS. The wrapper for the paper's community detection tools, and the
#	only entry point: test scripts and the pipeline call this module, never the
#	included source files directly.
#
#		pajek_io.jl      Pajek format in and out
#		leiden_tools.jl  Leiden community detection
#		champ_tools.jl   CHAMP sweeps and partition agreement
#
#	include() splices each file into this module's scope, so the export statements
#	those files carry are already this module's exports. They are restated below so
#	the public surface reads in one place; a repeated export is a no-op.
#
#	WHAT THE ERA-LEVEL FUNCTIONS DO, under the granularity design
#	(granularity_design.md, fourth draft, sec. 9):
#
#		profile_era_network            structure of one era, no detection
#		champ_era_sweep                the CHAMP sweep, now carrying citer-weighted
#		                               granularity at every gamma
#		community_granularity          citer-weighted k_eff and g from a membership
#		                               vector and the within-era out-degrees
#		granularity_probe              the sec. 9.1 validation: monotonicity,
#		                               run-to-run variation, and AMI across seeds
#		calibrate_resolution           inverts a gamma -> k_eff curve to a target
#		write_partition_at_resolution  the partition at a declared gamma, tagged by
#		                               specification, with its gamma on disk
#
#	The admissible set, the elbow and gamma_star are still computed and written,
#	because the envelope diagnostics of the design's sec. 3 are reported; nothing
#	downstream of the sweep uses them.

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

#	CHAMP Sweeps and Partition Agreement
	include("champ_tools.jl")

###########################
#   NETWORK PREPARATION   #
###########################

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

#	Helper Function for _prepare_era_network: Weakly Connected Components
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

#	Helper Function for champ_era_sweep and Friends: Full-Length Membership
	function _expand_membership(membership::Vector{Int}, kept::Vector{Int}, comp::Vector{Int})
		"""
		Args:
			membership::Vector{Int}: detected labels, one per vertex in kept
			kept::Vector{Int}: the vertices detection was run on, as indices into comp
			comp::Vector{Int}: weak component of every vertex in the era network
		Returns:
			Vector{Int}: one community label per vertex of the era network
		Notes:
			Detected labels are carried to the vertices detection saw. Each component
			too small to be swept becomes a community of its own, numbered after the
			largest detected label in order of component number. That is the
			assignment modularity gives so small a component at any resolution in
			the sweep, and it keeps the partition aligned with the .net file, which
			the R tie layer requires.

			Because the restored components take the labels above
			maximum(membership), the detected count and the restored count are
			recoverable from the full vector: labels 1..n_detected were detected,
			the rest restored. community_granularity reports the two separately,
			which the design requires (sec. 5.1): a .clu count and a sweep count
			differ by exactly the restored components.
		"""

		#	Validation
			if length(membership) != length(kept)
				throw(ArgumentError("_expand_membership: membership and kept differ in length"))
			end
			if any(<(1), membership)
				throw(ArgumentError("_expand_membership: detected labels must be positive"))
			end

		#	Carry Detected Labels
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

#	Helper Function for champ_era_sweep, granularity_probe and write_partition_at_resolution: Read and Reduce One Era
	function _prepare_era_network(net_file::AbstractString, min_component_size::Int)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			min_component_size::Int: smallest weak component detection keeps
		Returns:
			NamedTuple: everything the sweep, the probe and the partition writer share
		Notes:
			One reading of the network serves all three, so they cannot disagree
			about which arcs were deduplicated, which components were dropped, or
			which vertices count as citing. outdeg is over the full vertex set, so a
			vertex in a dropped dyad still counts as a citing article: the
			granularity population is every article that cites inside the era
			(design doc, sec. 5.4), not only those in swept components.
		"""

		#	Validation
			if min_component_size < 1
				throw(ArgumentError("_prepare_era_network: min_component_size must be ≥ 1"))
			end

		#	Read the Era Network
			nodes, edges = read_net(net_file)
			n = nrow(nodes)
			if nrow(edges) > 0 && !occursin(r"^\*arcs"i, edges.tie_type[1])
				throw(ArgumentError("_prepare_era_network: $net_file is not a directed (*Arcs) network"))
			end

		#	Distinct Arcs
			arcs = unique(DataFrame(src = edges.person_i, dst = edges.person_j))
			n_duplicate_arcs = nrow(edges) - nrow(arcs)
			n_self_loops = count(arcs.src .== arcs.dst)

		#	Degrees Over the Full Vertex Set
			outdeg = zeros(Int, n)
			indeg  = zeros(Int, n)
			@inbounds for k in 1:nrow(arcs)
				outdeg[arcs.src[k]] += 1
				indeg[arcs.dst[k]]  += 1
			end

		#	Component Selection
			comp, sizes = _weak_components(n, arcs.src, arcs.dst)
			keep_v = sizes[comp] .>= min_component_size
			kept = findall(keep_v)
			kept_arcs = arcs[keep_v[arcs.src], :]

		#	Kept Graph in Kept Order
			n_kept = length(kept)
			pos = zeros(Int, n)
			pos[kept] .= 1:n_kept
			adj = sparse(pos[kept_arcs.src], pos[kept_arcs.dst],
			             ones(Float64, nrow(kept_arcs)), n_kept, n_kept)

		#	Detection Input: Pajek Indices as String Identifiers
			node_ids = string.(kept)
			champ_edges = DataFrame(src    = string.(kept_arcs.src),
			                        dst    = string.(kept_arcs.dst),
			                        weight = ones(Float64, nrow(kept_arcs)))

		#	Assembling Result
			return (net_file         = String(net_file),
			        labels           = nodes.label,
			        n                = n,
			        arcs             = arcs,
			        n_duplicate_arcs = n_duplicate_arcs,
			        n_self_loops     = n_self_loops,
			        outdeg           = outdeg,
			        indeg            = indeg,
			        comp             = comp,
			        sizes            = sizes,
			        kept             = kept,
			        kept_arcs        = kept_arcs,
			        adj              = adj,
			        m                = Float64(nrow(kept_arcs)),
			        node_ids         = node_ids,
			        champ_edges      = champ_edges,
			        n_restored       = count(<(min_component_size), sizes),
			        n_citing         = count(>(0), outdeg))
	end

#	Helper Function for the Sweep and the Probe: Largest gamma * K_out_c / m
	function _max_penalty_share(membership::Vector{Int}, outdeg::Vector{Int}, m::Float64, γ::Float64)
		"""
		Args:
			membership::Vector{Int}: labels over the full vertex set
			outdeg::Vector{Int}: within-era out-degree over the full vertex set
			m::Float64: arcs in the kept graph
			γ::Float64: the resolution at which the partition was found
		Returns:
			Float64: max over communities of gamma * K_out_c / m
		Notes:
			The design doc's sec. 2 argument -- that a cited-only vertex of in-degree
			one sits with its citer at the optimum -- holds only where this quantity
			is below 1. It is not automatic at gamma up to 6, so it is measured at
			every gamma rather than assumed.
		"""

		#	Community Out-Degree
			K_out = Dict{Int,Int}()
			@inbounds for v in eachindex(membership)
				outdeg[v] == 0 && continue
				K_out[membership[v]] = get(K_out, membership[v], 0) + outdeg[v]
			end

		#	Assembling Result
			return (isempty(K_out) || m == 0.0) ? 0.0 : γ * maximum(values(K_out)) / m
	end

#	Helper Function for _write_tsv and _read_tsv: Typed Column Parsing
	function _parse_column(values::Vector{String})
		"""
		Args:
			values::Vector{String}: one column of a TSV, as text
		Returns:
			Vector: Int, Float64, Bool or String, whichever parses every cell
		Notes:
			Tried in that order. NaN and empty cells are accepted in a Float64
			column, so a dominance bound of NaN written by the sweep reads back as
			one.
		"""

		#	Try Int
			ints = tryparse.(Int, values)
			if all(!isnothing, ints)
				return Vector{Int}(ints)
			end

		#	Try Float64
			floats = map(values) do s
				t = strip(s)
				isempty(t) ? NaN : tryparse(Float64, t)
			end
			if all(!isnothing, floats)
				return Vector{Float64}(floats)
			end

		#	Try Bool
			if all(v -> v in ("true", "false"), values)
				return values .== "true"
			end

		#	Fall Back to String
			return values
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

#	Helper Function for read_champ_sweep: Tab-Separated Input
	function _read_tsv(path::AbstractString)
		"""
		Args:
			path::AbstractString: a file written by _write_tsv
		Returns:
			DataFrame: with each column typed by _parse_column
		Notes:
			The inverse of _write_tsv, for reading a sweep back into a calibration
			run without a CSV dependency.
		"""

		#	Read Lines
			lines = filter(!isempty, readlines(path))
			if isempty(lines)
				throw(ArgumentError("_read_tsv: $path is empty"))
			end
			header = split(lines[1], '\t')
			cells  = [split(l, '\t') for l in lines[2:end]]
			if any(c -> length(c) != length(header), cells)
				throw(ArgumentError("_read_tsv: a row of $path has the wrong number of fields"))
			end

		#	Assemble Columns
			df = DataFrame()
			for (j, name) in enumerate(header)
				df[!, name] = _parse_column([String(c[j]) for c in cells])
			end

		#	Assembling Result
			return df
	end

###############################
#   GRANULARITY MEASUREMENT   #
###############################

#	Citer-Weighted Granularity of One Partition
	function community_granularity(membership::Vector{Int}, outdeg::Vector{Int};
	                               n_detected::Union{Int,Nothing} = nothing)
		"""
		Args:
			membership::Vector{Int}: community label per vertex, over the full .net order
			outdeg::Vector{Int}: within-era out-degree per vertex, same order
			n_detected::Union{Int,Nothing}: labels 1..n_detected were detected, the rest
				restored (default = nothing, in which case the split is not reported)
		Returns:
			NamedTuple: k_eff, g, and the size-distribution summaries of design sec. 9.2
		Notes:
			A vertex is a citing article when outdeg > 0. With w_c the number of
			citing articles in community c and W their total,

				k_eff = W^2 / sum_c w_c^2        g = W / k_eff = sum_c w_c^2 / W

			g is the number of citing articles in the community of a randomly chosen
			citing article; k_eff is the inverse-Simpson effective count. Both are
			measured on citing articles only (design doc, sec. 5.2 and 5.4). The
			cited-only vertices stay in the partition, where they shaped the
			boundaries, and contribute nothing here.

			The size summaries are over communities holding at least one citing
			article: the counts above 1, 3, 5 and 10, the largest community's share
			of W, a Gini over those sizes, and two singleton shares -- the share of
			such communities that hold exactly one citer, and the share of citers
			who sit alone. Communities holding no citing article (a restored
			component of cited-only works cannot exist, but a detected one can, in
			principle) are counted in n_communities and nowhere else.
		"""

		#	Validation
			if length(membership) != length(outdeg)
				throw(ArgumentError("community_granularity: membership and outdeg differ in length"))
			end

		#	Citing Articles per Community
			w = Dict{Int,Int}()
			all_labels = Set{Int}()
			@inbounds for v in eachindex(membership)
				push!(all_labels, membership[v])
				if outdeg[v] > 0
					w[membership[v]] = get(w, membership[v], 0) + 1
				end
			end
			W = sum(values(w); init = 0)
			sizes = sort!(collect(values(w)))

		#	Effective Count and Granularity
			sumsq = sum(x -> x^2, sizes; init = 0)
			k_eff = W == 0 ? 0.0 : W^2 / sumsq
			g     = W == 0 ? 0.0 : sumsq / W

		#	Size-Distribution Summaries
			n_ge1  = length(sizes)
			n_ge3  = count(>=(3),  sizes)
			n_ge5  = count(>=(5),  sizes)
			n_ge10 = count(>=(10), sizes)
			n_w1   = count(==(1),  sizes)
			largest_share = W == 0 ? 0.0 : maximum(sizes) / W
			gini = if n_ge1 < 2 || W == 0
				0.0
			else
				n_s = length(sizes)
				sum(((2i - n_s - 1) * sizes[i]) for i in 1:n_s) / (n_s * W)
			end
			singleton_community_share = n_ge1 == 0 ? 0.0 : n_w1 / n_ge1
			singleton_article_share   = W == 0     ? 0.0 : n_w1 / W

		#	Detected and Restored Counts
			n_communities = length(all_labels)
			n_det = isnothing(n_detected) ? n_communities : n_detected
			n_res = isnothing(n_detected) ? 0 : n_communities - n_detected

		#	Assembling Result
			return (W                         = W,
			        k_eff_citer               = k_eff,
			        g_citer                   = g,
			        n_communities             = n_communities,
			        n_detected                = n_det,
			        n_restored                = n_res,
			        n_ge1                     = n_ge1,
			        n_ge3                     = n_ge3,
			        n_ge5                     = n_ge5,
			        n_ge10                    = n_ge10,
			        largest_share_citer       = largest_share,
			        size_gini_citer           = gini,
			        singleton_community_share = singleton_community_share,
			        singleton_article_share   = singleton_article_share)
	end
	@doc raw"""
	**Description**
	Citer-weighted granularity of one partition: the effective number of
	communities `k_eff` and the granularity `g`, measured on the articles that
	cite inside the era, together with the size-distribution summaries the
	granularity design asks for (sec. 9.2).

	**Usage**
	`community_granularity(membership, outdeg; n_detected=nothing)`

	**Arguments**
	- `membership::Vector{Int}`: Community label per vertex, in `.net` vertex order.
	- `outdeg::Vector{Int}`: Within-era out-degree per vertex, same order. A vertex with `outdeg > 0` is a citing article.
	- `n_detected::Int|nothing`: Labels `1..n_detected` were detected and the rest restored from dropped components, as `_expand_membership` numbers them. When given, the detected and restored counts are reported separately.

	**Details**
	With `w_c` the number of citing articles in community `c` and `W = Σ w_c`,

	`k_eff = W² / Σ w_c²` and `g = W / k_eff = Σ w_c² / W`.

	`g` is the number of citing articles in the community of a randomly chosen
	citing article; `k_eff` is the inverse-Simpson (Hill order 2) effective count.
	Both are measured on citing articles only, because granularity that counted
	cited-only works would depend on how many citation targets each community
	happens to accumulate (design doc, sec. 5.4). `k_eff` is a diversity
	equivalent, not a number of analytical units; the counts of communities above
	a size floor are the diagnostic for that (sec. 5.3).

	**Value**
	A `NamedTuple` with `W`, `k_eff_citer`, `g_citer`, `n_communities`,
	`n_detected`, `n_restored`, `n_ge1`, `n_ge3`, `n_ge5`, `n_ge10`,
	`largest_share_citer`, `size_gini_citer`, `singleton_community_share` (the
	share of citer-bearing communities with one citer) and
	`singleton_article_share` (the share of citers who sit alone).

	**Examples**
	```julia
		#	Granularity of a written partition
			prep = community_detection_tools._prepare_era_network("era17.net", 4)
			clu  = read_clu("era17_alpha1.00_g120_e14.clu")
			community_granularity(clu, prep.outdeg)
	```

	**See Also**
	`champ_era_sweep`, `granularity_probe`, `calibrate_resolution`
	""" community_granularity

#################
#   PROFILING   #
#################

#	Profile One Era Network Without Sweeping It
	function profile_era_network(net_file::AbstractString; min_component_size::Int = 4)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			min_component_size::Int: smallest weak component a sweep would keep (default = 4)
		Returns:
			NamedTuple: one row of structural counts, no community detection
		Notes:
			The viability screen needs what a file header cannot supply. Vertex and
			arc counts say nothing about whether a resolution parameter has anything
			to choose among, because gamma can only separate partitions WITHIN a
			connected component: a network of 3,001 vertices in 260 fragments clears
			any arc threshold while offering nothing to detect.

			Everything here is linear in the arcs, so profiling every era costs
			seconds and can be run before committing to a sweep that costs an hour.

			n_citing is W, the size of the granularity population, and is what the
			calibration scales targets by. n_cited_only_multi counts cited-only
			vertices with in-degree two or more: the only cited-only vertices that
			can join citers who would otherwise be apart (design doc, sec. 5.4),
			and so the measurable part of the construct-validity question.
		"""

		#	Read and Reduce
			prep = _prepare_era_network(net_file, min_component_size)
			n = prep.n
			arcs = prep.arcs

		#	Early return
			if nrow(arcs) == 0
				return (net_file = prep.net_file, n_vertices = n, n_arcs = 0,
						n_duplicate_arcs = prep.n_duplicate_arcs, n_self_loops = 0,
						n_components = n, giant_component_size = (n == 0 ? 0 : 1),
						giant_component_share = (n == 0 ? 0.0 : 1.0 / n),
						n_kept_vertices = 0, n_kept_arcs = 0, n_kept_components = 0,
						n_restored_components = 0,
						n_citing = 0, n_cited = 0, n_both_degree = 0,
						n_cited_only_multi = 0, n_arcs_two_step = 0,
						mean_in_degree_cited_only = 0.0, mean_degree = 0.0)
			end

		#	Depth and Composition
			outdeg, indeg = prep.outdeg, prep.indeg
			both = count(i -> outdeg[i] > 0 && indeg[i] > 0, 1:n)
			cited_only = findall(i -> outdeg[i] == 0 && indeg[i] > 0, 1:n)
			cited_only_multi = count(i -> indeg[i] >= 2, cited_only)
			mean_in_cited_only = isempty(cited_only) ? 0.0 :
				sum(indeg[i] for i in cited_only) / length(cited_only)

		#	Arcs Whose Target Also Cites
		#	A network with none of these has depth one: every arc runs from a vertex
		#	that only cites to a vertex that is only cited, and no path of length two
		#	exists anywhere in it.
			two_step = count(k -> outdeg[arcs.dst[k]] > 0, 1:nrow(arcs))

		#	Assembling Result
			sizes = prep.sizes
			return (net_file                  = prep.net_file,
					n_vertices                = n,
					n_arcs                    = nrow(arcs),
					n_duplicate_arcs          = prep.n_duplicate_arcs,
					n_self_loops              = prep.n_self_loops,
					n_components              = length(sizes),
					giant_component_size      = maximum(sizes),
					giant_component_share     = maximum(sizes) / n,
					n_kept_vertices           = length(prep.kept),
					n_kept_arcs               = nrow(prep.kept_arcs),
					n_kept_components         = count(>=(min_component_size), sizes),
					n_restored_components     = prep.n_restored,
					n_citing                  = prep.n_citing,
					n_cited                   = count(>(0), indeg),
					n_both_degree             = both,
					n_cited_only_multi        = cited_only_multi,
					n_arcs_two_step           = two_step,
					mean_in_degree_cited_only = mean_in_cited_only,
					mean_degree               = 2 * nrow(arcs) / n)
	end
	@doc raw"""
	**Description**
	Report the structure of one era network without running community detection:
	component profile, giant-component share, the citing and cited counts, the
	depth measures, and the composition figures the granularity design records.

	**Usage**
	`profile_era_network(net_file; min_component_size=4)`

	**Arguments**
	- `net_file::AbstractString`: Path to a directed (`*Arcs`) Pajek network.
	- `min_component_size::Int`: Smallest weak component a sweep would keep
	  (default `4`), used to report what a sweep would retain and restore.

	**Details**
	Built for the viability screen. A `.net` header gives vertices and arcs, and
	neither tells you whether a resolution parameter has anything to decide: gamma
	separates partitions only within a connected component, so a large network in
	many fragments offers nothing to detect. `giant_component_share` is the quantity
	to screen on, and it costs one pass over the arcs rather than an hour of sweeping.

	Three fields serve the granularity design. `n_citing` is `W`, the population
	granularity is measured on and the quantity the calibration targets scale with.
	`n_restored_components` is the number of communities a `.clu` file holds beyond
	the detected count, so the two counts the design distinguishes (sec. 5.1) can be
	reconciled. `n_cited_only_multi` counts cited-only vertices with in-degree two or
	more, the only ones that can join citers who would otherwise be apart, with
	`mean_in_degree_cited_only` beside it so the arc total can be checked against
	the composition (the design's Appendix B open check).

	**Value**
	A `NamedTuple` carrying `n_vertices`, `n_arcs`, `n_duplicate_arcs`,
	`n_self_loops`, `n_components`, `giant_component_size`,
	`giant_component_share`, `n_kept_vertices`, `n_kept_arcs`,
	`n_kept_components`, `n_restored_components`, `n_citing`, `n_cited`,
	`n_both_degree`, `n_cited_only_multi`, `n_arcs_two_step`,
	`mean_in_degree_cited_only` and `mean_degree`.

	**Examples**
	```julia
		#	Screen one era before committing to a sweep
			p = profile_era_network("pajek_files/eras_7yr/era17.net")
			p.giant_component_share
			p.n_citing
	```

	**See Also**
	`champ_era_sweep`, `write_champ_summary`
	""" profile_era_network

#############
#   SWEEP   #
#############

#	CHAMP Sweep for One Era Network
	function champ_era_sweep(net_file::AbstractString;
	                         min_component_size::Int = 4,
	                         resolution_range::Tuple{Float64,Float64} = (0.1, 6.0),
	                         n_resolutions::Int = 60,
	                         n_runs_per_gamma::Int = 5,
	                         n_iterations_per_run::Int = 10,
	                         seed::Union{Int,Nothing} = nothing,
	                         show_progress::Bool = true)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			min_component_size::Int: smallest weak component swept (default = 4)
			resolution_range::Tuple{Float64,Float64}: gamma range (default = (0.1, 6.0))
			n_resolutions::Int: gamma values in the sweep (default = 60)
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

			The sweep table now carries, at every gamma, the citer-weighted
			granularity measures of community_granularity, the m/P proxy, the
			admissible-set interval, and the largest gamma * K_out_c / m. The
			gamma -> k_eff_citer column is the calibration curve the design inverts
			(sec. 6.1); the proxy column is kept beside it so the proxy-to-measured
			correction of sec. 5.5 is read from the same table.

			The .clu written by write_champ_sweep is the partition at the CHAMP
			elbow, kept as an envelope diagnostic; the partitions the analysis
			uses come from write_partition_at_resolution.
		"""

		#	Validation
			if !(0.0 < resolution_range[1] < resolution_range[2])
				throw(ArgumentError("champ_era_sweep: resolution_range must satisfy 0 < min < max"))
			end
			if n_resolutions < 2
				throw(ArgumentError("champ_era_sweep: n_resolutions must be ≥ 2"))
			end

		#	Read and Reduce
			prep = _prepare_era_network(net_file, min_component_size)
			if isempty(prep.kept) || nrow(prep.kept_arcs) == 0
				throw(ArgumentError("champ_era_sweep: no component of $min_component_size or more " *
				                    "vertices carries an arc in $net_file"))
			end

		#	CHAMP Sweep, Keeping Every Partition
			result = champ_community_detection(prep.champ_edges;
			                                   nodes                = prep.node_ids,
			                                   resolution_range     = resolution_range,
			                                   n_resolutions        = n_resolutions,
			                                   n_runs_per_gamma     = n_runs_per_gamma,
			                                   n_iterations_per_run = n_iterations_per_run,
			                                   weighted             = false,
			                                   directed             = true,
			                                   seed                 = seed,
			                                   keep_partitions      = true,
			                                   show_progress        = show_progress)

		#	Node Order Check
			if result.node_names != prep.node_ids
				throw(ErrorException("champ_era_sweep: CHAMP returned vertices in an unexpected order"))
			end

		#	Granularity at Every Gamma
			n_gamma = length(result.gammas)
			gran = Vector{NamedTuple}(undef, n_gamma)
			max_pen = zeros(Float64, n_gamma)
			for ix in 1:n_gamma
				full = _expand_membership(result.memberships[ix], prep.kept, prep.comp)
				gran[ix] = community_granularity(full, prep.outdeg;
				                                 n_detected = maximum(result.memberships[ix]))
				max_pen[ix] = _max_penalty_share(full, prep.outdeg, prep.m, result.gammas[ix])
			end

		#	Elbow Partition Against the Component Partition
			best     = result.best_index
			γ_star   = result.resolution_used
			adj      = prep.adj
			m        = prep.m
			comp_kept = prep.comp[prep.kept]
			Q_star = calculate_modularity(adj, result.membership;
			                              weighted = false, directed = true, γ = γ_star)
			Q_comp = calculate_modularity(adj, comp_kept;
			                              weighted = false, directed = true, γ = γ_star)

		#	Per-Gamma Sweep Table
			sweep = DataFrame(gamma                     = result.gammas,
			                  A                         = result.A_coeffs,
			                  P                         = result.P_coeffs,
			                  modularity                = result.modularities,
			                  n_communities             = result.n_communities_per_gamma,
			                  n_communities_full        = [g.n_communities for g in gran],
			                  dominant                  = result.dominant,
			                  dominance_lower           = result.dominance_lower,
			                  dominance_upper           = result.dominance_upper,
			                  selected                  = [i == best for i in 1:n_gamma],
			                  k_eff_proxy               = [p > 0 ? m / p : NaN for p in result.P_coeffs],
			                  k_eff_citer               = [g.k_eff_citer for g in gran],
			                  g_citer                   = [g.g_citer for g in gran],
			                  n_ge1                     = [g.n_ge1 for g in gran],
			                  n_ge3                     = [g.n_ge3 for g in gran],
			                  n_ge5                     = [g.n_ge5 for g in gran],
			                  n_ge10                    = [g.n_ge10 for g in gran],
			                  largest_share_citer       = [g.largest_share_citer for g in gran],
			                  size_gini_citer           = [g.size_gini_citer for g in gran],
			                  singleton_community_share = [g.singleton_community_share for g in gran],
			                  singleton_article_share   = [g.singleton_article_share for g in gran],
			                  max_penalty_share         = max_pen)

		#	Era Diagnostics
			k_citer = sweep.k_eff_citer
			diagnostics = (
				net_file               = prep.net_file,
				resolution_min         = resolution_range[1],
				resolution_max         = resolution_range[2],
				n_resolutions          = n_resolutions,
				n_runs_per_gamma       = n_runs_per_gamma,
				n_vertices             = prep.n,
				n_arcs                 = nrow(prep.arcs),
				n_duplicate_arcs       = prep.n_duplicate_arcs,
				n_self_loops           = prep.n_self_loops,
				n_components           = length(prep.sizes),
				min_component_size     = min_component_size,
				n_kept_vertices        = length(prep.kept),
				n_kept_arcs            = nrow(prep.kept_arcs),
				n_kept_components      = count(>=(min_component_size), prep.sizes),
				n_restored_components  = prep.n_restored,
				giant_component_share  = maximum(prep.sizes) / prep.n,
				m                      = m,
				W                      = prep.n_citing,
				gamma_star             = γ_star,
				best_index             = best,
				gamma_interior         = 1 < best < n_gamma,
				n_dominant             = count(result.dominant),
				n_communities_star     = result.n_communities,
				modularity_star        = Q_star,
				modularity_components  = Q_comp,
				gain_over_components   = Q_star - Q_comp,
				penalty_share_star     = γ_star * result.P_coeffs[best] / m,
				k_eff_citer_min        = minimum(k_citer),
				k_eff_citer_max        = maximum(k_citer),
				k_eff_citer_monotone   = issorted(k_citer),
				g_citer_min            = minimum(sweep.g_citer),
				g_citer_max            = maximum(sweep.g_citer),
				max_penalty_share      = maximum(max_pen),
				penalty_condition_met  = maximum(max_pen) < 1.0,
				modularity_min         = minimum(result.modularities),
				modularity_max         = maximum(result.modularities),
				modularity_range       = maximum(result.modularities) - minimum(result.modularities),
				n_communities_min      = minimum(result.n_communities_per_gamma),
				n_communities_max      = maximum(result.n_communities_per_gamma),
				n_communities_range    = maximum(result.n_communities_per_gamma) -
				                         minimum(result.n_communities_per_gamma)
			)

		#	Full-Length Membership at the Elbow
			membership = _expand_membership(result.membership, prep.kept, prep.comp)

		#	Assembling Result
			return (diagnostics = diagnostics,
			        sweep       = sweep,
			        membership  = membership,
			        labels      = prep.labels)
	end
	@doc raw"""
	**Description**
	Run a CHAMP resolution sweep on one era network and record what the
	granularity design needs: the hull coefficients and admissible-set interval at
	every gamma, the citer-weighted granularity at every gamma (the calibration
	curve), the `m/P` proxy beside it, and the era diagnostics.

	**Usage**
	`champ_era_sweep(net_file; min_component_size=4, resolution_range=(0.1, 6.0), n_resolutions=60, n_runs_per_gamma=5, n_iterations_per_run=10, seed=nothing, show_progress=true)`

	**Arguments**
	- `net_file::AbstractString`: Path to a directed (`*Arcs`) Pajek network.
	- `min_component_size::Int`: Weak components smaller than this are not swept
	  (default `4`, which drops isolates, dyads, and triads).
	- `resolution_range::Tuple{Float64,Float64}`: Gamma range (default `(0.1, 6.0)`, grid 2 of the design doc).
	- `n_resolutions::Int`: Gamma values in the grid (default `60`).
	- `n_runs_per_gamma::Int`: Leiden multi-starts per gamma (default `5`).
	- `n_iterations_per_run::Int`: Leiden iterations per run (default `10`).
	- `seed::Union{Int,Nothing}`: Leiden base seed. With `nothing`, run `r` is
	  seeded with `r - 1`, so the sweep is reproducible either way.
	- `show_progress::Bool`: Show the CHAMP progress bar (default `true`).

	**Details**
	The network is treated as directed and unweighted, and duplicate arcs are
	collapsed. Weak components are computed with direction ignored. CHAMP is run on
	the vertices of components at or above `min_component_size`; every smaller
	component is restored afterward as a community of its own, so every membership
	covers every vertex of the `.net` file in its vertex order. The sweep table's
	`n_communities` is the detected count and `n_communities_full` the count with
	restored components, the two counts the design distinguishes (sec. 5.1).

	Each era is swept separately. Pooling the eras into one graph before sweeping
	would give a block-diagonal network in which each era's effective resolution is
	gamma scaled by its share of the arcs.

	The columns that serve the design are:
	- `k_eff_citer`, `g_citer`: citer-weighted effective count and granularity at each gamma, from `community_granularity`. The calibration inverts `gamma → k_eff_citer`.
	- `k_eff_proxy`: `m/P`, the degree-weighted proxy of sec. 5.5, beside the measured value so the correction is read from one table.
	- `n_ge1 … n_ge10`, `largest_share_citer`, `size_gini_citer`, the singleton shares: the shape of the size distribution, which concentration alone does not capture (sec. 7.2).
	- `max_penalty_share`: the largest `gamma · K_out_c / m`, which must stay below 1 for the sec. 2 argument to hold.
	- `dominant`, `dominance_lower`, `dominance_upper`: the admissible set and each member's interval, computed exactly (Appendix A).

	The diagnostics add `W` (citing articles), `n_restored_components`, the range
	and monotonicity of `k_eff_citer`, and `penalty_condition_met`.

	**Value**
	A `NamedTuple`:
	- `diagnostics::NamedTuple`: one row of era diagnostics.
	- `sweep::DataFrame`: one row per gamma, as above.
	- `membership::Vector{Int}`: the elbow partition over every vertex, in `.net` vertex order, kept as an envelope diagnostic.
	- `labels::Vector{String}`: the `.net` vertex labels, which carry node ids.

	**Examples**
	```julia
		#	Sweep one era and read its calibration curve
			res = champ_era_sweep("pajek_files/eras_7yr/era17.net")
			res.sweep[:, [:gamma, :k_eff_citer, :k_eff_proxy]]
	```

	**See Also**
	`write_champ_sweep`, `write_champ_summary`, `calibrate_resolution`, `granularity_probe`
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
			The .clu holds the partition at the CHAMP elbow, gamma_star. It is an
			envelope diagnostic, not a partition the analysis uses; those are
			written by write_partition_at_resolution, tagged by specification and
			with their gamma recorded beside them.
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

		#	Write the Elbow Partition
			clu_path = write_clu(sweep_result.membership, "$(prefix)_local_gamma"; directory = directory)

		#	Assembling Result
			return (sweep = sweep_path, clu = clu_path)
	end
	@doc raw"""
	**Description**
	Write one era's CHAMP sweep: the per-gamma table and the partition at the
	CHAMP elbow.

	**Usage**
	`write_champ_sweep(sweep_result, prefix; directory=pwd())`

	**Arguments**
	- `sweep_result::NamedTuple`: Output of `champ_era_sweep`.
	- `prefix::AbstractString`: File stem, such as `"era12"`.
	- `directory::AbstractString`: Destination directory (default `pwd()`); must exist.

	**Details**
	Writes `<prefix>_sweep.tsv`, one row per gamma, and `<prefix>_local_gamma.clu`,
	the elbow partition over every vertex of the era network. The `.clu` is an
	envelope diagnostic; its community count includes restored components, and the
	gamma it was written at is the `gamma_star` column of the summary table.

	**Value**
	A `NamedTuple` `(sweep, clu)` of the paths written.

	**See Also**
	`champ_era_sweep`, `read_champ_sweep`, `write_partition_at_resolution`
	""" write_champ_sweep

#	Read One Era's Sweep Back
	function read_champ_sweep(path::AbstractString)
		"""
		Args:
			path::AbstractString: an era<NN>_sweep.tsv written by write_champ_sweep
		Returns:
			DataFrame: the sweep table, typed
		Notes:
			A calibration run reads sweeps already on disk rather than re-sweeping,
			so the hour of Leiden runs is spent once. A sweep written before the
			granularity columns existed is refused, with the reason.
		"""

		#	Read
			if !isfile(path)
				throw(ArgumentError("read_champ_sweep: no such file: $path"))
			end
			sweep = _read_tsv(path)

		#	Require the Calibration Curve
			if !("k_eff_citer" in names(sweep))
				throw(ArgumentError("read_champ_sweep: $path has no k_eff_citer column; it was " *
				                    "written before granularity was measured. Re-run the sweep."))
			end

		#	Assembling Result
			return sweep
	end
	@doc raw"""
	**Description**
	Read an `era<NN>_sweep.tsv` back into a typed `DataFrame`, for calibration
	runs that invert a sweep already on disk.

	**Usage**
	`read_champ_sweep(path)`

	**Value**
	The sweep table. A file written before the `k_eff_citer` column existed is
	refused, since it holds no calibration curve.

	**See Also**
	`write_champ_sweep`, `calibrate_resolution`
	""" read_champ_sweep

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

	**See Also**
	`champ_era_sweep`, `write_champ_sweep`
	""" write_champ_summary

#############
#   PROBE   #
#############

#	Validation Probe: Monotonicity, Run-to-Run Variation, and Agreement Across Seeds
	function granularity_probe(net_file::AbstractString;
	                           gammas::AbstractVector{<:Real} = [0.5, 1.0, 2.0, 3.0, 4.0],
	                           seeds::AbstractVector{Int} = collect(0:9),
	                           min_component_size::Int = 4,
	                           n_iterations_per_run::Int = 10,
	                           show_progress::Bool = true)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			gammas::AbstractVector: resolutions to probe (default = [0.5, 1, 2, 3, 4])
			seeds::AbstractVector{Int}: Leiden seeds per gamma (default = 0:9)
			min_component_size::Int: smallest weak component kept (default = 4)
			n_iterations_per_run::Int: Leiden iterations per run (default = 10)
			show_progress::Bool: progress bar over gamma x seed (default = true)
		Returns:
			NamedTuple: (runs, agreement, summary, monotone)
		Notes:
			Implements sec. 9.1 of the design. At each gamma, one Leiden run per
			seed -- a single start, not the best of five, since a best-of-five
			masks exactly the run-to-run variation being measured. For each run:
			citer-weighted k_eff and g, the m/P proxy, the detected and restored
			counts, and the largest gamma * K_out_c / m. For each pair of seeds at
			one gamma: adjusted mutual information and adjusted Rand index of their
			memberships restricted to the citing articles, since agreement among
			cited-only vertices is mostly inherited from their citers.

			summary gives, per gamma, the mean and coefficient of variation of
			k_eff_citer across seeds (requirement 2), the mean and minimum AMI
			(requirement 3), and the mean proxy-to-measured ratio (sec. 5.5).
			monotone reports whether mean k_eff_citer rises with gamma
			(requirement 1). Requirements 4 and 5 -- the selection rule among
			near-equivalent partitions and the tolerance -- are decided from these
			numbers and implemented in calibrate_resolution.

			The design marks requirement 3 as a decision point: if AMI across seeds
			is poor at a fixed gamma, the partitions are not a stable foundation
			under any scaling rule, and bibliographic coupling moves from a
			validation exercise to a prerequisite.
		"""

		#	Validation
			if isempty(gammas) || isempty(seeds)
				throw(ArgumentError("granularity_probe: gammas and seeds must be non-empty"))
			end
			if any(<=(0.0), gammas)
				throw(ArgumentError("granularity_probe: every gamma must be positive"))
			end
			if length(unique(seeds)) != length(seeds)
				throw(ArgumentError("granularity_probe: seeds must be distinct"))
			end

		#	Read and Reduce
			prep = _prepare_era_network(net_file, min_component_size)
			if isempty(prep.kept) || nrow(prep.kept_arcs) == 0
				throw(ArgumentError("granularity_probe: no component of $min_component_size or more " *
				                    "vertices carries an arc in $net_file"))
			end
			citing = findall(>(0), prep.outdeg)
			gammas_f = Float64.(collect(gammas))

		#	Progress
			prog = show_progress ? Progress(length(gammas_f) * length(seeds);
			                                desc = "Granularity probe") : nothing

		#	One Leiden Run per Gamma and Seed
			run_rows = NamedTuple[]
			citing_memberships = Dict{Tuple{Float64,Int},Vector{Int}}()
			for γ in gammas_f, s in seeds
				res = champ_community_detection(prep.champ_edges;
				                                nodes                = prep.node_ids,
				                                resolution           = γ,
				                                n_runs_per_gamma     = 1,
				                                n_iterations_per_run = n_iterations_per_run,
				                                weighted             = false,
				                                directed             = true,
				                                seed                 = s,
				                                show_progress        = false)
				if res.node_names != prep.node_ids
					throw(ErrorException("granularity_probe: vertices returned in an unexpected order"))
				end
				full = _expand_membership(res.membership, prep.kept, prep.comp)
				gr   = community_granularity(full, prep.outdeg; n_detected = maximum(res.membership))
				P    = res.P_coeffs[1]
				push!(run_rows, (gamma               = γ,
				                 seed                = s,
				                 modularity          = res.modularity,
				                 n_detected          = gr.n_detected,
				                 n_restored          = gr.n_restored,
				                 W                   = gr.W,
				                 k_eff_citer         = gr.k_eff_citer,
				                 g_citer             = gr.g_citer,
				                 k_eff_proxy         = P > 0 ? prep.m / P : NaN,
				                 n_ge1               = gr.n_ge1,
				                 n_ge3               = gr.n_ge3,
				                 n_ge5               = gr.n_ge5,
				                 n_ge10              = gr.n_ge10,
				                 largest_share_citer = gr.largest_share_citer,
				                 size_gini_citer     = gr.size_gini_citer,
				                 max_penalty_share   = _max_penalty_share(full, prep.outdeg, prep.m, γ)))
				citing_memberships[(γ, s)] = full[citing]
				show_progress && next!(prog)
			end
			runs = vcat([DataFrame([r]) for r in run_rows]...)

		#	Pairwise Agreement Across Seeds at Each Gamma
			agree_rows = NamedTuple[]
			for γ in gammas_f
				for (ia, sa) in enumerate(seeds), sb in seeds[(ia + 1):end]
					ma = citing_memberships[(γ, sa)]
					mb = citing_memberships[(γ, sb)]
					push!(agree_rows, (gamma  = γ,
					                   seed_a = sa,
					                   seed_b = sb,
					                   ami    = adjusted_mutual_information(ma, mb),
					                   ari    = adjusted_rand_index(ma, mb)))
				end
			end
			agreement = isempty(agree_rows) ?
				DataFrame(gamma = Float64[], seed_a = Int[], seed_b = Int[], ami = Float64[], ari = Float64[]) :
				vcat([DataFrame([r]) for r in agree_rows]...)

		#	Per-Gamma Summary
			summary_rows = NamedTuple[]
			for γ in gammas_f
				r = runs[runs.gamma .== γ, :]
				a = agreement[agreement.gamma .== γ, :]
				k = r.k_eff_citer
				μ = sum(k) / length(k)
				σ = length(k) > 1 ? sqrt(sum((k .- μ) .^ 2) / (length(k) - 1)) : 0.0
				ratio = r.k_eff_proxy ./ r.k_eff_citer
				push!(summary_rows, (gamma                 = γ,
				                     n_seeds               = length(k),
				                     k_eff_citer_mean      = μ,
				                     k_eff_citer_sd        = σ,
				                     k_eff_citer_cv        = μ > 0 ? σ / μ : NaN,
				                     k_eff_citer_min       = minimum(k),
				                     k_eff_citer_max       = maximum(k),
				                     g_citer_mean          = sum(r.g_citer) / length(k),
				                     proxy_ratio_mean      = sum(ratio) / length(ratio),
				                     ami_mean              = nrow(a) == 0 ? NaN : sum(a.ami) / nrow(a),
				                     ami_min               = nrow(a) == 0 ? NaN : minimum(a.ami),
				                     ari_mean              = nrow(a) == 0 ? NaN : sum(a.ari) / nrow(a),
				                     max_penalty_share     = maximum(r.max_penalty_share)))
			end
			summary = vcat([DataFrame([r]) for r in summary_rows]...)
			sort!(summary, :gamma)

		#	Assembling Result
			return (runs      = runs,
			        agreement = agreement,
			        summary   = summary,
			        monotone  = issorted(summary.k_eff_citer_mean))
	end
	@doc raw"""
	**Description**
	The validation the granularity design requires before any partition is written
	(sec. 9.1): whether citer-weighted `k_eff` is monotone in gamma, how much it
	moves across Leiden seeds at a fixed gamma, and how far the partitions those
	seeds return agree in membership.

	**Usage**
	`granularity_probe(net_file; gammas=[0.5,1,2,3,4], seeds=0:9, min_component_size=4, n_iterations_per_run=10, show_progress=true)`

	**Arguments**
	- `net_file::AbstractString`: Path to a directed (`*Arcs`) Pajek network.
	- `gammas`: Resolutions to probe.
	- `seeds`: Leiden seeds; one single-start run per seed at each gamma.
	- `min_component_size::Int`: Weak components smaller than this are not swept.
	- `n_iterations_per_run::Int`: Leiden iterations per run.
	- `show_progress::Bool`: Progress bar over gamma × seed.

	**Details**
	Each run is a single Leiden start, not the best of several, because a
	best-of-five hides the run-to-run variation the probe exists to measure.
	Agreement between seeds is measured on the citing articles only, with adjusted
	mutual information (chance-corrected, and suited to partitions with different
	cluster counts) and the adjusted Rand index beside it.

	The three questions the design puts to this probe:
	1. **Monotonicity** — `monotone` is true when mean `k_eff_citer` rises with gamma over the probed values. The calibration inverts this curve, which needs it.
	2. **Run-to-run variation** — `summary.k_eff_citer_cv` per gamma. If it is comparable to the calibration tolerance, calibration cannot be told from noise.
	3. **Partition stability** — `summary.ami_mean` and `ami_min` per gamma. The design marks this a decision point: poor agreement means the partitions are not a stable foundation under any scaling rule.

	`summary.proxy_ratio_mean` is `m/P` over the measured citer-weighted count, the
	correction the design's sec. 5.5 says must be measured rather than assumed.
	`max_penalty_share` checks the sec. 2 condition.

	**Value**
	A `NamedTuple` `(runs, agreement, summary, monotone)`: one row per gamma × seed,
	one per gamma × seed pair, one per gamma, and the monotonicity verdict.

	**Examples**
	```julia
		#	Probe era 17 at five resolutions with ten seeds
			probe = granularity_probe("pajek_files/eras_7yr/era17.net")
			probe.summary[:, [:gamma, :k_eff_citer_cv, :ami_mean, :ami_min]]
			probe.monotone
	```

	**See Also**
	`write_granularity_probe`, `adjusted_mutual_information`, `calibrate_resolution`
	""" granularity_probe

#	Write One Era's Probe
	function write_granularity_probe(probe::NamedTuple, prefix::AbstractString;
	                                 directory::AbstractString = pwd())
		"""
		Args:
			probe::NamedTuple: output of granularity_probe
			prefix::AbstractString: file stem, e.g. "era17"
			directory::AbstractString: destination directory (default = pwd())
		Returns:
			NamedTuple: (runs, agreement, summary), the three paths written
		Notes:
			Three tables, so the per-run and per-pair detail stays available when
			the per-gamma summary raises a question.
		"""

		#	Validation
			if !isdir(directory)
				throw(ArgumentError("write_granularity_probe: output directory does not exist: $directory"))
			end
			if !all(k -> haskey(probe, k), (:runs, :agreement, :summary))
				throw(ArgumentError("write_granularity_probe: expected the output of granularity_probe"))
			end

		#	Write
			runs_path      = _write_tsv(probe.runs,      joinpath(directory, "$(prefix)_probe_runs.tsv"))
			agreement_path = _write_tsv(probe.agreement, joinpath(directory, "$(prefix)_probe_agreement.tsv"))
			summary_path   = _write_tsv(probe.summary,   joinpath(directory, "$(prefix)_probe_summary.tsv"))

		#	Assembling Result
			return (runs = runs_path, agreement = agreement_path, summary = summary_path)
	end
	@doc raw"""
	**Description**
	Write one era's validation probe as three tables: `<prefix>_probe_runs.tsv`,
	`<prefix>_probe_agreement.tsv` and `<prefix>_probe_summary.tsv`.

	**Usage**
	`write_granularity_probe(probe, prefix; directory=pwd())`

	**See Also**
	`granularity_probe`
	""" write_granularity_probe

###################
#   CALIBRATION   #
###################

#	Invert a Gamma -> k_eff Curve to a Target
	function calibrate_resolution(gammas::AbstractVector{<:Real}, k_eff::AbstractVector{<:Real},
	                              target::Real; tolerance::Real = 0.10)
		"""
		Args:
			gammas::AbstractVector: the swept resolutions
			k_eff::AbstractVector: citer-weighted k_eff at each, from the sweep table
			target::Real: the k_eff the scaling rule asks for in this era
			tolerance::Real: relative deviation accepted as a successful calibration
				(default = 0.10, to be replaced by the value the probe justifies)
		Returns:
			NamedTuple: the selected gamma, target and achieved separately, and the
				flags the design asks for
		Notes:
			Deviation is measured as |log(k_eff / target)|, which treats a miss
			above and a miss below the target alike; a tolerance of 0.10 admits
			k_eff between target / 1.1 and target * 1.1.

			Selection rule (design sec. 9.1, requirement 4): among grid points within
			tolerance, take the one nearest the target; on a tie, the smaller gamma.
			When no grid point is within tolerance, the nearest is returned with met
			= false. gamma_within_min and gamma_within_max show how far apart the
			admissible grid points lie, which is the ambiguity the rule resolves;
			when they span much of the grid the curve is not monotone and the probe
			should be consulted before the gamma is used.

			at_bound is true when the selected point is the first or last of the
			grid, so the target may lie outside the swept range (design sec. 10,
			the censored floor).

			The achieved value here is the sweep's, from the best of its runs at
			that gamma. write_partition_at_resolution re-runs Leiden and reports
			its own achieved value beside this one.
		"""

		#	Validation
			n = length(gammas)
			if length(k_eff) != n || n == 0
				throw(ArgumentError("calibrate_resolution: gammas and k_eff must be non-empty and the same length"))
			end
			if !(target > 0)
				throw(ArgumentError("calibrate_resolution: target must be positive"))
			end
			if !(tolerance >= 0)
				throw(ArgumentError("calibrate_resolution: tolerance must be non-negative"))
			end

		#	Work in Gamma Order
			order = sortperm(collect(gammas))
			γs = Float64.(collect(gammas))[order]
			ks = Float64.(collect(k_eff))[order]
			valid = findall(k -> isfinite(k) && k > 0, ks)
			if isempty(valid)
				throw(ArgumentError("calibrate_resolution: no finite positive k_eff on the curve"))
			end

		#	Log Deviation from Target
			dev = fill(Inf, n)
			dev[valid] .= abs.(log.(ks[valid] ./ target))
			bound = log(1 + tolerance)
			within = findall(d -> d <= bound, dev)

		#	Select
			if isempty(within)
				idx = argmin(dev)
				met = false
			else
				best = minimum(dev[within])
				idx = within[findfirst(i -> dev[i] == best, within)]
				met = true
			end

		#	Assembling Result
			return (gamma              = γs[idx],
			        index              = idx,
			        target_k_eff       = Float64(target),
			        achieved_k_eff     = ks[idx],
			        relative_deviation = ks[idx] / target - 1,
			        tolerance          = Float64(tolerance),
			        met                = met,
			        at_bound           = idx == 1 || idx == n,
			        n_within_tolerance = length(within),
			        gamma_within_min   = isempty(within) ? NaN : γs[first(within)],
			        gamma_within_max   = isempty(within) ? NaN : γs[last(within)],
			        curve_monotone     = issorted(ks[valid]))
	end
	@doc raw"""
	**Description**
	Invert one era's `gamma → k_eff` calibration curve to a target effective count,
	reporting the target and the achieved value separately, whether the tolerance
	was met, and whether the search pressed against a bound of the grid (design
	doc, sec. 9.2).

	**Usage**
	`calibrate_resolution(gammas, k_eff, target; tolerance=0.10)`

	**Arguments**
	- `gammas`, `k_eff`: The sweep's resolutions and citer-weighted effective counts, as in `read_champ_sweep(...).gamma` and `.k_eff_citer`.
	- `target::Real`: The `k_eff` the scaling rule asks for: `k_ref · (W_e / W_ref)^α`.
	- `tolerance::Real`: Relative deviation accepted (default `0.10`). The design says this must be justified by the probe's run-to-run variation and membership stability, not chosen; the default is a placeholder.

	**Details**
	Deviation is `|log(k_eff / target)|`, symmetric above and below. Among grid
	points within tolerance the nearest to the target is taken, with the smaller
	gamma on a tie; the span `gamma_within_min … gamma_within_max` shows how far
	apart the acceptable points lie, which is the ambiguity requirement 4 of
	sec. 9.1 asks the selection rule to resolve. `at_bound` flags a selection at
	the edge of the grid, where the target may lie outside the swept range.

	**Value**
	A `NamedTuple` with `gamma`, `index`, `target_k_eff`, `achieved_k_eff`,
	`relative_deviation`, `tolerance`, `met`, `at_bound`, `n_within_tolerance`,
	`gamma_within_min`, `gamma_within_max` and `curve_monotone`.

	**Examples**
	```julia
		#	Era 17 at alpha = 1, anchored at g = 120 in era 14
			sweep = read_champ_sweep("era17_sweep.tsv")
			k_ref = W[14] / 120
			target = k_ref * (W[17] / W[14])^1.0
			cal = calibrate_resolution(sweep.gamma, sweep.k_eff_citer, target)
			cal.gamma, cal.met, cal.at_bound
	```

	**See Also**
	`read_champ_sweep`, `write_partition_at_resolution`, `granularity_probe`
	""" calibrate_resolution

#	Write the Partition at a Declared Gamma
	function write_partition_at_resolution(net_file::AbstractString, gamma::Real;
	                                       prefix::AbstractString,
	                                       spec::AbstractString,
	                                       directory::AbstractString = pwd(),
	                                       target_k_eff::Real = NaN,
	                                       seed::Union{Int,Nothing} = 0,
	                                       min_component_size::Int = 4,
	                                       n_runs_per_gamma::Int = 5,
	                                       n_iterations_per_run::Int = 10)
		"""
		Args:
			net_file::AbstractString: path to an era .net file (directed, *Arcs)
			gamma::Real: the resolution the calibration selected
			prefix::AbstractString: file stem, e.g. "era17"
			spec::AbstractString: specification tag, e.g. "alpha1.00_g120_e14"
			directory::AbstractString: destination directory (default = pwd())
			target_k_eff::Real: the target the gamma was chosen for (default = NaN)
			seed::Union{Int,Nothing}: Leiden base seed (default = 0)
			min_component_size::Int: smallest weak component kept (default = 4)
			n_runs_per_gamma::Int: Leiden multi-starts (default = 5)
			n_iterations_per_run::Int: Leiden iterations per run (default = 10)
		Returns:
			NamedTuple: (meta, membership, labels, clu, meta_path)
		Notes:
			Runs Leiden at the declared gamma, expands the membership over the full
			.net order, and writes <prefix>_<spec>.clu together with
			<prefix>_<spec>_meta.tsv. Pajek .clu files carry no metadata, so the
			gamma, seed, specification and achieved granularity live in the
			sidecar, which is what the design means by "with the gamma recorded in
			the file" (sec. 9.2). Specifications coexist because the tag is in the
			file name.

			Leiden is re-run here rather than copied from the sweep, so the
			achieved k_eff is measured on the partition actually written and
			reported beside the target. With a fixed seed the write is
			reproducible.
		"""

		#	Validation
			if !(gamma > 0)
				throw(ArgumentError("write_partition_at_resolution: gamma must be positive"))
			end
			if !isdir(directory)
				throw(ArgumentError("write_partition_at_resolution: output directory does not exist: $directory"))
			end
			if isempty(spec) || occursin(r"[\s/\\]", spec)
				throw(ArgumentError("write_partition_at_resolution: spec must be a file-safe tag"))
			end

		#	Read and Reduce
			prep = _prepare_era_network(net_file, min_component_size)
			if isempty(prep.kept) || nrow(prep.kept_arcs) == 0
				throw(ArgumentError("write_partition_at_resolution: no component of $min_component_size " *
				                    "or more vertices carries an arc in $net_file"))
			end

		#	Detect at the Declared Gamma
			γ = Float64(gamma)
			res = champ_community_detection(prep.champ_edges;
			                                nodes                = prep.node_ids,
			                                resolution           = γ,
			                                n_runs_per_gamma     = n_runs_per_gamma,
			                                n_iterations_per_run = n_iterations_per_run,
			                                weighted             = false,
			                                directed             = true,
			                                seed                 = seed,
			                                show_progress        = false)
			if res.node_names != prep.node_ids
				throw(ErrorException("write_partition_at_resolution: vertices returned in an unexpected order"))
			end

		#	Expand and Measure
			membership = _expand_membership(res.membership, prep.kept, prep.comp)
			gr = community_granularity(membership, prep.outdeg; n_detected = maximum(res.membership))
			P  = res.P_coeffs[1]

		#	Write the Partition and Its Sidecar
			stem = "$(prefix)_$(spec)"
			clu_path = write_clu(membership, stem; directory = directory)
			meta = (net_file                  = prep.net_file,
			        spec                      = String(spec),
			        gamma                     = γ,
			        seed                      = isnothing(seed) ? -1 : seed,
			        n_runs_per_gamma          = n_runs_per_gamma,
			        n_iterations_per_run      = n_iterations_per_run,
			        min_component_size        = min_component_size,
			        written                   = Libc.strftime("%Y-%m-%dT%H:%M:%S", time()),
			        modularity                = res.modularity,
			        W                         = gr.W,
			        target_k_eff              = Float64(target_k_eff),
			        achieved_k_eff_citer      = gr.k_eff_citer,
			        relative_deviation        = isnan(target_k_eff) ? NaN : gr.k_eff_citer / target_k_eff - 1,
			        g_citer                   = gr.g_citer,
			        k_eff_proxy               = P > 0 ? prep.m / P : NaN,
			        n_detected                = gr.n_detected,
			        n_restored                = gr.n_restored,
			        n_communities_full        = gr.n_communities,
			        n_ge1                     = gr.n_ge1,
			        n_ge3                     = gr.n_ge3,
			        n_ge5                     = gr.n_ge5,
			        n_ge10                    = gr.n_ge10,
			        largest_share_citer       = gr.largest_share_citer,
			        size_gini_citer           = gr.size_gini_citer,
			        singleton_community_share = gr.singleton_community_share,
			        singleton_article_share   = gr.singleton_article_share,
			        max_penalty_share         = _max_penalty_share(membership, prep.outdeg, prep.m, γ))
			meta_path = _write_tsv(DataFrame([meta]), joinpath(directory, "$(stem)_meta.tsv"))

		#	Assembling Result
			return (meta       = meta,
			        membership = membership,
			        labels     = prep.labels,
			        clu        = clu_path,
			        meta_path  = meta_path)
	end
	@doc raw"""
	**Description**
	Run Leiden at a declared gamma and write the partition over every vertex of the
	era network, tagged by specification, with the gamma and the achieved
	granularity recorded beside it.

	**Usage**
	`write_partition_at_resolution(net_file, gamma; prefix, spec, directory=pwd(), target_k_eff=NaN, seed=0, min_component_size=4, n_runs_per_gamma=5, n_iterations_per_run=10)`

	**Arguments**
	- `net_file::AbstractString`: Path to a directed (`*Arcs`) Pajek network.
	- `gamma::Real`: The resolution `calibrate_resolution` selected.
	- `prefix`, `spec`: File stem and specification tag; the files are `<prefix>_<spec>.clu` and `<prefix>_<spec>_meta.tsv`.
	- `target_k_eff::Real`: The target the gamma was chosen for, recorded in the sidecar.
	- `seed`: Leiden base seed, so the write is reproducible.

	**Details**
	Pajek `.clu` files carry no metadata, so the sidecar TSV holds what the design
	says must travel with a partition (sec. 9.2): its gamma, seed, specification,
	target and achieved effective count, the detected and restored community
	counts, and the size-distribution summaries. Leiden is re-run at the gamma
	rather than copied from the sweep, so `achieved_k_eff_citer` is measured on
	the partition actually written.

	**Value**
	A `NamedTuple` `(meta, membership, labels, clu, meta_path)`.

	**Examples**
	```julia
		#	Write era 17 under the primary specification
			out = write_partition_at_resolution("pajek_files/eras_7yr/era17.net", cal.gamma;
			                                    prefix = "era17", spec = "alpha1.00_g120_e14",
			                                    directory = spec_dir, target_k_eff = cal.target_k_eff)
			out.meta.achieved_k_eff_citer
	```

	**See Also**
	`calibrate_resolution`, `community_granularity`, `write_clu`
	""" write_partition_at_resolution

###############
#   EXPORTS   #
###############

#	Public Surface
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
		   adjusted_mutual_information,
		   profile_era_network,
		   community_granularity,
		   champ_era_sweep,
		   write_champ_sweep,
		   read_champ_sweep,
		   write_champ_summary,
		   granularity_probe,
		   write_granularity_probe,
		   calibrate_resolution,
		   write_partition_at_resolution

end # module community_detection_tools
