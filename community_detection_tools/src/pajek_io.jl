#	pajek_io.jl -- Reading and Writing Pajek .net, .clu, and .vec Files
#	Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
#	8 September 2026
#
#	WHERE THIS SITS. First of three source files included by
#	community_detection_tools.jl:
#
#		pajek_io.jl      (this file)  Pajek format in and out
#		leiden_tools.jl               Leiden community detection
#		champ_tools.jl                CHAMP resolution selection
#
#	Ported from Balikatan_2024.jl with four changes, each noted at the function
#	that carries it: writers take a directory rather than writing to the process
#	working directory; section boundaries in a .net file are located rather than
#	assumed; arcs without an explicit weight are accepted; and .clu and .vec
#	headers are checked against the number of values that follow.
#
#	All read and write functions are public facing and carry attached docstrings.
#	Helpers follow the Large_Graph_Similarity convention: a leading underscore
#	marks a private function, documented for developers only.

#	Packages
	using DataFrames

################
#   HELPERS    #
################

#	Helper Function for _parse_numeric: Numeric String Test
	function _is_string_numeric(s::AbstractString)
		"""
		Args:
			s::AbstractString: candidate string
		Returns:
			Bool: true when the string parses as a Float64
		Notes:
			Used to decide whether a column of Pajek values is numeric before
			attempting a vector-wide conversion.
		"""

		#	Assemble result
			return tryparse(Float64, String(s)) !== nothing
	end

#	Helper Function for read_clu and read_vec: Vector Type Coercion
	function _parse_numeric(data::Vector{String})
		"""
		Args:
			data::Vector{String}: raw values read from a Pajek file
		Returns:
			Vector{Int}, Vector{Float64}, or the input unchanged
		Notes:
			Integers are preferred when every element parses as one, which keeps a
			partition an integer vector rather than a float vector. Falls back to
			Float64, then to the original strings, so a malformed file surfaces as
			unexpected strings rather than as a parse error.
		"""

		#	Prefer Integers
			if all(d -> tryparse(Int, d) !== nothing, data)
				return parse.(Int, data)
			end

		#	Fall Back to Floats
			if all(d -> tryparse(Float64, d) !== nothing, data)
				return parse.(Float64, data)
			end

		#	Assemble result
			return data
	end

#	Helper Function for read_net: Vertex Line Parsing
	function _parse_node_line(nodes_data::Vector{String})
		"""
		Args:
			nodes_data::Vector{String}: vertex lines, header excluded
		Returns:
			DataFrame: columns id::Int, label::String
		Notes:
			A Pajek vertex line is an index, a quoted label, then optional
			coordinates and display attributes:

				1 "8112"    0.0000 0.0000 0.5000 ic Blue bc White

			Splitting on the quote character isolates the index and the label and
			discards everything after, which is display state rather than data.
			A line whose label is not quoted is an error here rather than a silent
			fall-through, because an unquoted label cannot be distinguished from a
			coordinate.
		"""

		#	Split on the Quote Character
			elements = split.(nodes_data, "\""; keepempty = false)

		#	Validation
			bad = findfirst(e -> length(e) < 2, elements)
			if bad !== nothing
				throw(ArgumentError("pajek_io: vertex line $bad has no quoted label: " *
									String(nodes_data[bad])))
			end

		#	Extract the Index and the Label
			ids    = parse.(Int, String.(strip.([e[1] for e in elements])))
			labels = String.(strip.([e[2] for e in elements]))

		#	Assemble result
			return DataFrame(id = ids, label = labels)
	end

#	Helper Function for read_net: Arc and Edge Line Parsing
	function _parse_edge_line(edges_data::Vector{String})
		"""
		Args:
			edges_data::Vector{String}: arc or edge lines, header excluded
		Returns:
			DataFrame: columns person_i::Int, person_j::Int, weight
		Notes:
			A Pajek arc line is a source, a target, an optional weight, then
			optional display attributes:

				1 990 1 c Gray

			Only the first three whitespace-separated fields are read; trailing
			colour tokens are display state. CHANGE FROM Balikatan_2024: a line
			carrying only source and target is accepted and given unit weight,
			rather than failing on a missing third field.
		"""

		#	Split on Whitespace
			elements = map(r -> split(String(r); keepempty = false), edges_data)

		#	Validation
			bad = findfirst(e -> length(e) < 2, elements)
			if bad !== nothing
				throw(ArgumentError("pajek_io: arc line $bad has fewer than two fields: " *
									String(edges_data[bad])))
			end

		#	Extract Endpoints
			source = parse.(Int, String.(strip.([e[1] for e in elements])))
			target = parse.(Int, String.(strip.([e[2] for e in elements])))

		#	Extract Weights, Defaulting to Unity
			weight_str = [length(e) >= 3 ? String(strip(e[3])) : "1" for e in elements]
			weight = _parse_numeric(weight_str)

		#	Assemble result
			return DataFrame(person_i = source, person_j = target, weight = weight)
	end

#	Helper Function for the Pajek Writers: Output Path Resolution
	function _pajek_output_path(var_name::AbstractString, extension::AbstractString,
								directory::AbstractString)
		"""
		Args:
			var_name::AbstractString: file name, with or without its extension
			extension::AbstractString: expected extension, including the dot
			directory::AbstractString: destination directory
		Returns:
			String: full path to write to
		Notes:
			CHANGE FROM Balikatan_2024, which built `var_name * ".clu"` and opened a
			relative path, so every file landed in the process working directory. In
			a per-era pipeline that means each era overwrites the last. The
			extension is appended only when absent, so a caller may pass either a
			bare name or a complete file name. An absolute var_name is honoured and
			the directory ignored, which lets a caller bypass the argument entirely.
		"""

		#	Append the Extension Only When Absent
			name = String(var_name)
			if !endswith(lowercase(name), lowercase(extension))
				name = name * extension
			end

		#	Honour an Absolute Path
			if isabspath(name)
				return name
			end

		#	Validation
			if !isdir(directory)
				throw(ArgumentError("pajek_io: output directory does not exist: $directory"))
			end

		#	Assemble result
			return joinpath(directory, name)
	end

#	Helper Function for read_clu and read_vec: Header and Value Extraction
	function _read_pajek_values(file_path::AbstractString, extension::AbstractString)
		"""
		Args:
			file_path::AbstractString: path to a .clu or .vec file
			extension::AbstractString: expected extension, for the error message
		Returns:
			Vector{Int} or Vector{Float64}: the values, header removed
		Notes:
			CHANGE FROM Balikatan_2024, which called `deleteat!(v, 1)` and returned
			whatever followed. The header of a Pajek partition or vector declares a
			vertex count, and a file whose declared count disagrees with the number
			of values it carries is corrupt in a way that produces a silently
			misaligned partition rather than an error. That count is now checked.

			Blank trailing lines are dropped, since an editor may leave one and it
			would otherwise parse as a missing value.
		"""

		#	Validation
			if !isfile(file_path)
				throw(ArgumentError("pajek_io: $extension file not found: $file_path"))
			end

		#	Read and Separate the Header
			lines = readlines(file_path)
			if length(lines) < 2
				throw(ArgumentError("pajek_io: $extension file carries no values: $file_path"))
			end
			header = strip(lines[1])
			body   = String.(strip.(lines[2:end]))
			body   = body[.!isempty.(body)]

		#	Cross-Check the Declared Vertex Count
			declared = tryparse(Int, String(replace(header, r"^\s*\*[Vv]ertices\s+" => "")))
			if declared !== nothing && declared != length(body)
				throw(ArgumentError("pajek_io: header declares $declared vertices but " *
									"$(length(body)) values follow in $file_path"))
			end

		#	Assemble result
			return _parse_numeric(body)
	end

##########################
#   PUBLIC INTERFACE     #
##########################

#	Read a Pajek Network
	function read_net(net_file::AbstractString)
		"""
		Args:
			net_file::AbstractString: path to a .net file
		Returns:
			Tuple{DataFrame,DataFrame}: (nodes, edges) where nodes carries
			id::Int and label::String, and edges carries tie_type::String,
			person_i::Int, person_j::Int, and weight
		Notes:
			CHANGE FROM Balikatan_2024, which assumed exactly one metadata line
			followed by one arc block and sliced by arithmetic on the declared
			vertex count. Section markers are now located by scanning for lines
			beginning with an asterisk, so a file carrying a trailing *Partition or
			*Vector block does not silently fold that block into the edge list.

			Only the first arc or edge block is read. A file carrying both *Arcs and
			*Edges throws, because collapsing the two loses the distinction between
			directed and undirected ties.
		"""

		#	Validation
			if !isfile(net_file)
				throw(ArgumentError("pajek_io: network file not found: $net_file"))
			end

		#	Read and Locate the Section Markers
			net = readlines(net_file)
			star_ix = findall(l -> startswith(strip(l), "*"), net)
			if isempty(star_ix)
				throw(ArgumentError("pajek_io: no *Vertices header in $net_file"))
			end

		#	Validate the Vertex Header
			header = strip(net[star_ix[1]])
			if !occursin(r"^\*[Vv]ertices"i, header)
				throw(ArgumentError("pajek_io: first section is not *Vertices in $net_file"))
			end
			declared = tryparse(Int, String(replace(header, r"^\s*\*[Vv]ertices\s+" => "")))
			if declared === nothing
				throw(ArgumentError("pajek_io: cannot read the vertex count from: $header"))
			end

		#	Extract the Vertex Block
			v_stop = length(star_ix) > 1 ? star_ix[2] - 1 : length(net)
			nodes_data = String.(strip.(net[(star_ix[1] + 1):v_stop]))
			nodes_data = nodes_data[.!isempty.(nodes_data)]
			if length(nodes_data) != declared
				throw(ArgumentError("pajek_io: header declares $declared vertices but " *
									"$(length(nodes_data)) vertex lines follow in $net_file"))
			end
			nodes = _parse_node_line(nodes_data)

		#	Locate the Tie Block
			if length(star_ix) < 2
				throw(ArgumentError("pajek_io: no *Arcs or *Edges section in $net_file"))
			end
			tie_type = String(strip(net[star_ix[2]]))
			if !occursin(r"^\*(Arcs|Edges)"i, tie_type)
				throw(ArgumentError("pajek_io: second section is $tie_type, expected *Arcs or *Edges"))
			end

		#	Reject a Mixed Tie File
			later = [String(strip(net[i])) for i in star_ix[3:end]]
			mixed = findfirst(s -> occursin(r"^\*(Arcs|Edges)"i, s), later)
			if mixed !== nothing
				throw(ArgumentError("pajek_io: $net_file carries both $tie_type and " *
									"$(later[mixed]); read these separately"))
			end

		#	Extract the Tie Block
			e_stop = length(star_ix) > 2 ? star_ix[3] - 1 : length(net)
			edges_data = String.(strip.(net[(star_ix[2] + 1):e_stop]))
			edges_data = edges_data[.!isempty.(edges_data)]
			edges = _parse_edge_line(edges_data)

		#	Record the Tie Type
			insertcols!(edges, 1, :tie_type => fill(tie_type, nrow(edges)))

		#	Assemble result
			return (nodes, edges)
	end
	@doc raw"""
	**Description**
	Read a Pajek `.net` network file into a node table and an edge table. Handles
	the display attributes Pajek writes onto vertex and arc lines, and locates
	section boundaries rather than inferring them from the declared vertex count.

	**Usage**
	`read_net(net_file::AbstractString)`

	**Arguments**
	- `net_file::AbstractString`: Path to the `.net` file.

	**Details**
	A Pajek vertex line carries an index, a quoted label, and optional coordinates
	and display attributes; only the index and label are retained. An arc line
	carries a source, a target, an optional weight, and optional colour tokens;
	only the first three fields are read, and a line without a weight is given
	unit weight.

	Section markers are found by scanning for lines beginning with `*`, so a
	trailing `*Partition` or `*Vector` block is excluded from the edge list rather
	than parsed as ties. A file carrying both `*Arcs` and `*Edges` throws, since
	folding them together would lose the directed/undirected distinction.

	The declared vertex count is checked against the number of vertex lines
	present. A mismatch throws rather than producing a partially populated table.

	**Value**
	A `Tuple{DataFrame,DataFrame}`:
	- `nodes`: `id::Int` (the Pajek vertex index, 1-based and sequential),
	  `label::String` (the quoted label).
	- `edges`: `tie_type::String` (`"*Arcs"` or `"*Edges"`), `person_i::Int`,
	  `person_j::Int`, `weight`.

	Note that `id` is the Pajek index, not any identifier the label may encode. In
	this project the label holds the citation node id, so a caller wanting to work
	in node ids should read them from `label`.

	**Examples**
	```julia
		#	Read an era network
			nodes, edges = read_net("pajek_files/Era12/era12.net")
			println("$(nrow(nodes)) vertices, $(nrow(edges)) arcs")

		#	Labels carry the node id in this project
			node_ids = parse.(Int, nodes.label)
	```

	**See Also**
	`write_net`, `read_clu`, `read_vec`
	""" read_net

#	Write a Pajek Network
	function write_net(tie_type::AbstractString, data_id::DataFrame,
					   person_i::Vector{Int}, person_j::Vector{Int},
					   net_name::AbstractString;
					   directory::AbstractString = pwd(),
					   sort_simplify::Bool = true,
					   tie_weight::Union{Vector{Int},Vector{Float64},Nothing} = nothing,
					   x_coord::Union{Vector{Float64},Nothing} = nothing,
					   y_coord::Union{Vector{Float64},Nothing} = nothing,
					   z_coord::Union{Vector{Float64},Nothing} = nothing,
					   node_color::Union{AbstractString,Nothing} = nothing,
					   node_border::Union{AbstractString,Nothing} = nothing,
					   tie_color::Union{AbstractString,Nothing} = nothing)
		"""
		Args:
			tie_type::AbstractString: "edges" or "arcs", case insensitive
			data_id::DataFrame: node table; column 1 is the index, column 2 the label
			person_i::Vector{Int}: source vertex indices
			person_j::Vector{Int}: target vertex indices
			net_name::AbstractString: file name, with or without .net
			directory::AbstractString: destination directory (default = pwd())
			sort_simplify::Bool: sort and deduplicate the tie list (default = true)
			tie_weight: tie weights (default = unit weights)
			x_coord, y_coord, z_coord: vertex coordinates (default = blank)
			node_color, node_border, tie_color: display attributes (default = none)
		Returns:
			String: the path written
		Notes:
			CHANGE FROM Balikatan_2024: takes a directory and returns the path
			written, rather than opening a relative path and returning nothing.

			Line endings are CRLF, which is what Pajek itself writes.
		"""

		#	Validation
			if nrow(data_id) == 0
				throw(ArgumentError("pajek_io: node table is empty"))
			end
			if length(person_i) != length(person_j)
				throw(ArgumentError("pajek_io: source and target vectors differ in length"))
			end

		#	Normalize the Tie Type
			tie_header = lowercase(String(tie_type)) == "edges" ? "*Edges" : "*Arcs"

		#	Prepare Coordinates
			n_nodes = nrow(data_id)
			x = isnothing(x_coord) ? fill("", n_nodes) : string.(x_coord)
			y = isnothing(y_coord) ? fill("", n_nodes) : string.(y_coord)
			z = isnothing(z_coord) ? fill("", n_nodes) : string.(z_coord)

		#	Prepare Display Attributes
			ic = isnothing(node_color)  ? fill("", n_nodes) : fill("ic " * uppercasefirst(strip(String(node_color))), n_nodes)
			bc = isnothing(node_border) ? fill("", n_nodes) : fill("bc " * uppercasefirst(strip(String(node_border))), n_nodes)
			tc = isnothing(tie_color)   ? fill("", length(person_i)) : fill("c " * uppercasefirst(strip(String(tie_color))), length(person_i))

		#	Assemble the Tie List
			edge_list = DataFrame(sender = person_i, target = person_j,
								  weight = isnothing(tie_weight) ? fill(1.0, length(person_i)) : tie_weight)
			if sort_simplify
				#	Sort, Then Reduce to Distinct Ties
					edge_list = unique(sort(edge_list, [:sender, :target]))
					tc = tc[1:nrow(edge_list)]
			end

		#	Format the Vertex Block
			vertex_header = "*Vertices $(n_nodes)"
			node_lines = [string(data_id[i, 1], " \"", data_id[i, 2], "\" ",
								 x[i], " ", y[i], " ", z[i], " ", ic[i], " ", bc[i])
						  for i in 1:n_nodes]

		#	Format the Tie Block
			tie_lines = [string(edge_list[i, :sender], " ", edge_list[i, :target], " ",
								edge_list[i, :weight], " ", tc[i])
						 for i in 1:nrow(edge_list)]

		#	Write
			out_path = _pajek_output_path(net_name, ".net", directory)
			open(out_path, "w") do file
				write(file, join([vertex_header; node_lines], "\r\n"))
				write(file, "\r\n")
				write(file, join([tie_header; tie_lines], "\r\n"))
				write(file, "\r\n")
			end

		#	Assemble result
			return out_path
	end
	@doc raw"""
	**Description**
	Write a node table and tie list to a Pajek `.net` file, with optional
	coordinates and display attributes.

	**Usage**
	`write_net(tie_type, data_id, person_i, person_j, net_name; directory=pwd(), sort_simplify=true, tie_weight=nothing, x_coord=nothing, y_coord=nothing, z_coord=nothing, node_color=nothing, node_border=nothing, tie_color=nothing)`

	**Arguments**
	- `tie_type::AbstractString`: `"edges"` or `"arcs"`; anything other than
	  `"edges"` is written as `*Arcs`.
	- `data_id::DataFrame`: Node table. Column 1 supplies the vertex index and
	  column 2 the label; both are used positionally.
	- `person_i::Vector{Int}`, `person_j::Vector{Int}`: Tie endpoints, as vertex
	  indices into `data_id`.
	- `net_name::AbstractString`: File name, with or without the `.net` extension.
	- `directory::AbstractString`: Destination directory (default `pwd()`). An
	  absolute `net_name` overrides it.
	- `sort_simplify::Bool`: Sort ties by endpoint and drop duplicates
	  (default `true`).
	- `tie_weight`: Tie weights (default unit weights).
	- `x_coord`, `y_coord`, `z_coord`: Vertex coordinates (default blank).
	- `node_color`, `node_border`, `tie_color`: Display attributes applied
	  uniformly to every vertex or tie (default none).

	**Details**
	Line endings are CRLF, matching what Pajek writes. Vertex labels are quoted.
	Because `data_id` is read positionally, a node table whose first two columns
	are not index and label will produce a structurally valid but wrong file.

	**Value**
	The path written, as a `String`.

	**Examples**
	```julia
		#	Write a small directed network
			nodes = DataFrame(id = 1:3, label = ["a", "b", "c"])
			path  = write_net("arcs", nodes, [1, 2], [2, 3], "demo";
							  directory = tempdir())
	```

	**See Also**
	`read_net`, `write_clu`, `write_vec`
	""" write_net

#	Read a Pajek Partition
	function read_clu(clu_file::AbstractString)
		"""
		Args:
			clu_file::AbstractString: path to a .clu file
		Returns:
			Vector{Int}: community assignment per vertex, in vertex order
		Notes:
			CHANGE FROM Balikatan_2024: the declared vertex count in the header is
			checked against the number of assignments. A partition whose length
			disagrees with its header misaligns every downstream community, and it
			does so silently.
		"""

		#	Assemble result
			return _read_pajek_values(clu_file, ".clu")
	end
	@doc raw"""
	**Description**
	Read a Pajek `.clu` partition file into a vector of community assignments,
	one per vertex, in vertex order.

	**Usage**
	`read_clu(clu_file::AbstractString)`

	**Arguments**
	- `clu_file::AbstractString`: Path to the `.clu` file.

	**Details**
	The `*Vertices n` header is removed and its declared count checked against the
	number of values that follow; a mismatch throws. Blank trailing lines are
	dropped. Values are returned as `Int` when every one parses as an integer,
	which is the normal case for a partition.

	**Value**
	`Vector{Int}` of community labels, positionally aligned with the vertices of
	the corresponding `.net` file. Labels are not guaranteed contiguous.

	**Examples**
	```julia
		#	Read a partition and count communities
			partition = read_clu("pajek_files/era12_Community.clu")
			println("$(length(unique(partition))) communities over $(length(partition)) vertices")
	```

	**See Also**
	`write_clu`, `read_net`, `read_vec`
	""" read_clu

#	Write a Pajek Partition
	function write_clu(cat_variable::AbstractVector{<:Integer}, var_name::AbstractString;
					   directory::AbstractString = pwd())
		"""
		Args:
			cat_variable::AbstractVector{<:Integer}: community assignment per vertex
			var_name::AbstractString: file name, with or without .clu
			directory::AbstractString: destination directory (default = pwd())
		Returns:
			String: the path written
		Notes:
			CHANGE FROM Balikatan_2024: takes a directory, accepts any integer
			vector rather than Vector{Int64} specifically, and returns the path
			written. Widening the type matters because a Leiden or CHAMP membership
			arrives as Vector{Int}, which is Int64 only on 64-bit builds.

			Assignments are written in the order given, which must be the vertex
			order of the network the partition describes.
		"""

		#	Validation
			if isempty(cat_variable)
				throw(ArgumentError("pajek_io: partition is empty"))
			end

		#	Format
			pajek_lines = [string("*Vertices ", length(cat_variable)); string.(cat_variable)]

		#	Write
			out_path = _pajek_output_path(var_name, ".clu", directory)
			open(out_path, "w") do f
				for line in pajek_lines
					write(f, string(line, "\n"))
				end
			end

		#	Assemble result
			return out_path
	end
	@doc raw"""
	**Description**
	Write a vector of community assignments to a Pajek `.clu` partition file.

	**Usage**
	`write_clu(cat_variable, var_name; directory=pwd())`

	**Arguments**
	- `cat_variable::AbstractVector{<:Integer}`: Community assignment per vertex,
	  in the vertex order of the network the partition describes.
	- `var_name::AbstractString`: File name, with or without the `.clu` extension.
	- `directory::AbstractString`: Destination directory (default `pwd()`). An
	  absolute `var_name` overrides it.

	**Details**
	Writes a `*Vertices n` header followed by one assignment per line. Order is
	preserved exactly; the function cannot check that the order matches its
	network, so a caller that reorders a membership vector before writing will
	produce a valid file describing the wrong vertices.

	Accepts any integer vector, which matters because a Leiden or CHAMP membership
	arrives as `Vector{Int}`.

	**Value**
	The path written, as a `String`.

	**Examples**
	```julia
		#	Write a partition beside its network
			path = write_clu(membership, "era12_champ";
							 directory = "pajek_files/Era12")
	```

	**See Also**
	`read_clu`, `write_vec`, `write_net`
	""" write_clu

#	Read a Pajek Vector
	function read_vec(vector_file::AbstractString)
		"""
		Args:
			vector_file::AbstractString: path to a .vec file
		Returns:
			Vector{Int} or Vector{Float64}: one value per vertex, in vertex order
		Notes:
			Shares its implementation with read_clu; the two differ only in the
			extension reported on error and in whether the values are expected to
			be integral. Integer .vec files are returned as integers.
		"""

		#	Assemble result
			return _read_pajek_values(vector_file, ".vec")
	end
	@doc raw"""
	**Description**
	Read a Pajek `.vec` vector file into a numeric vector, one value per vertex,
	in vertex order.

	**Usage**
	`read_vec(vector_file::AbstractString)`

	**Arguments**
	- `vector_file::AbstractString`: Path to the `.vec` file.

	**Details**
	The `*Vertices n` header is removed and its declared count checked against the
	number of values; a mismatch throws. Values parse to `Int` when all are
	integral and `Float64` otherwise, so a degree vector returns as integers and a
	centrality vector as floats.

	**Value**
	`Vector{Int}` or `Vector{Float64}`, positionally aligned with the vertices of
	the corresponding `.net` file.

	**Examples**
	```julia
		#	Read a degree vector
			degrees = read_vec("pajek_files/Era12/era12_within_degree.vec")
	```

	**See Also**
	`write_vec`, `read_clu`, `read_net`
	""" read_vec

#	Write a Pajek Vector
	function write_vec(con_variable::AbstractVector{<:Real}, var_name::AbstractString;
					   directory::AbstractString = pwd())
		"""
		Args:
			con_variable::AbstractVector{<:Real}: one value per vertex
			var_name::AbstractString: file name, with or without .vec
			directory::AbstractString: destination directory (default = pwd())
		Returns:
			String: the path written
		Notes:
			CHANGE FROM Balikatan_2024, which carried one method for Vector{Int64}
			and another for Vector{Float64} with identical bodies. One method over
			AbstractVector{<:Real} covers both and also covers the integer widths a
			degree computation can produce.
		"""

		#	Validation
			if isempty(con_variable)
				throw(ArgumentError("pajek_io: vector is empty"))
			end

		#	Format
			pajek_lines = [string("*Vertices ", length(con_variable)); string.(con_variable)]

		#	Write
			out_path = _pajek_output_path(var_name, ".vec", directory)
			open(out_path, "w") do f
				for line in pajek_lines
					write(f, string(line, "\n"))
				end
			end

		#	Assemble result
			return out_path
	end
	@doc raw"""
	**Description**
	Write a numeric vector to a Pajek `.vec` file, one value per vertex.

	**Usage**
	`write_vec(con_variable, var_name; directory=pwd())`

	**Arguments**
	- `con_variable::AbstractVector{<:Real}`: One value per vertex, in the vertex
	  order of the network the vector describes.
	- `var_name::AbstractString`: File name, with or without the `.vec` extension.
	- `directory::AbstractString`: Destination directory (default `pwd()`). An
	  absolute `var_name` overrides it.

	**Details**
	Writes a `*Vertices n` header followed by one value per line. Integers are
	written without a decimal point and floats in Julia's default representation,
	so a value of `2.0` writes as `2.0` and `2` writes as `2`.

	A single method covers both integer and floating-point input.

	**Value**
	The path written, as a `String`.

	**Examples**
	```julia
		#	Write within-community degree beside its network
			path = write_vec(within_degree, "era12_within_degree";
							 directory = "pajek_files/Era12")
	```

	**See Also**
	`read_vec`, `write_clu`, `write_net`
	""" write_vec

#   Exporting Objects
    export read_net,
		   write_net,
           read_clu,
           write_clu,
           read_vec,
           write_vec