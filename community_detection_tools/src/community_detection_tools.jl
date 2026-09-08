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

#   Gamma Test Functions go here.

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
		   adjusted_rand_index

end # module community_detection_tools
