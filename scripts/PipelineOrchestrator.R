## Pipeline Orchestrator ##
# Sarah Delmar and Jonathan Morgan, 9 June 2026

# This is the wrapper function for the community identification and labeling pipeline

###################
###     Notes   ###
###################
# Add parallelization after completing initial draft of wrapper
# Add CHAMP community detection

########################
###     Functions    ###
########################
  setwd("/workspace/caffeine_citation/scripts")

# Source Pipeline functions

  source("/workspace/caffeine_citation/scripts/EraPipeline.R")
  source("/workspace/caffeine_citation/scripts/Labeling_Communities_Ollama.R")

# Pipeline Wrapper
  runPipeline <- function(citation_net1,citation_net2, comm1,comm2, file_outputs){

    #Era Pipeline:  id_map()
    #Era Pipeline: citation_finder()
    #Era Pipeline: community_membership()
    #Era Pipeline: era_article_info()
    #Era Pipeline: node_identifier()
    #Era Pipeline: community_identifier()
    #Labeling_Communities_Ollama: write_pajek_mcr()
    #Labeling_Communities_Ollama: community_degree_mapper()
    #Labeling_Communities_Ollama: generate_community_themes()
    #Era Pipeline: community_era_net()
    #Era Pipeline: write_clu()
    #Era Pipeline: write_net()


  }
