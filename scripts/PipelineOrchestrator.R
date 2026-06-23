## Pipeline Orchestrator ##
# Sarah Delmar and Jonathan Morgan, 9 June 2026

# This is the wrapper function for the community identification and labeling pipeline

###################
###     Notes   ###
###################
# Add parallelization after completing initial draft of wrapper
# Add CHAMP community detection

##########################
#####     Import      ####
##########################

  load("data/citation_node_list_3Oct2024.Rda")
  load("data/article_combinedv2.Rda")
  load("data/citation_edge_list_21Mar2024.Rda")
  load("data/caffeine_articles_20April2021.Rda")

########################
###     Functions    ###
########################
  setwd("/workspace/caffeine_citation/scripts")

# Source Pipeline functions

  source("/workspace/caffeine_citation/scripts/EraPipeline.R")
  source("/workspace/caffeine_citation/scripts/Labeling_Communities_Ollama.R")

# Pipeline Wrapper
#
  era1 = 22
  era2 = 23
  era1_dir = "/workspace/caffeine_citation/pajek_files/Era22"
  era2_dir = "/workspace/caffeine_citation/pajek_files/Era23"
  citation_net1 = "era22.net"
  citation_net2 = "era23.net"
  comm1 = "era22_testCommunity"
  comm2 = "era23_testCommunity"
  runPipeline <- function(era1, era2, era1_dir, era2_dir, citation_net1,citation_net2, comm1,comm2, file_outputs){
# Read in files from Pajek

  # Era 2
    setwd(era2_dir)  
    read_clu(getwd(), comm2)
    read_net(citation_net2)
    era2_vertices <- vertices
    era2_edges <- ties
    era2_communities <- network_partition

  # Era 1
    setwd(era1_dir)  
    read_clu(getwd(), comm1)
    read_net(citation_net1)
    era1_vertices <- vertices
    era1_edges <- ties
    era1_communities <- network_partition
    rm(network_partition, vertices, ties)

    # Era Pipeline functions
    # Map community id to file/article id
      era2_id_index <- id_map(node_list, era2_vertices, era2, era2_communities)
      era1_id_index <- id_map(node_list, era1_vertices, era1, era1_communities)

    # Return cited articles found in the current eras
      cited_index <- citation_finder(node_list, edges, era1, era2)

    # Return longitudinal edge list with community from both eras sorted by proportion
      eras_edges <- community_membership(era1_id_index, era2_id_index, cited_index)

    # Create a list of abstract/title/keywords for all articles in a given era
      era1_data <- era_article_info(article_combined, era1, file_outputs)
      era2_data <- era_article_info(article_combined, era2, file_outputs)

    # Add in node_id  
      era1_data <- node_identifier(era1_data, era1, node_list)
      era2_data <- node_identifier(era2_data, era2, node_list)

    # Add in community_id and sort by community
      era1_data <- community_identifier(era1_data, era1_id_index)
      era2_data <- community_identifier(era2_data, era2_id_index)

    # Currently not saving out data - come back here if we want to change that

    # Labeling Era1 Communities with Ollama
      # Formatting data (May Back Keywords and Title Later)
        era1_prompt <- era1_data[c(4,6:8)]
        colnames(era1_prompt)[[1]] <- c("community_id")
        community_data <- data.frame(community_id = era1_prompt$community_id, text_theme = as.character(era1_prompt$abstract_list))
        
      # Generating Cluster Degree Rankings for the Purpose of Prompt Weighting
        network_path <- paste0(era1_dir, "/", citation_net1)
        partition_path <- paste0(era1_dir, "/", comm1, ".clu")
        output_dir <- paste0(era1_dir, "/Community_Degree_Files")
        mcr_file_path <- paste0(era1_dir, "/net1.MCR")
        write_pajek_mcr(network_path,  partition_path,  output_dir,  mcr_file_path)
    
      # Mapping Community Degrees to Prompt Data  
      ##################
      ## Add sending Pajek the file after writing mcr, start here next
      ###########  
        era1_prompt <- community_degree_mapper(network_path,partition_path,era1_prompt, output_dir)
    
#   Generating Community Labels & Exporting Era 22 Results
    era22_results <- generate_community_themes(era22_prompt, core_threshold = 8000, max_timeout = 1200,
                                          model = "llama3.1:8b", fallback_timeout = 900,     # 15 minutes
                                          cooldown_seconds = 30)
    readr::write_csv(era22_results, file=c("/workspace/caffeine_citation/data/era22_results.csv"))


    #Labeling_Communities_Ollama: write_pajek_mcr()
    #Labeling_Communities_Ollama: community_degree_mapper()
    #Labeling_Communities_Ollama: generate_community_themes()
    #Era Pipeline: community_era_net()
    #Era Pipeline: write_clu()
    #Era Pipeline: write_net()


  }
