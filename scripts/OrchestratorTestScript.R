#################
####  Import ####
#################

load("data/citation_node_list_3Oct2024.Rda")
load("data/article_combinedv2.Rda")
load("data/citation_edge_list_21Mar2024.Rda")
#load("data/caffeine_articles_20April2021.Rda")

# Read in files from Pajek

# Era 23
  setwd("/workspace/caffeine_citation/pajek_files/Era23")  
  read_clu(getwd(), "era23_testCommunity")
  read_net("era23.net")
  era23_vertices <- vertices
  era23_edges <- ties
  era23_communities <- network_partition

# Era 22
  setwd("/workspace/caffeine_citation/pajek_files/Era22")  
  read_clu(getwd(), "era22_testCommunity")
  read_net("era22.net")
  era22_vertices <- vertices
  era22_edges <- ties
  era22_communities <- network_partition
  rm(network_partition, vertices, ties)

###########################
####  Analysis Steps   ####
###########################

# Map community id to file/article id
  era23_id_index <- id_map(node_list, era23_vertices, 23, era23_communities)
  era22_id_index <- id_map(node_list, era22_vertices, 22, era22_communities)

# Return cited articles found in the current eras
  cited_index <- citation_finder(node_list, edges, 22, 23)

# Return longitudinal edge list with community from both eras sorted by proportion
  eras_edges <- community_membership(era22_id_index, era23_id_index, cited_index)
  
# Create a list of abstract/title/keywords for all articles in a given era
  era22_data <- era_article_info(article_combined, 22, file_outputs)
  era23_data <- era_article_info(article_combined, 23, file_outputs)

# Add in node_id  
  era22_data <- node_identifier(era22_data, 22, node_list)
  era23_data <- node_identifier(era23_data, 23, node_list)
  
# Add in community_id and sort by community
  era22_data <- community_identifier(era22_data, era22_id_index)
  era23_data <- community_identifier(era23_data, era23_id_index)

# Saving data
  setwd("/workspace/caffeine_citation/data")
  save(era22_data, file="era22_prompt.Rda")
  save(era23_data, file="era23_prompt.Rda")

##############################
#   IMPORTING CLUSTER DATA   #
##############################

#   ERA 22

#   Loading Era 22 Prompt Data
    load("/workspace/caffeine_citation/data/era22_prompt.Rda")

#   Formatting data (May Back Keywords and Title Later)
    era22_prompt <- era22_data[c(4,6:8)]
    colnames(era22_prompt)[[1]] <- c("community_id")
    community_data <- data.frame(community_id = era22_prompt$community_id, text_theme = as.character(era22_prompt$abstract_list))

#   Pulling-Results
    era_22_results <- readr::read_csv("/workspace/caffeine_citation/data/era22_results.csv")
    

#   ERA 23

#   Loading Era 23 Prompt Data
    load("/workspace/caffeine_citation/data/era23_prompt.Rda")

#   Formatting data (May Back Keywords and Title Later)
    era23_prompt <- era23_data[c(4,6:8)]
    colnames(era23_prompt)[[1]] <- c("community_id")
    era_23_community_data <- data.frame(community_id = era23_prompt$community_id, text_theme = as.character(era23_prompt$abstract_list))

#   Pulling-Results
    era_23_results <- readr::read_csv("/workspace/caffeine_citation/data/era23_results.csv")

##########################
#   COMPARING CLUSTERS   #
##########################
#   ERA 22

#   Generating Cluster Degree Rankings for the Purpose of Prompt Weighting
    network_path <- c('/workspace/caffeine_citation/pajek_files/Era22/era22.net')
    partition_path <- c('/workspace/caffeine_citation/pajek_files/Era22/era22_testCommunity.clu')
    output_dir <- c('/workspace/caffeine_citation/pajek_files/Era22/Community_Degree_Files')
    mcr_file_path <- c('/workspace/caffeine_citation/pajek_files/Era22/test_2.MCR')
#   write_pajek_mcr(network_path,  partition_path,  output_dir,  mcr_file_path)
    
#   Mapping Community Degrees to Prompt Data    
    era_prompt <- "/workspace/caffeine_citation/data/era22_prompt.Rda"
    degree_file_loc <- "/workspace/caffeine_citation/pajek_files/Era22/Community_Degree_Files"
    era22_prompt <- community_degree_mapper(network_path,partition_path,era_prompt, degree_file_loc)
    
#   Generating Community Labels & Exporting Era 22 Results
    era22_results <- generate_community_themes(era22_prompt, core_threshold = 8000, max_timeout = 1200,
                                          model = "llama3.1:8b", fallback_timeout = 900,     # 15 minutes
                                          cooldown_seconds = 30)
    readr::write_csv(era22_results, file=c("/workspace/caffeine_citation/data/era22_results.csv"))

#   ERA 23

#   Generating Cluster Degree Rankings for the Purpose of Prompt Weighting
    network_path <- c('/workspace/caffeine_citation/pajek_files/Era23/era23.net')
    partition_path <- c('/workspace/caffeine_citation/pajek_files/Era23/era23_testCommunity.clu')
    output_dir <- c('/workspace/caffeine_citation/pajek_files/Era23/Community_Degree_Files')
    mcr_file_path <- c('/workspace/caffeine_citation/pajek_files/Era23/test_2.MCR')
    #write_pajek_mcr(network_path,  partition_path,  output_dir,  mcr_file_path)
    
#   Mapping Community Degrees to Prompt Data    
    era_prompt <- "/workspace/caffeine_citation/data/era23_prompt.Rda"
    degree_file_loc <- "/workspace/caffeine_citation/pajek_files/Era23/Community_Degree_Files"
    era23_prompt <- community_degree_mapper(network_path,partition_path,era_prompt, degree_file_loc)
    
#   Generating Community Labels & Exporting Era 23 Results
    era23_results <- generate_community_themes(era23_prompt, core_threshold = 8000, max_timeout = 1200,
                                          model = "llama3.1:8b", fallback_timeout = 900,     # 15 minutes
                                          cooldown_seconds = 30)
    readr::write_csv(era23_results, file=c("/workspace/caffeine_citation/data/era23_results.csv"))

##############################################################
#####                                                     ####
#####           Constructing Community Network            ####
#####                                                     ####
##############################################################

# Write a function that creates a pajek node list and edge list with ID
# We need to create sequential IDs that we map to the community edgelist (era_edges), 
# since we are creating a network from Era23 to Era22.
  era22_themes <- readr::read_csv(file="/workspace/caffeine_citation/data/era22_results.csv")
  era23_themes <- readr::read_csv(file="/workspace/caffeine_citation/data/era23_results.csv")

  era_community_list <- community_era_net(eras_edges, era23_themes, era22_themes)
  
# Stack era_community_list
  community_nodes <- era_community_list$community_nodes
  community_edges <- era_community_list$community_edges

# Write .clu file to label sender vs target for our node list
  role <- strsplit(community_nodes$label, "_")
  role <- unlist(lapply(role, function(x) x[[1]]))
  community_role <- ifelse(role == "sender", 1,2)
  write_clu(community_role,"community_role")

# Writ-Out to Pajek
  write_net('Arcs', era_community_list$community_nodes$name, '', '', '', 'blue', 'white', 
            era_community_list$community_edges$sender_id, era_community_list$community_edges$target_id,
            era_community_list$community_edges$proportion, 'gray', 'Era_community_22_23', TRUE)

##########################
#   ORCHESTRATOR TESTS   #
########################## 
          

###################
#   EVALUATIONS   #
###################

#   Pulling-In Community & Theme Data
    community_themes <- community_abstracts_finder(era23_data, 9, era_23_results)

#   Collapsing Abstracts by Cluster
    community_abstracts <- prepare_community_data(era22_prompt)
    print(community_abstracts$core_themes[(1:5),])
    print(community_abstracts$minor_themes[(1:5),])

#######################
#   FUNCTION CHECKS   #
#######################

#   Testing that stop_ollama() works
    stop_ollama()

#   Testing if I can start ollama
    ensure_ollama_running()

#   Testing that if Ollama is Running that ensure_ollama_running() Returns the Correct Value
    ensure_ollama_running()
    
#   Testing C Path Conversion function
    test_path <- getwd()
    c_path <- .make_c_paths(test_path)
    print(c_path)
    
#   Testing MCR Generator

