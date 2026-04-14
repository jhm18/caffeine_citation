# Era Pipeline 
# Sarah Delmar and Jon Morgan
# 29 Jan 2025

# To run Pajek from Visual Studio Code's Terminal: DISPLAY=:100.0 pajek

################
#### Notes  ####
################

### Pipeline Elements
# We are currently using articles, but we want to use their citations
# We want to look at the overlap in DOI_combined across communities
# Check about what network is looking at, go to edge list, compare edges not nodes

# 1. Build a function that extracts the links between communities as the ratio of time 2 to time 1 (T2:T1) (referencing backwards)
#   - ex. all ties between a community in T2 to all connected communities in T1 should sum to 1
# 2. Cluster the extracted links into communities (delta graph)
#   - colored by time
# 3. Perform Cluster Labeling Using ChatGPT for each Era(t)/Era(t-1) Network
# 4. Check logic (if bad, could adjust time window to be smaller)

# Next Steps
# 1. Create Community features based on Abstracts, Titles, and Keywords for Labeling

###################
#### Preamble  ####
###################

# Clear Out Console Script
  cat("\014")

# Options
  options(stringsAsFactors = FALSE)
  options(scipen = 999)

# Set Working Directory
  setwd("/workspace/caffeine_citation")

# Push changes
# docker exec -it caffeine /bin/bash

####################
#### Functions  ####
####################
  
# Source Pajek
  source("scripts/RPajekFunctions_30April2023.r")

# vertices <- era22_vertices
# network_partition <- era22_communities 
# Map from current vertices list to article and file id
  id_map <- function(node_list, vertices, era, network_partition){
    # Reduce node list  to current era
      curr_node_list <- node_list[node_list$era == era, ]
    
    # Map community to curr_node_list to make id_index using node_id/label
      vertices$community <- network_partition
      community_nodes <- vertices[c(1,2,6)]
      colnames(community_nodes)[c(1,2)] <- c("pajek_id","node_id")
      id_index <- dplyr::left_join(curr_node_list, community_nodes, by="node_id")
      id_index <- id_index[c(8,1,2,9,7)]
      
    # Returning ID Index
      return(id_index)
  }

# Pull out citations from each article in each era
# id_index <- t1_id_index
# vertices <- era22_vertices
  citation_finder <- function(node_list,edges, era1, era2){
    # Returns cited articles found in the current eras
    # Join era with edge list
      node_index <- node_list[c(1,7)]
      colnames(node_index)[[1]] <- c("sender_id")
      citation_index <- dplyr::left_join(edges, node_index, by='sender_id')
    
    # Define edge list for each era
      citation_index1 <- citation_index[citation_index$era == era1, ]
      citation_index2 <- citation_index[citation_index$era == era2, ]
      colnames(citation_index1)[[5]] <- c("era_1")
      colnames(citation_index2)[[5]] <- c("era_2")
    
    # Make target_id unique in each era
      citation_index1 <- citation_index1[!duplicated(citation_index1$target_id), ]
      citation_index2 <- citation_index2[!duplicated(citation_index2$target_id), ]
    
    # Identify shared citations
      cited_index <- dplyr::left_join(citation_index2, citation_index1[c(3,5)], by='target_id')
      cited_index <- cited_index[!is.na(cited_index$era_1),]
    
    # Returning Cited Index
      return(cited_index)
  }

# Extract links between communities
  community_membership <- function(t1_id_index, t2_id_index, cited_index){
    # Make a shared index for citations that span eras
      shared_index <- cited_index[c(1:4)]

    # Create string that combines sender and target
      pair_id <- paste0(shared_index$sender_id, "_", shared_index$target_id)
      shared_index$pair_id <- seq(1,length(pair_id),1)

    # Create sender and target label datasets to join with shared index
      sender_labels <- shared_index[c('pair_id', 'sender_label')]
      target_labels <- shared_index[c('pair_id', 'target_label')]

    # Rename doi column
      colnames(sender_labels)[[2]] <- c('node_label')
      colnames(target_labels)[[2]] <- c('node_label')

    # Join by doi; target list ends up with NA values because some articles are not within era  
      sender_labels <- dplyr::left_join(sender_labels,t2_id_index[c('node_label','community')], by='node_label')
      target_labels <- dplyr::left_join(target_labels, t1_id_index[c('node_label','community')], by='node_label')

    # Rename community columns by 1 and 2
      colnames(sender_labels)[c(2,3)] <- c('sender_doi','sender_community')
      colnames(target_labels)[c(2,3)] <- c('target_doi','target_community')

    # Join senders and targets by pair id
      shared_edges <- dplyr::left_join(sender_labels, target_labels, by = 'pair_id')
      shared_edges$keep <- shared_edges$sender_community*shared_edges$target_community
      shared_edges <- shared_edges[(is.na(shared_edges$keep) == FALSE),]
      shared_edges <- shared_edges[c(2:5)]

    # Make a pair id based on community
      shared_edges <- shared_edges[order(shared_edges$sender_community,shared_edges$target_community),]
      shared_edges$pair_id <- paste0(shared_edges$sender_community, "_", shared_edges$target_community)

    # Aggregate the row counts for each unique pair 
      result <- aggregate(x = list(count = shared_edges$pair_id), by = list(pair_id = shared_edges$pair_id),FUN = length)
      edges_index <- shared_edges[!duplicated(shared_edges$pair_id),]
      result <- dplyr::left_join(result, edges_index, by='pair_id')
      result <- result[c('pair_id','sender_community','target_community','count')]
    
    # Calculate proportion for each first element
      first_elements_list <- unique(result$sender_community)
      proportion <- vector("list", length = length(first_elements_list))
      for(i in seq_along(first_elements_list)){
        curr_element <- result[result$sender_community == first_elements_list[[i]], ]
        curr_sum <- sum(curr_element$count)
        curr_element$proportion <- curr_element$count/curr_sum
        curr_element <- curr_element[order(curr_element$proportion, decreasing = TRUE),]
        proportion[[i]] <- curr_element
      }
      result2 <- do.call("rbind", proportion)
    
    # Return Community Memberships
      return(result2)
  }

# Create node and edge list of communities that span eras for Pajek
  community_era_net <- function(eras_edges, sender_themes, target_themes){
    # Sort eras edges by sender, target
      eras_ties <- eras_edges[order(eras_edges$sender_community, eras_edges$target_community), ]

    # Label communities with target or sender for node list
      eras_ties$sender_id <- paste0("sender_", eras_ties$sender_community)
      eras_ties$target_id <- paste0("target_", eras_ties$target_community)
      sender_nodes <- data.frame(sender_community = eras_ties$sender_community, sender_id = eras_ties$sender_id)
      sender_nodes <- sender_nodes[order(sender_nodes$sender_community), ]
      target_nodes <- data.frame(target_community = eras_ties$target_community, target_id = eras_ties$target_id)
      target_nodes <- target_nodes[order(target_nodes$target_community), ]

    # Create node list of communities
      nodes <- unique(c(sender_nodes$sender_id, target_nodes$target_id))
      community_nodes <- data.frame(id = seq(1,length(nodes), 1), label = nodes)
    
      community_edges <- eras_ties
      community_edges$obs_id <- seq(1,nrow(eras_ties),1) 
    
    # Create sender and target lists
      sender_edges <- community_edges[c(8,2,6)]
      colnames(sender_edges)[3] <- c("label")
      sender_edges <- dplyr::left_join(sender_edges, community_nodes, by = "label")
      sender_edges <- sender_edges[c(1,4,2,3)]
      colnames(sender_edges)[c(2,4)] <- c("sender_id", "sender_label")
    
      target_edges <- community_edges[c(8,3,7,4,5)]
      colnames(target_edges)[3] <- c("label")
      target_edges <- dplyr::left_join(target_edges, community_nodes, by = "label")
      target_edges <- target_edges[c(1,6,2,3,4,5)]
      colnames(target_edges)[c(2,4)] <- c("target_id", "target_label")

    # Relabel columns to add themes from Olama csv output
      colnames(sender_themes)[c(1,2)] <- c("sender_community", "sender_name")
      colnames(target_themes)[c(1,2)] <- c("target_community", "target_name")
      sender_edges <- dplyr::left_join(sender_edges, sender_themes[c(1,2)], by="sender_community")
      target_edges <- dplyr::left_join(target_edges, target_themes[c(1,2)], by="target_community")
      target_edges <- target_edges[c(1:4,7,5,6)]
    
    # Join lists by obs id to create edge list for communities
      cluster_edges <- dplyr::left_join(sender_edges, target_edges, by="obs_id")

    # Remake node list with themes
      sender_nodes <- sender_edges[c(2,4,5)]
      colnames(sender_nodes) <- c("node_id", "label", "name")
      target_nodes <- target_edges[c(2,4,5)]
      colnames(target_nodes) <- c("node_id", "label", "name")
      node_list <- rbind(sender_nodes, target_nodes)
      node_list <- node_list[!duplicated(node_list$node_id),]
      node_list <- node_list[order(node_list$node_id),]

    # Reduce edge list
      community_edges <- cluster_edges[c(2,6,10,11)]

      community_list <- list(community_nodes = node_list, community_edges = community_edges)
    
    # Return Sequential Network List Objects
      return(community_list)
  }

# For a given era, pull out doi, abstract, title, keywords for all articles
  era_article_info <- function(article_combined, era, file_outputs){
    # Subset by era
      era_data <- article_combined[is.na(article_combined$era) == FALSE, ]
      era_data <- era_data[era_data$era == era, ]
      
    # Subset by file id
      file_ids <- unique(era_data$file_id)
      era_files <- file_outputs[c(file_ids)]
      
    # Create output for each file that holds title, abstract, keywords
      era_info <- vector("list", length = length(era_files))
      for(i in seq_along(file_ids)){
        # Subset era_data by file and era_files by file
          article_files <- unique(era_data[era_data$file_id == file_ids[[i]], 3])
          era_file <- era_files[[i]]
          
          # Create output list to hold info for each article
            title_list <- vector("character", length(article_files))
            abstract_list <- vector("character", length(article_files))
            keyword_list <- vector("character", length(article_files))
            doi_list <- vector("character", length(article_files))
            
          # Loop through each article to pull info from era_files
            for(j in seq_along(article_files)){
              
              # In article = 1, j = 16 and 17 are wrong - the j indexing is wrong!!
              era_article <- era_file[[article_files[[j]]]]
              
              # Pull doi
                article_doi <- unique(era_data[era_data$file_id == file_ids[[i]] & 
                                                  era_data$article_id == article_files[[j]],"source_combined"])
                doi_list[[j]] <- article_doi
                
              # Manipulate title
                article_title <- unlist(era_article$TI)
                article_title <- article_title[-c(1)]
                article_title <- paste(article_title, collapse = " ")
                title_list[[j]] <- article_title
                
              # Manipulate keywords
                article_keywords <- unlist(era_article$ID)
                article_keywords <- article_keywords[-c(1)]
                article_keywords <- paste(article_keywords, collapse = " ")
                article_keywords <- strsplit(article_keywords, ";")[[1]]
                article_keywords <- trim(article_keywords)
                article_keywords <- paste(article_keywords, collapse = ",")
                keyword_list[[j]] <- article_keywords
                
              # Manipulate abstract
                article_abstract <- unlist(era_article$AB)
                article_abstract <- article_abstract[-c(1)]
                article_abstract <- paste(article_abstract, collapse = " ")
                abstract_list[[j]] <- article_abstract
            }
            
            era_info[[i]] <- data.frame(file_id = file_ids[[i]], article_id = article_files, doi = doi_list, title = title_list,
                                        abstract_list = abstract_list, keywords = keyword_list)
      }
      
    # Stack all output lists
      era_info <- do.call("rbind", era_info)
    
      return(era_info)
  }

# For a given era, merge era#_data with node list to get node_id
  node_identifier <- function(era_df, era, node_list){
    # Subset node list to current era
      curr_node_list <- node_list[node_list$era == era, ]
      
    # Rename columns to match df
      colnames(curr_node_list)[[2]] <- c("doi")
      
    # Merge df with node list
      era_df <- dplyr::left_join(era_df,curr_node_list[c(1,2)], by="doi")
      era_df <- era_df[c(1,2,7,3:6)]
      
      return(era_df)
  }

# For a given era, add in community ids
  community_identifier <- function(era_df, id_index){
    # Merge era_df with community id
      era_df <- dplyr::left_join(era_df, id_index[c(2,4)], by="node_id")
      era_df <- era_df[c(1:3,8,4:7)]
      
    # Sort by community
      era_df <- era_df[order(era_df$community),]
    
      return(era_df)
  }
  
##################
#### Packages ####
##################

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
