# Era Pipeline 
# Sarah Delmar and Jon Morgan
# 29 Jan 2025

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
      shared_index <- cited_index[c(1,3,4)]
      colnames(shared_index)[[1]] <- c("pajek_id")
    
    # Make unique senders list
    # shared_senders <- data.frame(pajek_id =unique(shared_index$pajek_id))
    # shared_senders <- dplyr::left_join(shared_senders, t1_id_index[c(1,4,5)], by="pajek_id")
    # Add community to cited index for each era
      shared_index <- dplyr::left_join(shared_index, t1_id_index[c(1,4,5)], by="pajek_id")
      colnames(shared_index)[c(4,5)] <- c("community1", "era1")
      shared_index <- dplyr::left_join(shared_index, t2_id_index[c(1,4,5)], by="pajek_id")
      colnames(shared_index)[c(6,7)] <- c("community2", "era2")
      
    # Aggregate unique community pairs from cols community1 and community2
      shared_index$pair <- apply(shared_index[, c("community1", "community2")], 1, function(x) paste(sort(x), collapse = "-"))      
    
    # Aggregate the sum by unique pairs 
      shared_index$count <- 1 
      
    # Aggregate the row counts for each unique pair 
      result <- aggregate(count ~ pair, data = shared_index, sum)
      result_pairs <- strsplit(result$pair, "-")
      
    # First element of each pair 
      first_elements <- sapply(result_pairs, `[`, 1) 
      
    # Second element of each pair
      second_elements <- sapply(result_pairs, `[`, 2)
    
      result <- cbind(result,first_elements,second_elements)
      result$first_elements <- as.integer(result$first_elements)
      result$second_elements <- as.integer(result$second_elements)
      result <- result[order(result$first_elements,result$second_elements), ]
    
    # Calculate proportion for each first element
      first_elements_list <- unique(result$first_elements)
      proportion <- vector("list", length = length(first_elements_list))
      for(i in seq_along(first_elements_list)){
        curr_element <- result[result$first_elements == first_elements_list[[i]], ]
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
  community_era_net <- function(eras_edges){
    # Create node list of communities
      nodes <- sort(unique(c(eras_edges$first_elements, eras_edges$second_elements)))
      community_nodes <- data.frame(id = seq(1,length(nodes), 1), label = nodes)
    
      community_edges <- eras_edges
      community_edges$obs_id <- seq(1,nrow(eras_edges),1) 
    
    # Create sender and target lists
      sender_edges <- community_edges[c(6,3)]
      colnames(sender_edges)[2] <- c("label")
      sender_edges <- dplyr::left_join(sender_edges, community_nodes, by = "label")
      sender_edges <- sender_edges[c(1,3,2)]
      colnames(sender_edges)[c(2,3)] <- c("sender_id", "sender_label")
    
      target_edges <- community_edges[c(6,4,2,5)]
      colnames(target_edges)[2] <- c("label")
      target_edges <- dplyr::left_join(target_edges, community_nodes, by = "label")
      target_edges <- target_edges[c(1,5,2,3,4)]
      colnames(target_edges)[c(2,3)] <- c("target_id", "target_label")
    
    # Join lists by obs id to create edge list for communities
      cluster_edges <- dplyr::left_join(sender_edges, target_edges, by="obs_id")
      community_list <- list(community_nodes = community_nodes, community_edges = cluster_edges)
    
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

# Label communities with their respective era (for t1 and t2)
  eras_first_elements <- paste0("era1_",eras_edges$first_elements)
  eras_second_elements <- paste0("era2_", eras_edges$second_elements)
  eras_nodes <- unique(c(eras_first_elements, eras_second_elements))

# Write a function that creates a pajek node list and edge list with ID
# We need to create sequential IDs that we map to the community edgelist (era_edges), 
# since we are creating a network from Era23 to Era22.
# Creating function, move to function section later
  era_community_list <- community_era_net(eras_edges)
  
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
  
  setwd("/workspace/caffeine_citation/pajek_files/Era22")
#######################
## Labeling Networks ##
#######################
load(file="/workspace/caffeine_citation/data/era22_prompt.Rda")
load(file="/workspace/caffeine_citation/data/era23_prompt.Rda")
era22_themes <- readr::read_csv(file="/workspace/caffeine_citation/data/era22_results.csv")
era23_themes <- readr::read_csv(file="/workspace/caffeine_citation/data/era23_results.csv")

first_elements_df <- eras_edges[c(1,3)]
first_themes <- era22_themes[c(1,2)]
colnames(first_elements_df)[c(2)] <- c("community_id")

first_elements_df <- dplyr::left_join(first_elements_df,first_themes, by="community_id")

second_elements_df <- eras_edges[c(1,4)]
second_themes <- era23_themes[c(1,2)]
colnames(second_elements_df)[c(2)] <- c("community_id")

second_elements_df <- dplyr::left_join(second_elements_df,second_themes, by="community_id")

######NOTES#######
# Investigate the community ids for the joined network vs era22 and era23 prompt data 
# look in community_identifier function 


############################
## Create Pajek Networks  ##
############################

# Writ-Out to Pajek
  write_net('Arcs', era_community_list$community_nodes$label, '', '', '', 'blue', 'white', 
            era_community_list$community_edges$sender_id, era_community_list$community_edges$target_id,
            era_community_list$community_edges$proportion, 'gray', 'Era_community_22_23', TRUE)
