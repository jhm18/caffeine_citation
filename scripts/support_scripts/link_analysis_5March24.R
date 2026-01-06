# Link Analysis
# Jonathan Morgan & Sarah Ricupero
# 5 March 2024

#Source info: https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/

# Clear Out Console Script
  cat("\014")

# Set Working Directory
  if (paste(Sys.info()[[1]], collapse=" ") == "Windows"){
  setwd("~/CaffeineSNA")
}else{
  setwd("/mnt/d/Dropbox/ACS/Caffeine_CitationAnalysis/Caffeine_SNA_Files Oct27")
  getwd()
}

# Options
  options(scipen=999)

################
#   PACKAGES   #
################


###################
#    FUNCTIONS    #
###################

# Utility Functions
  '%notin%' <- Negate('%in%') 
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Sourcing Pajek Functions
  if (paste(Sys.info()[[1]], collapse=" ") == "Windows"){
  source("RPajekFunctions_30April2023.R")
}else{
  source("/mnt/c/Users/metal/Pajek_Resources/Pajek R Tools/RPajekFunctions.R")
}
  
# Create ids for all nodes
  nodelist_maker <- function(article_combined){
    # Create dataframe with article ids and source strings
      senders <- article_combined[c(1,7)]
      senders <- senders[!duplicated(senders$id), ]
      senders <- senders[!duplicated(senders$source_combined), ]
      senders$seq_id <- seq(1,nrow(senders), 1)
      senders <- senders[c(3,2)]
      colnames(senders) <- c("node_id", "node_label")
      senders$role <- c("sender")
      
      # Used to ensure repeated source_combined with different ids could be collapsed
      # Find repeats in source_combined and make unique
      #test <- article_combined[article_combined$id %notin% senders$id, ]
      #test <- test[!duplicated(test$id), ]
      
    # Create target list
      targets <- unique(article_combined$DOI_combined)
      targets <- data.frame(node_id= seq(34975, 34974+length(targets), 1),node_label = targets)
      targets$role <- c("target")
    
    # Stack to make node list
      node_list <- rbind(senders, targets)
      
    # Get rid of duplicates of node_label
      node_list <- node_list[!duplicated(node_list$node_label), ]
      node_list$node_id <- seq(1, nrow(node_list), 1)
      row.names(node_list) <- seq(1, nrow(node_list), 1)
      
      return(node_list)
  }
  
# Create edges
  edgelist_make <- function(node_list, article_combined){
    # Create output list
    edge_list <- vector("list", length = nrow(node_list))
    names(edge_list) <- node_list$node_id
    for(i in seq_along(node_list$node_id)){
      # Identify the node, and subset to get the ego network
      current_node <- node_list$node_label[[i]]
      ego_net <- article_combined[article_combined$source_combined == current_node | article_combined$DOI_combined == current_node,c(7,9)]
      ego_net$obs_id <- seq(1, nrow(ego_net), 1)
            
      # Create senders and targets columns by merging 
      senders <- ego_net[c(1,3)]
      colnames(senders)[1] <- c("node_label")
      senders <- dplyr::left_join(senders, node_list[c(1,2)], by = "node_label")
      colnames(senders)[c(1,3)] <- c("sender_label", "sender_id")
      senders <- senders[c(2,3,1)]
      
      targets <- ego_net[c(2,3)]
      colnames(targets)[1] <- c("node_label")
      targets <- dplyr::left_join(targets, node_list[c(1,2)], by = "node_label")
      colnames(targets)[c(1,3)] <- c("target_label", "target_id")
      targets <- targets[c(2,3,1)]
      
      # Merge targets and senders into a single edge list
      ego_edges <- dplyr::left_join(senders, targets, by = "obs_id")
      ego_edges <- ego_edges[-c(1)]
      
      edge_list[[i]] <- ego_edges
    }
    
    edges <- do.call("rbind", edge_list)
    return(edges)
  }

  # Make an author - community list so that we can collapse articles, a community list that matches to the node id list
  # Take the first author of each node (go back to dataset with element1), make connections between first authors
  
####################
#   IMPORT FILES   #
####################
  load("article_combined_March5.Rda")
  load("caffeine_authors_df_28April2021.Rda")
  
###################
#  LINK ANALYSIS  #
###################
  
# Create node list and edge list
  node_list <- nodelist_maker(article_combined)
  
  edges <- edgelist_make(node_list, article_combined)
  
  save(edges, file = "citation_edge_list_21Mar2024.Rda")
  load("citation_edge_list_21Mar2024.Rda")
  
# Create author partition
# authors_df has authors for all articles - we need to just get the first author
  authors_df$id <- paste0(authors_df$file_id, "_", authors_df$article_id)
  first_authors <- authors_df[!duplicated(authors_df$id), ]
  first_authors <- dplyr::left_join(first_authors, article_combined[!duplicated(article_combined$id),c(1,7)], by = "id")
  first_authors <- first_authors[c(7,5,8)]
  # Change author name to no space all lower case
    first_authors$AU <- base::tolower(first_authors$AU)
    first_authors$AU <- gsub("[[:punct:]]", "", first_authors$AU)
    first_authors$AU <-  gsub(" ", "", first_authors$AU)
    first_authors$AU <-  trim(first_authors$AU)
    
# article_combined has author info for citations
  citation_first_authors <- article_combined[c(1,8,9)]
  colnames(citation_first_authors)[[2]] <- c("AU")
  # Select the first element from each split
    split_strings <- strsplit(citation_first_authors$AU, split = ",")
    citation_first_authors$AU <- sapply(split_strings, `[`, 1)
  # Change author name to no space all lower case
    citation_first_authors$AU <- base::tolower(citation_first_authors$AU)      
    citation_first_authors$AU <- gsub("[[:punct:]]", "", citation_first_authors$AU)
    citaiton_first_authors$AU <-  gsub(" ", "", citation_first_authors$AU)
    citation_first_authors$AU <-  trim(citation_first_authors$AU)
  
# Create a dataframe with unique list of first author names
  author_list <- sort(unique(c(first_authors$AU, citation_first_authors$AU)))
  author_index <- data.frame(id=seq(1,length(author_list), 1), AU=author_list)
  colnames(author_index)[[1]] <- c("author_id") 
  
# Go back to first_author and citation_first_author to add id and sender/target label
  first_authors <- dplyr::left_join(first_authors, author_index, by = "AU")
  first_authors$role <- c("sender")
  colnames(first_authors)[[3]] <- c("node_label")
  
  citation_first_authors <- dplyr::left_join(citation_first_authors, author_index, by= "AU")
  citation_first_authors$role <- c("target")
  colnames(citation_first_authors)[[3]] <- c("node_label")
  
# Stack and make sure there is no duplicates
  first_authors_combined <- rbind(first_authors, citation_first_authors)
  first_authors_combined <- first_authors_combined[!duplicated(first_authors_combined$node_label),]
  
# Merge first_authors to our node_list
  node_list <- dplyr::left_join(node_list, first_authors_combined[c(3,4)], by="node_label")
  
  save(node_list, file="citation_node_list_2April2024.Rda")
  load("citation_node_list_2April2024.Rda")
  
#############################
###   Write out networks  ###
#############################

# Use citation node and edge list to write network 
  write_net('arcs', node_list$node_id , 
            '', '', '', 
            'blue' , 'white',
            edges$sender_id, edges$target_id, rep(1,nrow(edges)), ' ', 
            'cocitation_net_9April24', TRUE)
  
# Write out author partition
  write_clu(node_list$author_id, 'firstauthor_id')
  