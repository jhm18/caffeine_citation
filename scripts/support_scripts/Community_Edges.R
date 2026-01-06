# Volume-Based Community Edge Weights
# Jonathan H. Morgan
# Based on James Moody's groupmixing1.sas 
# 12 January 2023

# Clear Out Console Script
  cat("\014")

# Options
  options(stringsAsFactors = FALSE)
  options(scipen = 999)

# Set Working Directory
  if (paste(Sys.info()[[1]], collapse=" ") == "Darwin"){
    setwd("~/.CMVolumes/Jonathan Morgan/DNAC/Volume_Based_Community_Ties")
    getwd()
  }else{
    setwd("/mnt/d/Dropbox/DNAC/Volume_Based_Community_Ties")
    getwd()
  }
  
################
#   PACKAGES   #
################
library('igraph')
  
######################
#   IMPORTING DATA   #
######################

# Importing SAS Files
  test_matrix <- as.matrix(readr::read_csv("Example_Net.csv"))
  test_partition <- readr::read_csv("Example_Partition.csv")
  mixing_matrix <- readr::read_csv("Example_MixingMatrix.csv")
  expected_values <- readr::read_csv("Example_ExpectedValues.csv")
  
################################
#   CREATING NETWORK OBJECTS   #
################################
  
# Converting Matrix into a Graph
  test_network <- igraph::graph_from_adjacency_matrix(test_matrix, mode=c('directed'), diag = TRUE)
  par(mar=c(0,0,0,0))
  plot(test_network)
  
# Extracting Edge and Node List
  edges <- as.data.frame(igraph::as_edgelist(test_network, names=FALSE))
  nodes <- as.data.frame(1:length(igraph::V(test_network)))
  colnames(nodes) <- c("id")
  nodes$label <- paste0("COL", nodes$id)
  
######################
#   EDGE FUNCTIONS   #
######################
  
# Creating Community Edges Weights Corrected for Volume
  VolumeBased_Mixing <- function(edgelist, weights=NULL, partition){
    # Remove Self-Loops
      edges <- edgelist[(edgelist[[1]] != edgelist[[2]]),]
      colnames(edges) <- c('i_nodes', 'j_nodes')
      edges$Obs_ID <- seq(1,length(edges[[1]]), 1) 
      edges <- edges[c(3,1,2)]
      
    # Extracting nodes from the edgelist
      nodes <- sort(unique(c(edgelist[[1]], edgelist[[2]])))
      nodes <- as.data.frame(cbind(nodes, partition))
      
    # Add Partition Data to Edges
      colnames(nodes) <- c('i_nodes', 'i_partition')
      senders <- edges[c(1,2)]
      senders <- dplyr::left_join(senders,nodes, by=c("i_nodes"))
      
      colnames(nodes) <- c('j_nodes', 'j_partition')
      targets <- edges[c(1,3)]
      targets <- dplyr::left_join(targets,nodes, by=c("j_nodes"))
      
      edges <- dplyr::left_join(senders, targets, by=c("Obs_ID"))
      rm(senders, targets)
      
    # Adding Weights
      if(is.null(weights)){
        weights <- rep(1,dim(edges)[[1]])
        edges$weight <- weights
      }else{
        edges$weight <- weights
      }
      
    # Creating Mixing Matrix
      mixing_table <- aggregate(weight ~ i_partition + j_partition, data = edges, FUN = sum, na.rm = TRUE)
      mixing_table <- matrix(mixing_table$weight, ncol = length(sort(unique(partition))), nrow = length(sort(unique(partition))))
      colnames(mixing_table) <- sort(unique(partition))
      rownames(mixing_table) <- sort(unique(partition))
      
      row_values <- vector('integer', dim(mixing_table)[[1]])
      col_values <- vector('integer', dim(mixing_table)[[2]])
      for(i in seq_along(mixing_table[,1])){
        row_values[[i]] <- sum(mixing_table[i,])
        col_values[[i]] <- sum(mixing_table[,i])
      }
       
    # Calculating Expected Values: row x col over sum
      expectation <- (row_values %o%col_values)/sum(mixing_table)
      colnames(expectation) <- sort(unique(partition))
      rownames(expectation) <- sort(unique(partition))
      
    # Return Expected Values
      output_list <- vector('list', 2)
      names(output_list) <- c('Mixing Table', 'Expectations')
      output_list[[1]] <- mixing_table
      output_list[[2]] <- expectation
      return(output_list)
  }
  
  community_edge_weights <- VolumeBased_Mixing(edges, weights=NULL, test_partition$community)
  
# Transforming into an Edgelist
  community_edge_list <- function(community_edges, partition){
    # Converting the edge_matrix into an edgelist
      edges <- as.data.frame(gtools::permutations(n=length(sort(unique(partition))), r=2, 
                                                  v=sort(unique(partition)), repeats.allowed=T))
      colnames(edges) <- c('community_i', 'community_j')
      edges$value <-  as.vector(t(community_edges[[1]]))
      edges$expectation <- as.vector(t(community_edges[[2]]))
      
    # Returning the edgelist
      return(edges)
  }
  
  community_edgelist <- community_edge_list(community_edge_weights, test_partition$community)
  
# Calculating chi_squared_difference_scores 
  chi_square_differences <- function(community_edgelist){
    # Calculate Differences
      community_differences <- community_edgelist$value - community_edgelist$expectation
      valences <-  sign(community_differences)
      
    # Squaring Differences
      community_squares <- community_differences * community_differences
      
    # Calculating Magnitudes
      community_magnitudes <- community_squares/community_edgelist$expectation
      
    # Returning the Adjusted Weights
      community_edgelist$adjusted_weights <- community_magnitudes*valences
      return(community_edgelist)
  }
  community_edgelist <- chi_square_differences(community_edgelist)
  
# Plotting Adjusted Community Graph
  el <- cbind((community_edgelist[,1] + 1),(community_edgelist[,2] + 1))
  comm_net <- igraph::graph_from_edgelist(el)
  E(comm_net)$weight <- community_edgelist$adjusted_weights
  E(comm_net)$weight
  
  par(mar=c(0,0,0,0))
  E(comm_net)$width <- E(comm_net)$weight + min(E(comm_net)$weight) + 1 # offset=1
  plot(comm_graph)
  
#############
#   TESTS   #
#############
  
# Converting Edge Matrix into a igraph object
  comm_graph <- graph.adjacency(community_edge_weights[[2]], mode="directed", weighted=TRUE, diag = TRUE)
  
# Checking Edgelist with function output
  edges <- as.data.frame(igraph::as_edgelist(comm_graph, names=FALSE))
  edges$weight <- E(comm_graph)$weight 
  
# Plotting the Community Graph
  par(mar=c(0,0,0,0))
  E(comm_graph)$width <- E(comm_graph)$weight + min(E(comm_graph)$weight) + 1 # offset=1
  plot(comm_graph)
  