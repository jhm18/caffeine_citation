## Oct 8 2024
## Sarah Delmar and Jon Morgan

### Era Pipeline ###

### Next Steps
# We are currently using articles, but we want to use their citations
# We want to look at the overlap in DOI_combined across communities
# Check about what network is looking at, go to edge list, compare edges not nodes

# 1. Build a function that extracts the links between communities as the ratio of time 2 to time 1 (T2:T1) (referencing backwards)
#   - ex. all ties between a community in T2 to all connected communities in T1 should sum to 1
# 2. Cluster the extracted links into communities (delta graph)
#   - colored by time
# 3. Run TF-IDF on the abstracts and keywords from each community
# 4. Check logic (if bad, could adjust time window to be smaller)

# Clear Out Console Script
cat("\014")

# Options
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Set Working Directory
setwd("/workspace/caffeine_citation")

######################
####   Functions  ####
######################
#Source Pajek
source("scripts/RPajekFunctions_30April2023.r")

vertices <- era22_vertices
network_partition <- era22_communities 
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
  return(id_index)
}

t1_id_index <- era22_id_index
t2_id_index <- era23_id_index

# Pull out citations from each article in each era
id_index <- t1_id_index
vertices <- era22_vertices
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
  
  return(cited_index)
}

## Edit community membership next##
# Extract links between communities
community_membership <- function(t1_id_index, t2_id_index, cited_index){
  
  # Make a shared index for citations that span eras
  shared_index <- cited_index[c(1,3,4)]
  colnames(shared_index)[[1]] <- c("pajek_id")
  
  # Make unique senders list
  #shared_senders <- data.frame(pajek_id =unique(shared_index$pajek_id))
  #shared_senders <- dplyr::left_join(shared_senders, t1_id_index[c(1,4,5)], by="pajek_id")
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
  
  return(result2)
}


##############
## Tests   ###
##############



##############
## Packages ##
##############




#####################
####  Import     ####
#####################

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

