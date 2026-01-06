### 14 May 2024, Sarah Ricupero
### Thinning cocitation network

### Next Steps
# 1. Build a function that extracts the links between communities as the ratio of time 2 to time 1 (T2:T1) (referencing backwards)
#   - ex. all ties between a community in T2 to all connected communities in T1 should sum to 1
# 2. Cluster the extracted links into communities (delta graph)
#   - colored by time
# 3. Run TF-IDF on the abstracts and keywords from each community
# 4. Check logic (if bad, could adjust time window to be smaller)


#Notes:
# Currently saving all temporal networks
# Next, cluster one network, use an example network and clustering solution to test our keyword, title, and abstract extraction routines
# Start with last network, export and use as use case
##
#Source Pajek
  source("RPajekFunctions_30April2023.R")

# Read in .net file
  read_net("cocitation_net_9April24.net")

# Rank-Based Thinning Algorithm 
  rank_based_thinning <- function(edges, k_rank, direction = "target", 
                                  symmetrize = FALSE, reduce_self_loop = FALSE) {
    # Making a Deep Copy to Avoid Altering the Input Edgelist
      edge_list <- data.frame(edges, stringsAsFactors = FALSE)
      names(edge_list) <- c('source_id', 'target_id', 'weight')
    
    # Conditional: Eliminate Self-Loops
      if (reduce_self_loop) {
        edge_list <- edge_list[edge_list$source_id != edge_list$target_id, ]
      }
    
    # Conditional symmetrization
      if (symmetrize) {
        # Create a standardized identifier for edges where order does not matter
          standard_id <- mapply(function(a, b) paste(min(a, b), max(a, b), sep="-"), edge_list$source_id, edge_list$target_id)
          edge_list$standard_id <- standard_id
      
        # Combine edges based on this new identifier and sum weights
          agg_data <- aggregate(weight ~ standard_id, data = edge_list, sum)
          split_ids <- strsplit(as.character(agg_data$standard_id), "-")
      
          edge_list <- data.frame(source_id = sapply(split_ids, function(x) x[1]),
                                  target_id = sapply(split_ids, function(x) x[2]),
                                  weight = agg_data$weight)
      }
    
    # Sort by descending weight
      edge_list <- edge_list[order(-edge_list$weight), ]
    
    # Get unique node IDs from both source and target
      node_ids <- if(direction == "target"){
                        unique(c(edge_list$target_id, edge_list$source_id))
                  }else{
                    unique(c(edge_list$source_id, edge_list$target_id))
                  }
    
    # Preallocate a list for each node ID
      ego_edges <- vector("list", length(node_ids))
      names(ego_edges) <- node_ids
      for (node_id in node_ids) {
        # Filter edges where current node_id is either source or target
          filtered_edges <- edge_list[edge_list$source_id == node_id | edge_list$target_id == node_id, ]
      
        # Get the top 'k_rank' edges
          ego_edges[[node_id]] <- head(filtered_edges, min(nrow(filtered_edges), k_rank))
      }
    
    # Combine all individual DataFrames into one
      rank_thinned_edges <- do.call(rbind, ego_edges)
    
    # Eliminate Duplicates
      rank_thinned_edges <- rank_thinned_edges[!duplicated(rank_thinned_edges), ]
    
    # Return thinned edge list
      return(rank_thinned_edges)
  }
  
# Use thinning function
  thinned_edges <- rank_based_thinning(ties[c(1:3)], 25)
  
  save(thinned_edges, file = "thinned_edges_cociation_net.Rda")
  
# Save out in Pajek as .net file
  nodes <- sort(unique(c(thinned_edges$source_id, thinned_edges$target_id)))
  
  write_net("arcs", nodes, "", "", "", "blue", "white", thinned_edges$source_id, thinned_edges$target_id, thinned_edges$weight, "gray", "cocitation_thinned_edges_net", TRUE)
  ##############################
  #####  Temporal Analyses #####
  ##############################
# Examine counts by year
  load("~/Downloads/article_combined_March5.Rda")
  
  year_table <- table(article_combined$year)
  year_count <- data.frame(year=names(year_table), count=year_table)
  year_count <- year_count[-c(2)]
  
# Important plot of paper count over time  
  plot(year_count$year, year_count$count.Freq, bty="l", las=1)
  
  year_count$year <- as.integer(year_count$year)
  year_count_high <- year_count[year_count$year > 1990,]
  year_count_low <- year_count[year_count$year <= 1990,]
  
  plot(year_count_high$year, year_count_high$count.Freq, bty="l", las=1)
  # 5 year chunks up to 2008, then by 1 year
  plot(year_count_low$year, year_count_low$count.Freq, bty="l", las=1)
  # No real increase before 1960, look at 1945*
  # Golden era (1907 to 1944)
  # Look at 1945
  # Every 10 years until 1990
  
  # golden era, 1945, 46-55, 56-65, 66-75, 76-85, 86-90, 91-95, 96-2000, 01-05, 06-08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 (23 total)
  
# Create unique time ID
  time_id <- data.frame(time_id=seq_along(year_count$year), year=year_count$year)
  time_id$year <- as.integer(time_id$year)
  era_index <- data.frame(era=seq(1,11,1), start=c(1907,1945,1946,1956,1966,1976,1986,1991,1996,2001,2006), 
                          end=c(1944,1945,1955,1965,1975,1985,1990,1995,2000,2005,2008))
  late_era <- data.frame(era=seq(12,23,1), start=seq(2009,2020,1), end=seq(2009,2020,1))
  era_index <- rbind(era_index, late_era)
  era <- vector("integer", length= nrow(year_count))
  for(i in seq_along(era)){
    year <- year_count[i,1]
    era[[i]] <- era_index[(year >= era_index$start & year <= era_index$end), 1]
  }
  time_id$era <- era

  time_id <- time_id[time_id$year < 2021, ]
  
# Map back to article_combined dataset
  article_combined <-  dplyr::left_join(article_combined, time_id, by=c("year"))
  
  save(article_combined, file="article_combinedv2.Rda")
# Recreate node list to include year id and year
  source_article_combined <- article_combined[c(6,7,10)]
  target_article_combined <- article_combined[c(6,9,10)]
  
  colnames(source_article_combined)[2] <- c("node_label")
  colnames(target_article_combined)[2] <- c("node_label")
  
  id_index <- rbind(source_article_combined,target_article_combined)
  id_index <- id_index[!duplicated(id_index$node_label),]
  era_index <- article_combined[!duplicated(article_combined$time_id),c(10:11)]
  
  id_index <- dplyr::left_join(id_index, era_index, by=c("time_id"))
  
  node_list <- dplyr::left_join(node_list, id_index, by="node_label")
  
  # Use 999 for NA era
  node_list[is.na(node_list$era), 7] <- 999
  save(node_list, file = "citation_node_list_3Oct2024.Rda")
  
# Write a pajeck file
  write_clu(node_list$era, "cocitation_era_3Oct2024")
  
  

  
  