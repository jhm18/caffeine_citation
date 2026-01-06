# Creating MCR File to Isolate Node-Level Ranking for Each Community by Degree
# Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
# 2 September 2025

# Clear Out Console Script
  cat("\014")

# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())
  options(scipen = 999)
  
#################
#   FUNCTIONS   #
#################
  
#	Generate MCR Commands Helper
  .generate_pajek_community_mcr <- function(network_path, partition_path, output_dir) {
    #	"""
    #	Args:
    #		network_path: string, full path to the .net network file
    #		partition_path: string, full path to the .clu partition file
    #		output_dir: string, directory path for output vector files
    #	Returns:
    #		character vector, each element is a line of the MCR file
    #	Notes:
    #		Helper function that generates the complete MCR content including
    #		headers, file reading commands, and community processing loops.
    #	"""
    
    #   Determine K
      community_vector <- readLines(partition_path)
      community_vector <- as.integer(community_vector[c(-1)])
      k <- length(unique(community_vector))
      
    #	Initialize output vector
      mcr_lines <- character()
      
    #	Add MCR file headers
      headers <- c(
        "NETBEGIN 1",
        "CLUBEGIN 1", 
        "PERBEGIN 1",
        "CLSBEGIN 1",
        "HIEBEGIN 1",
        "VECBEGIN 1"
      )
      mcr_lines <- c(mcr_lines, headers)
    
    #	Add network reading section
      net_section <- c(
        paste0("% Reading Network   ---    ", network_path),
        paste0("N 1 RDN \"", network_path, "\"")
      )
      mcr_lines <- c(mcr_lines, net_section)
    
    #	Add partition reading section  
      part_section <- c(
        paste0("% Reading Partition   ---    ", partition_path),
        paste0("C 1 RDC \"", partition_path, "\"")
      )
      mcr_lines <- c(mcr_lines, part_section)
      
    #	Add subnetwork extraction section
      extract_section <- c(
        "% Extracting Subnetworks induced by each selected Cluster",
        paste0("N ", k + 1, " EXTNETALL 1 1 [1-*]")
      )
      mcr_lines <- c(mcr_lines, extract_section)
    
    #	Generate commands for each community
      for (i in seq_len(k)) {
        #	Calculate network index (starts from 2 since N1 is parent)
        net_idx <- i + 1
        
        #	Create comment line for degree centrality calculation
        comment_deg <- paste0("% All degree centrality of ", net_idx, 
                              ". Subnetwork of N1 induced by C1 [", i, "]")
        
        #	Create degree centrality command
        deg_cmd <- paste0("V ", i, " DEGV ", net_idx, " [2]")
        
        #	Create file path for output
        file_path <- file.path(output_dir, 
                               paste0("total_degree_community_", i, ".vec"))
        
        #	Create comment line for saving vector
        comment_save <- paste0("% Saving vector to file   ---    ", file_path)
        
        #	Create save command
        save_cmd <- paste0("V ", i, " WV \"", file_path, "\" 0")
        
        #	Add all lines for this community
        mcr_lines <- c(mcr_lines, comment_deg, deg_cmd, comment_save, save_cmd)
      }
      
    #	Assembling result
      return(mcr_lines)
  }
  
  #	Generate Complete Pajek MCR for Community Degree Centrality
  #' @title write_pajek_mcr
  #' @description Generate a complete Pajek MCR file for calculating degree centrality within communities.
  #'
  #' @param network_path Full path to the .net network file.
  #' @param partition_path Full path to the .clu partition file containing community assignments.
  #' @param output_dir Directory path where individual community degree vector files will be saved.
  #' @param mcr_file_path Full path (including filename) where the MCR script file should be saved.
  #'
  #' @details This function automatically determines the number of communities from the partition file
  #' and generates a complete Pajek MCR script that includes initialization commands, network and
  #' partition file reading, subnetwork extraction for each community, degree centrality calculations,
  #' and file output commands for each community's degree vector.
  #'
  #' @return NULL (invisibly). The function writes the MCR file to disk and prints confirmation messages.
  #'
  #' @examples
  #' # Generate MCR file for network analysis
  #' network_file <- "C:/Users/metal/OneDrive/Desktop/era22.net"
  #' partition_file <- "C:/Users/metal/OneDrive/Desktop/era22_testCommunity.clu"
  #' output_directory <- "C:/Users/metal/OneDrive/Desktop"
  #' mcr_script <- "C:/Users/metal/OneDrive/Desktop/community_analysis.mcr"
  #' 
  #' write_pajek_mcr(network_file, partition_file, output_directory, mcr_script)
  #'
  #' @export
  write_pajek_mcr <- function(network_path, partition_path, output_dir, mcr_file_path) {
    #	"""
    #	Args:
    #		network_path: string, full path to the .net network file
    #		partition_path: string, full path to the .clu partition file
    #		output_dir: string, directory path for output vector files
    #		mcr_file_path: string, full path where MCR file should be saved
    #	Returns:
    #		NULL (writes complete MCR file to disk)
    #	Notes:
    #		Generates complete Pajek MCR file including initialization,
    #		network/partition reading, subnetwork extraction, and degree
    #		centrality calculations for each community.
    #	"""
    
    #	Determine K
      community_vector <- readLines(partition_path)
      community_vector <- as.integer(community_vector[c(-1)])
      k <- length(unique(community_vector))
    
    #	Validation
      stopifnot(is.numeric(k), k >= 1, k == floor(k))
      stopifnot(is.character(network_path), length(network_path) == 1)
      stopifnot(is.character(partition_path), length(partition_path) == 1)
      stopifnot(is.character(output_dir), length(output_dir) == 1)
      stopifnot(is.character(mcr_file_path), length(mcr_file_path) == 1)
    
    #	Generate complete MCR content
      mcr_content <- .generate_pajek_community_mcr(network_path, partition_path, output_dir)
    
    #	Write to file
      writeLines(mcr_content, mcr_file_path)
    
    #	Confirmation message
      cat("Complete MCR file written to:", mcr_file_path, "\n")
      cat("Generated commands for", k, "communities\n")
  }
  
#########################
#   CREATING MCR FILE   #
#########################
  
# Specifying Inputs
  directory <- c("/mnt/c/Users/metal/OneDrive/Desktop/Era_22_Community_Outputs")
  network_path <- paste0(directory, "/", "era22.net")
  partition_path <- paste0(directory, "/", "era22_testCommunity.clu")
  output_dir <- paste0(directory,"/","Community_Degree_Files")
  mcr_file_path <- paste0(directory,"/","Community_Degree_Rankings.MCR")
  
# Generating MCR File for Pajek
  write_pajek_mcr(network_path, partition_path, output_dir, mcr_file_path)
  
# The MCR file looks good; however, we have to use C:\ directory standard not Posix directories.
# The only directory necessary for these functions is the partitition_path.
# The rest can be inputted in Windows format.
# We can specify a function to change the directory into Windows format so that Pajek can read it.
