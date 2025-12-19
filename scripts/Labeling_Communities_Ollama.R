#Labeling Communities with Ollama
#Jonathan H. Morgan, Ph.D. Sarah Delmar
#12 August 2025

# Clear Out Console Script
  cat("\014")

# Options
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  options(mc.cores = parallel::detectCores())

################
#   PACKAGES   #
################

library("httr")
library("ollamar")

#################
#   FUNCTIONS   #
#################
#   Create not in function
  `%notin%` <- Negate(`%in%`)

#   Source Pajek
    source("/workspace/caffeine_citation/scripts/RPajekFunctions_30April2023.r")
  
#   Check Ollama
    check_ollama <- function() {
        tryCatch({
            #   Try to connect to the Ollama API
                response <- httr::GET("http://127.0.0.1:11434/api/tags", 
                            httr::timeout(5))

            #   Return Status
                return(httr::status_code(response) == 200)
        }, error = function(e) {
            return(FALSE)
        })
    }

#   Start Ollama
    ensure_ollama_running <- function(max_wait = 30) {
        #   Creating Prompt
            cat("Checking if Ollama is running...\n")
    
        #   Check if already running
            if (check_ollama()) {
                #   Ollama is Running
                    cat("✓ Ollama is already running!\n")
                    return(TRUE)
            }else{
                #   Not running, so start it
                    cat("Ollama not running. Starting with bash script...\n")
                    system("~/start-ollama.sh")
                
                #   Wait for it to start up and check periodically
                    cat("Waiting for Ollama to start")
                    for (i in 1:max_wait) {
                        Sys.sleep(1)
                        cat(".")
                        
                        if (check_ollama()) {
                            cat("\n✓ Ollama started successfully!\n")
                            return(TRUE)
                        }
                    }
                    cat("\n✗ Ollama failed to start within", max_wait, "seconds\n")
                
                #   Try to see what might be wrong
                    cat("Checking if ollama process is running...\n")
                    ps_result <- system("pgrep -f 'ollama serve'", intern = TRUE)
                    if (length(ps_result) > 0) {
                        cat("Ollama process is running but not responding to API calls\n")
                    } else {
                        cat("No ollama process found\n")
                    }
                
                #   Return Outcome
                    return(FALSE)
            }
    }

#   Function to stop Ollama (useful for cleanup)
    stop_ollama <- function() {
        cat("Stopping Ollama...\n")
        system("pkill -f 'ollama serve'")
        Sys.sleep(2)
        cat("✓ Ollama stopped\n")
    }

#   Basic ollamar Testing Function  
    test_ai_generation <- function() {
        #   Ensure Ollama is running
            if (!ensure_ollama_running()) {
                stop("Could not start Ollama service")
            }
  
        #   Simple test prompt
            test_prompt <- "Generate a concise 3-word theme for: machine learning research papers about neural networks"
        
        #   Make API call
            cat("Testing AI generation...\n")
            response <- POST(
                "http://127.0.0.1:11434/api/generate",
                body = list(
                    model = "llama3.1:8b",
                    prompt = test_prompt,
                    stream = FALSE
                ),
                encode = "json"
            )
        
        #   Extract response
            result <- content(response)$response
        
        #   Display result
            cat("Prompt:", test_prompt, "\n")
            cat("Response:", result, "\n")
        
        #   Return Result
            return(result)
    }

#   Function to Prepare the Data by Community
    prepare_community_data <- function(data) {
        #   Get unique communities
            unique_communities <- unique(data$community)
            
        #   Sort data by community and total degree
            data <- data[order(data$community, data$total_degree, decreasing = c(FALSE,TRUE)),]
        
        #   Initialize results
            core_community_themes <- data.frame(
                community_id = integer(),
                combined_abstracts = character(),
                abstract_count = integer(),
                stringsAsFactors = FALSE
            )
            
            minor_community_themes <- data.frame(
              community_id = integer(),
              combined_abstracts = character(),
              abstract_count = integer(),
              stringsAsFactors = FALSE
            )
    
        #   Process each community core
            for(comm_id in unique_communities) {
                #   Get all abstracts for this community
                    comm_data <- data[data$community == comm_id, ]
                    comm_data <- comm_data[c(1:5),]
                    
                #   Combine abstracts
                    combined_text <- paste(comm_data$abstract_list, collapse = " ")
                    abstract_count <- nrow(comm_data)
                    
                #   Add to results
                    core_community_themes <- rbind(core_community_themes, data.frame(
                        community_id = comm_id,
                        combined_abstracts = combined_text,
                        abstract_count = abstract_count,
                        stringsAsFactors = FALSE
                    ))
            }
            
        #   Process minor communities
            for(comm_id in unique_communities) {
              #   Get all abstracts for this community
              comm_data <- data[data$community == comm_id, ]
              comm_data <- comm_data[c(6:nrow(comm_data)),]
              
              #   Combine abstracts
              combined_text <- paste(comm_data$abstract_list, collapse = " ")
              abstract_count <- nrow(comm_data)
              
              #   Add to results
              minor_community_themes <- rbind(minor_community_themes, data.frame(
                community_id = comm_id,
                combined_abstracts = combined_text,
                abstract_count = abstract_count,
                stringsAsFactors = FALSE
              ))
            }
        
        #   Return Consolidated Abstracts by Community
            return(list(core_themes = core_community_themes, minor_themes = minor_community_themes))
    }

#   Function to map degree rankings
    community_degree_mapper <- function(network_loc, community_loc, era_prompt, degree_file_loc){
        #   Pull in network
            read_net(network_loc)
        
        #   Pull in community file
            communities <- readLines(community_loc)
            communities <- as.integer(communities[-c(1)])

        #   Create a community index
            community_idx <- cbind(vertices,communities)
            community_idx <- community_idx[order(community_idx$communities, community_idx$ID),]
            colnames(community_idx)[[2]] <- c("node_id")
            community_idx <- community_idx[c(1,2,6)]
            
        #   Load prompt data
            environment_elements <- ls()
            load(era_prompt)
            workspace_elements <- ls()
            workspace_elements <- workspace_elements[workspace_elements %notin% environment_elements]
            era_name <- workspace_elements[workspace_elements != "environment_elements"]
            iteration_prompt <- get(era_name)
            
        #   Loop through communities
            community_ids <- unique(community_idx$communities)
            degree_files <- list.files(path=degree_file_loc, pattern="*.vec", full.names= TRUE)
            output_list <- vector("list", length(community_ids))
            
            #   Check for i=2,  degree file naming is sorted by character name (1,10)
            #   Make degree index
                file_idx <- data.frame(location = degree_files)
                string_elements <- strsplit(file_idx$location, "/")
                net_numbers <- vector("numeric", length(string_elements))
                for (i in seq_along(string_elements)){
                    net_name <- string_elements[[i]][length(string_elements[[i]])]
                    net_elements <- strsplit(net_name, "_")[[1]]
                    net_elements <- net_elements[length(net_elements)]
                    net_number <- as.integer(strsplit(net_elements, "\\.")[[1]][[1]])
                    net_numbers[[i]] <- net_number
                }
                file_idx$net_id <- net_numbers
                file_idx <- file_idx[order(file_idx$net_id),]
                
            #   Take degree file name, break it up to isolate number from name and sort by number
                for (i in seq_along(community_ids)){
                    # Subset community index by current community
                    curr_community <- community_idx[community_idx$communities == community_ids[[i]], ]
                    
                    # Pull in degree file for community
                    degree <- readLines(file_idx$location[[i]])
                    degree <- as.integer(degree[-c(1)])
                    curr_community$total_degree <- degree
                    
                    # Subset using prompt data to isolate nodes in both eras
                    curr_prompt <- iteration_prompt[iteration_prompt$community == community_ids[[i]], ]
                    curr_prompt <- dplyr::left_join(curr_prompt, curr_community[c(2,4)], by = "node_id")
                    curr_prompt <- curr_prompt[order(curr_prompt$total_degree, decreasing = TRUE),]
                    
                    # Populate output list
                    output_list[[i]] <- curr_prompt
                }
            
            #   Create community degree index
                degree_idx <- do.call("rbind", output_list)
                unique(degree_idx$total_degree)

            #   Return community degree index
                return(degree_idx)
    }

#   Helper Function: Text Truncation 
    truncate_text <- function(text_list, core_threshold = 8000) {
        #   Core Theme Character Length
            core_theme_lengths <- nchar(text_list$core_themes$combined_abstracts)
            core_theme_index <- data.frame(community_id = text_list$core_themes$community_id, core_theme_n =  core_theme_lengths)

        #   Identify Max Count
            max_chars = round(max(core_theme_index$core_theme_n), digits=10)

        #   Identifying Communities that Require Minor Themes
            minor_communities <- core_theme_index$community_id[(core_theme_index$core_theme_n < core_threshold)]
            core_n <- core_theme_index$core_theme_n[(core_theme_index$core_theme_n < core_threshold)]
            community_deltas <- max_chars - core_theme_index$core_theme_n[(core_theme_index$core_theme_n < core_threshold)]
            minor_index <- data.frame(community =  minor_communities, core_theme_nchar =  core_n, char_delta = community_deltas)

        #   Extract Minor Themes
            minor_themes <- text_list$minor_themes[(text_list$minor_themes$community_id %in% minor_communities), ]

        #   Iterate Through Communities Supplemented by a Minor Theme
            minor_community_themes <- vector('list', nrow(minor_index))
            names(minor_community_themes) <- minor_index$community
            for (i in seq_along( minor_community_themes )){
                #   Isolate Minor Theme
                    minor_theme <- minor_themes[(minor_themes$community_id == minor_communities[[i]]), 2]  

                #   Isolating Community Delta
                    comm_delta <- minor_index$char_delta[[i]]

                #   Check if the prompt exceeds delta to Avoid Truncating Unnecessarily
                    if (nchar(minor_theme) <=  comm_delta){
                        #   Populating if Minor Themes is Below the Community Delta
                            minor_community_themes[[i]] <-  minor_theme
                    }else{
                        #   Truncate to max_chars
                            truncated <- substr(minor_theme, 1, comm_delta)
                
                        #   Try to end at a complete sentence
                            last_period <- max(gregexpr("\\.", truncated)[[1]])
                            if (last_period > comm_delta * 0.8) {
                                truncated <- substr(truncated, 1, last_period)
                            }

                        #   Report truncation
                            cat("    Truncated minor theme text from", nchar(minor_theme), "to", nchar(truncated), "characters\n")

                        #   Populate minor_community_themes
                            minor_community_themes[[i]] <- truncated
                    } 
            }

        #   Combine Major and Minor Themes
            core_theme_index$is_minor <- ifelse(core_theme_index$core_theme_n < 8000, 1, 0) 
            combined_themes <- vector("list", nrow(core_theme_index))
            names(combined_themes) <- core_theme_index$community_id
            iteration <- 0
            for(i in seq_along(combined_themes)){
                #   Update Check Variable
                    iteration <- i

                #   Has Minor Theme
                    is_minor <- core_theme_index$is_minor[[i]]
                    community_id <- core_theme_index$community_id[[i]]

                #   Handling Combined Major & Minor vs. Major Theme Only
                    if(is_minor == 1){
                        #   Isolate Major Theme
                            major_theme <- text_list$core_themes$combined_abstracts[[i]]

                        #   Isolate Truncated Minor Theme
                            minor_theme <- minor_community_themes[names(minor_community_themes) == community_id]

                        #   Combine Themes
                            combined_theme <- c(major_theme, minor_theme)
                            combined_theme <- paste( combined_theme, collapse = " ")

                        #   Populating Combined Themes
                            combined_themes[[i]] <- combined_theme
                    }else{
                        #   Populating Combined Themes
                            combined_themes[[i]] <- text_list$core_themes$combined_abstracts[[i]]
                    }
            }

        #   Stack the List into a DataFrame
            combined_themes_vector <- do.call("rbind",   combined_themes)
            combined_themes_df <- data.frame(community = as.integer(names(combined_themes)), theme= combined_themes_vector)

        #   Return Combined Theme
            return(combined_themes_df)
    }

#   Helper Function to Determine Stopping Points to Avoid Time Out Errors
    calculate_timeout <- function(char_count, max_timeout = 1200) {
        base_timeout <- 180
        seconds_per_1k <- 18
        timeout <- base_timeout + (char_count / 1000) * seconds_per_1k
        min(round(timeout), max_timeout)
    }

#   Function to Generate Community Themes
#   raw_data <- era22_prompt
    generate_community_themes <- function(raw_data, core_threshold = 8000, max_timeout = 1200,
                                          model = "llama3.1:8b", fallback_timeout = 900,     # 15 minutes
                                          cooldown_seconds = 30) {

        #   Ensure Ollama is running
            if (!ensure_ollama_running()) {
                stop("Could not start Ollama service")
            }

        #   Prepare data by community
            community_data <- prepare_community_data(raw_data)
            cat("Found", nrow(community_data$core_themes), "unique communities\n")

        #   Load required libraries
            library("httr")
            library("jsonlite")

        #   Initialize results
            results <- data.frame(
                community_id = integer(),
                theme = character(),
                processing_time = numeric(),
                status = character(),
                stringsAsFactors = FALSE
            )

        #   Prepare text (with truncation if needed)
            processed_text <- truncate_text(community_data, core_threshold)

        #   Helper: build prompt
            build_prompt <- function(comm_id, community_text) {
                paste(
                    "You must respond with ONLY a 5-10 word theme name. No explanations, no extra text.\n\n",
                    "Research cluster content for community", comm_id, ":\n",
                    community_text, "\n\n",
                    "Theme:"
                )
            }

        #   Helper: one API call
            make_request <- function(prompt_txt, timeout_seconds) {
                POST(
                    "http://127.0.0.1:11434/api/generate",
                    add_headers(`Content-Type` = "application/json"),
                    body = list(
                        model = model,
                        prompt = prompt_txt,
                        stream = FALSE,
                        options = list(
                            num_predict = 40,
                            temperature = 0.2
                        )
                    ),
                    encode = "json",
                    timeout(timeout_seconds)
                )
            }

        #   Process each community
            for (i in 1:nrow(processed_text)) {

                #   Isolating Data & Declaring Attempt
                    comm_id <- processed_text$community[i]
                    start_time <- Sys.time()

                    cat("Processing community", comm_id, "...\n")

                    community_text <- processed_text$theme[i]
                    char_count <- nchar(community_text)

                #   Attempt 1 timeout (length-based)
                    timeout_seconds <- calculate_timeout(char_count, max_timeout)
                    cat("    Using timeout:", timeout_seconds, "seconds\n")

                    prompt <- build_prompt(comm_id, community_text)

                    attempt <- 1
                    success <- FALSE
                    last_error <- NA_character_

                    while (attempt <= 2 && !success) {

                        if (attempt == 2) {
                            cat("    Retry with fallback timeout:", fallback_timeout, "seconds\n")
                        } else {
                            cat("    Attempt 1\n")
                        }

                        response <- NULL
                        err_msg <- NULL

                        tryCatch({
                            response <- make_request(prompt, if (attempt == 1) timeout_seconds else fallback_timeout)
                        }, error = function(e) {
                            err_msg <<- e$message
                        })

                        # Timeout/transport error
                        if (!is.null(err_msg)) {
                            last_error <- err_msg

                            is_timeout <- grepl("Timeout was reached|timed out", err_msg, ignore.case = TRUE)

                            if (is_timeout && attempt == 1) {
                                attempt <- 2
                                next
                            } else {
                                break
                            }
                        }

                        # Non-200 response
                        if (status_code(response) != 200) {
                            body_txt <- content(response, as = "text", encoding = "UTF-8")
                            last_error <- paste0("API status ", status_code(response), ": ", body_txt)
                            break
                        }

                        # Extract and clean response
                        theme <- content(response)$response
                        theme <- trimws(theme)
                        theme <- gsub("^(Here is|Here's|The theme is|Theme:|A theme could be):?\\s*", "", theme, ignore.case = TRUE)
                        theme <- gsub("^[\"']|[\"']$", "", theme)
                        theme <- gsub("\n.*", "", theme)
                        theme <- gsub("\\.$", "", theme)
                        theme <- strsplit(theme, "\\.|\\n")[[1]][1]
                        theme <- trimws(theme)

                        processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

                        results <- rbind(results, data.frame(
                            community_id = comm_id,
                            theme = theme,
                            processing_time = round(processing_time, 1),
                            status = "success",
                            stringsAsFactors = FALSE
                        ))

                        cat("Community", comm_id, ":", theme,
                            "(", round(processing_time, 1), "seconds )\n\n")

                        success <- TRUE
                    }

                #   Record failure + cooldown
                    if (!success) {
                        processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                        cat("ERROR for community", comm_id, ":", last_error, "\n")

                        results <- rbind(results, data.frame(
                            community_id = comm_id,
                            theme = paste("ERROR:", substr(last_error, 1, 120)),
                            processing_time = round(processing_time, 1),
                            status = "failed",
                            stringsAsFactors = FALSE
                        ))

                        cat("Cooling down for", cooldown_seconds, "seconds, then continuing...\n\n")
                        Sys.sleep(cooldown_seconds)
                    }
            }

        #   Summary
            successful <- sum(results$status == "success")
            failed <- sum(results$status == "failed")
            cat("=== SUMMARY ===\n")
            cat("Successful:", successful, "communities\n")
            cat("Failed:", failed, "communities\n")
            cat("Total processing time:", round(sum(results$processing_time), 1), "seconds\n")

        #   Return Results
            return(results)
    }

#   Function to Examine Community-Level Lists
    community_abstracts_finder <- function(data, community_id, themes_data){
        #   Formatting Data
            colnames(data)[[4]] <- c("community_id")

        #   Isolating Community Data
            community_data <- data[(data$community_id == community_id), c(4,6,7)]

        #   Adding Theme
            community_data <- dplyr::left_join(community_data, themes_data[,c(1,2)], by=c("community_id"))

        #   Return Community with Theme
            return(community_data)
    }

#	  Generate MCR Commands Helper
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
  				paste0("% Reading Network   ---    ", .make_c_paths(network_path)),
  				paste0("N 1 RDN ", .make_c_paths(network_path))
  			)
  			mcr_lines <- c(mcr_lines, net_section)
  		
  		#	Add partition reading section  
  			part_section <- c(
  				paste0("% Reading Partition   ---    ", .make_c_paths(partition_path)),
  				paste0("C 1 RDC ", .make_c_paths(partition_path))
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
  					comment_save <- paste0("% Saving vector to file   ---    ", .make_c_paths(file_path))
  				
  				#	Create save command
  					save_cmd <- paste0("V ", i, " WV ", 	.make_c_paths(file_path), " 0")
  				
  				#	Add all lines for this community
  					mcr_lines <- c(mcr_lines, comment_deg, deg_cmd, comment_save, save_cmd)
  			}
  		
  		#	Assembling result
  			return(mcr_lines)
  	}

#   Generate MCR Paths Helper
  	.make_c_paths <- function(directory_path) {
  	  #	"""
  	  #	Args:
  	  #		directory_path: string, Unix-style path (e.g., "/Users/metal")
  	  #	Returns:
  	  #		string, Windows C: drive path (e.g., "C:\\Users\\metal")
  	  #	Notes:
  	  #		Helper function for internal use. Converts forward slashes to backslashes.
  	  #	"""
  	  
  	  #	Validation
  	  if (missing(directory_path) || is.null(directory_path)) {
  	    stop("directory_path must be provided")
  	  }
  	  
  	  #	Split into elements
  	  path_elements <- base::strsplit(directory_path, "/")[[1]]
  	  path_elements <- path_elements[nchar(path_elements) != 0]
  	  
  	  #	Build C: path
  	  c_path <- paste0(path_elements, collapse = "\\")
  	  c_path <- paste0("C:\\", c_path)
  	  
  	  #	Assemble result
  	  return(c_path)
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
		stopifnot(is.character(network_path ), length(network_path) == 1)
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

##################
#   BASIC TEST   #
##################

#   Test if ollamar is available
    if (require(ollamar, quietly = TRUE)) {
        #   Test connection
            cat("✓ ollamar package is available\n")
            test_connection()
    } else {
        #   Error Warnings
            cat("✗ ollamar package not found\n")
            cat("We'll use direct HTTP calls instead\n")
    }

#   Performing Basic Test
    test_result <- test_ai_generation()

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
    write_pajek_mcr(network_path,  partition_path,  output_dir,  mcr_file_path)
    
#   Mapping Community Degrees to Prompt Data    
    era_prompt <- "/workspace/caffeine_citation/data/era22_prompt.Rda"
    degree_file_loc <- "/workspace/caffeine_citation/pajek_files/Era22/Community_Degree_Files"
    era22_prompt <- community_degree_mapper(network_path,partition_path,era_prompt, degree_file_loc)
    
#   Generating Community Labels & Exporting Era 22 Results
    era22_results <- generate_community_themes(era22_prompt, core_threshold = 8000, max_timeout = 1200,
                                          model = "llama3.1:8b", fallback_timeout = 900,     # 15 minutes
                                          cooldown_seconds = 30)
    readr::write_csv(era22_results, file=c("/workspace/caffeine_citation/data/era22_results.csv"))



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
    