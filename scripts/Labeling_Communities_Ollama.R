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
            unique_communities <- unique(data$community_id)
        
        #   Initialize results
            community_themes <- data.frame(
                community_id = integer(),
                combined_abstracts = character(),
                abstract_count = integer(),
                stringsAsFactors = FALSE
            )
    
        #   Process each community
            for(comm_id in unique_communities) {
                #   Get all abstracts for this community
                    comm_data <- data[data$community_id == comm_id, ]
                    
                #   Combine abstracts
                    combined_text <- paste(comm_data$text_theme, collapse = " ")
                    abstract_count <- nrow(comm_data)
                    
                #   Add to results
                    community_themes <- rbind(community_themes, data.frame(
                        community_id = comm_id,
                        combined_abstracts = combined_text,
                        abstract_count = abstract_count,
                        stringsAsFactors = FALSE
                    ))
            }
        
        #   Return Consolidated Abstracts by Community
            return(community_themes)
    }

#   Helper Function: Text Truncation 
    truncate_text <- function(text, max_chars = 8000) {
        #   Check if the prompt exceeds a Max Text Limit
            if (nchar(text) <= max_chars) return(text)
    
        #   Truncate to max_chars
            truncated <- substr(text, 1, max_chars)
        
        #   Try to end at a complete sentence
            last_period <- max(gregexpr("\\.", truncated)[[1]])
            if (last_period > max_chars * 0.8) {
                truncated <- substr(truncated, 1, last_period)
            }
        
        #   Report truncation
            cat("    Truncated text from", nchar(text), "to", nchar(truncated), "characters\n")
        
        #   Return Formatted Text
            return(truncated)
    }

#   Helper Function to Determine Stopping Points to Avoid Time Out Errors
    calculate_timeout <- function(abstract_count, char_count, max_timeout = 1200) {
        base_timeout <- 300  # 5 minutes base
        char_factor <- char_count / 1000 * 10  # 10 seconds per 1000 chars
        abstract_factor <- abstract_count * 15  # 15 seconds per abstract
    
        timeout <- base_timeout + char_factor + abstract_factor
        return(min(timeout, max_timeout))  # Cap at max_timeout
    }

#   Function to Generate Community Themes
    generate_community_themes <- function(raw_data, max_chars = 8000, max_timeout = 1200) {
        #   Ensure Ollama is running
            if (!ensure_ollama_running()) {
                stop("Could not start Ollama service")
            }
    
        #   Prepare data by community
            community_data <- prepare_community_data(raw_data)
            cat("Found", nrow(community_data), "unique communities\n")
    
        #   Load required libraries
            library("httr")
            library("jsonlite")
    
        #   Initialize results
            results <- data.frame(community_id = integer(), theme = character(), processing_time = numeric(), status = character())
    
        #   Process each community
            for(i in 1:nrow(community_data)) {
                #   Setting Initial Start Time
                    start_time <- Sys.time()
                    cat("Processing community", community_data$community_id[i], 
                        "with", community_data$abstract_count[i], "abstracts...\n")
            
                #   Prepare text (with truncation if needed)
                    processed_text <- truncate_text(community_data$combined_abstracts[i], max_chars)
                
                #   Calculate appropriate timeout
                    timeout_seconds <- calculate_timeout(
                        community_data$abstract_count[i], 
                        nchar(processed_text),
                        max_timeout
                    )
                    cat("    Using timeout:", timeout_seconds, "seconds\n")
            
                #   Create prompt
                    prompt <- paste(
                        "You must respond with ONLY a 5-10 word theme name. No explanations, no extra text.\n\n",
                        "Research cluster content (", community_data$abstract_count[i], "related abstracts):\n",
                        processed_text, "\n\n",
                        "Theme:"
                    )
            
                #   Make API call with error handling
                    tryCatch({
                        #   Creating Response
                            response <- POST("http://127.0.0.1:11434/api/generate",
                                        body = list(model = "llama3.1:8b", prompt = prompt, stream = FALSE),
                                        encode = "json",
                                        timeout(timeout_seconds))
                    
                        #   Check if request was successful
                            if (status_code(response) != 200) {
                                stop("API returned status code: ", status_code(response))
                            }
                    
                        #   Extract and clean response
                            theme <- content(response)$response
                            theme <- trimws(theme)
                            theme <- gsub("^(Here is|Here's|The theme is|Theme:|A theme could be):?\\s*", "", theme, ignore.case = TRUE)
                            theme <- gsub("^[\"']|[\"']$", "", theme)
                            theme <- gsub("\n.*", "", theme)
                            theme <- gsub("\\.$", "", theme)
                            theme <- strsplit(theme, "\\.|\\n")[[1]][1]
                            theme <- trimws(theme)
                    
                        #   Calculate processing time
                            processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                    
                        #   Store successful result
                            results <- rbind(results, data.frame(
                                community_id = community_data$community_id[i],
                                theme = theme,
                                processing_time = round(processing_time, 1),
                                status = "success"
                            ))
                            cat("Community", community_data$community_id[i], ":", theme, 
                                "(", round(processing_time, 1), "seconds )\n\n")
                    
                    }, error = function(e) {
                        #   Handle errors (timeout, connection issues, etc.)
                            processing_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                            error_msg <- e$message
                            cat("ERROR for community", community_data$community_id[i], ":", error_msg, "\n")
                        
                        #   Store failed result with error info
                            results <<- rbind(results, data.frame(
                                community_id = community_data$community_id[i],
                                theme = paste("ERROR:", substr(error_msg, 1, 50)),
                                processing_time = round(processing_time, 1),
                                status = "failed"
                            ))
                        cat("Continuing to next community...\n\n")
                    })
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

#   Collapsing Abstracts by Cluster
    community_abstracts <- prepare_community_data(community_data)
    print(community_abstracts[(1:5),])

#   Generating Community Labels & Exporting Era 22 Results
    era22_results <- generate_community_themes(community_abstracts, max_chars = 8000, max_timeout = 1200)
    readr::write_csv(era22_results, file=c("/workspace/caffeine_citation/data/era22_results.csv"))

#   ERA 23

#   Collapsing Abstracts by Cluster
    era_23_community_abstracts <- prepare_community_data(era_23_community_data)
    print(era_23_community_abstracts[(1:5),])

#   Generating Community Labels & Exporting Era 23 Results
    era23_results <- generate_community_themes( era_23_community_abstracts, max_chars = 8000, max_timeout = 1200)
    readr::write_csv(era23_results, file=c("/workspace/caffeine_citation/data/era23_results.csv"))

###################
#   EVALUATIONS   #
###################

#   Pulling-In Community & Theme Data
    community_themes <- community_abstracts_finder(era23_data, 9, era_23_results)

#######################
#   FUNCTION CHECKS   #
#######################

#   Testing that stop_ollama() works
    stop_ollama()

#   Testing if I can start ollama
    ensure_ollama_running()

#   Testing that if Ollama is Running that ensure_ollama_running() Returns the Correct Value
    ensure_ollama_running()
    