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

#   Function to Generate Community Themes
    generate_community_themes <- function(raw_data) {
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
            results <- data.frame(community_id = integer(), theme = character())
        
        #   Process each community
            for(i in 1:nrow(community_data)) {
                #   Creating Function Terminal Prompt
                    cat("Processing community", community_data$community_id[i], 
                        "with", community_data$abstract_count[i], "abstracts...\n")
            
                #   Create prompt with all abstracts for this community
                    prompt <- paste("You must respond with ONLY a 5-10 word theme name. No explanations, no extra text.\n\n",
                                    "Research cluster content (", community_data$abstract_count[i], "related abstracts):\n",
                                    community_data$combined_abstracts[i], "\n\n",  # This IS the content
                                    "Theme:")
            
                #   Make API call
                    response <- POST(
                        "http://127.0.0.1:11434/api/generate",
                        body = list(
                            model = "llama3.1:8b",
                            prompt = prompt,
                            stream = FALSE
                        ),
                        encode = "json"
                    )
            
                #   Extract and clean response
                    theme <- content(response)$response
                    theme <- trimws(theme)
                    theme <- gsub("^(Here is|Here's|The theme is|Theme:|A theme could be):?\\s*", "", theme, ignore.case = TRUE)
                    theme <- gsub("^[\"']|[\"']$", "", theme)
                    theme <- gsub("\n.*", "", theme)
                    theme <- gsub("\\.$", "", theme)
                    theme <- strsplit(theme, "\\.|\\n")[[1]][1]
                    theme <- trimws(theme)
            
                #   Store result
                    results <- rbind(results, data.frame(
                        community_id = community_data$community_id[i],
                        theme = theme
                    ))
            
                #   Show progress
                    cat("Community", community_data$community_id[i], ":", theme, "\n\n")
            }
        
        #   Return Community Themes
            return(results)
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

#   Loading Era 22 for Testing Purposes
    load("/workspace/caffeine_citation/data/era22_prompt.Rda")

#   Formatting data (May Back Keywords and Title Later)
    era22_prompt <- era22_data[c(4,6:8)]
    colnames(era22_prompt)[[1]] <- c("community_id")
    community_data <- data.frame(community_id = era22_prompt$community_id, text_theme = as.character(era22_prompt$abstract_list))

##########################
#   COMPARING CLUSTERS   #
##########################

#   Collapsing Abstracting by Cluster
    community_abstracts <- prepare_community_data(community_data)
    print(community_abstracts[(1:5),])





#######################
#   FUNCTION CHECKS   #
#######################

#   Testing that stop_ollama() works
    stop_ollama()

#   Testing if I can start ollama
    ensure_ollama_running()

#   Testing that if Ollama is Running that ensure_ollama_running() Returns the Correct Value
    ensure_ollama_running()
    