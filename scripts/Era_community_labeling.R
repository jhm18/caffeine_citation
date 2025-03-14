#Labeling Communities with OpenAI
#Jonathan H. Morgan, Ph.D. Sarah Delmar
# 14 March 2025

# Clear Out Console Script
  cat("\014")

# Options
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  options(mc.cores = parallel::detectCores())

################
#   PACKAGES   #
################

library(httr)
library(readr)
library(dplyr)
library(jsonlite)
library(parallel)

#################
#   FUNCTIONS   #
#################

# Note the API key is called from the user's .Renviron file in the query to ChatGPT.

# Chunking Function for Large Jobs
  chunk_data_by_community <- function(data, chunk_size) {
    # Split data by community_id
      community_groups <- split(data, data$community_id)
    
    # Create chunks that respect community boundaries
      chunks <- list()
      current_chunk <- data.frame()
      
    for (community_id in names(community_groups)) {
      community_data <- community_groups[[community_id]]
      if (nrow(current_chunk) + nrow(community_data) > chunk_size) {
        # Save the current chunk if it exceeds the size limit
          chunks <- append(chunks, list(current_chunk))
          current_chunk <- community_data
      } else {
        # Add the current community to the chunk
          current_chunk <- rbind(current_chunk, community_data)
      }
    }
    
    # Append the last chunk if it has any data
    if (nrow(current_chunk) > 0) {
      chunks <- append(chunks, list(current_chunk))
    }
    
    return(chunks)
  }

# Community Labeling with ChatGPT
  generate_cluster_name <- function(data, model = "gpt-4", retries = 3) {
    # Function to make a single API request
      make_request <- function(data) {
        # Create a temporary file for the prompt
          temp_file <- tempfile(fileext = ".txt")
        
        # Write the prompt to the file
          prompt_header <- "You are a helpful assistant that generates concise, meaningful names for clusters based on their associated community ID and text themes.
                            For each community, provide a short, descriptive name (2-10 words) that captures the main theme of the text themes for that community.
                            The theme is defined by  the following 3 columns: `title`, `abstract_list`, and `keywords`.
                            Return only the result as a CSV with two columns:
                                                  - `community_id`: The ID of the community.
                                                  - `theme`: The concise name for the cluster.
                            Do not include any extra text, explanations, or markdown formatting. The CSV should have no headers, footers, or commentary.
                            Here is the data to analyze: community_id, title, abstract_list, keywords"
        
          writeLines(prompt_header, temp_file)
          write.table(data, temp_file, append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
          prompt <- paste(readLines(temp_file), collapse = "\n")
          unlink(temp_file)  # Clean up temporary file immediately
            
        # Construct API request
        url <- "https://api.openai.com/v1/chat/completions"
        response <- POST(
          url,
          add_headers(
            "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
            "Content-Type" = "application/json"
          ),
          body = list(
            model = model,
            messages = list(
              list(role = "system", content = "You are a helpful assistant that generates concise, meaningful names for clusters based on their constituent elements and concepts."),
              list(role = "user", content = prompt)
            )
          ),
          encode = "json"
        )
        
        # Handle API response
        if (status_code(response) != 200) {
          stop("API call failed with status: ", status_code(response), "\n", content(response, "text"))
        }
        
        content(response, "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
      }
      
    # Retry logic
    for (i in seq_len(retries)) {
      result <- tryCatch(
        make_request(data),
        error = function(e) {
          if (i == retries) stop("API call failed after ", retries, " retries.")
          Sys.sleep(2^i)  # Exponential backoff
          return(NULL)
        }
      )
      if (!is.null(result)) break
    }
    
    # Extract and parse the response
    response_content <- result$choices$message.content
    response_lines <- unlist(strsplit(response_content, "\n"))
    split_lines <- lapply(response_lines, function(line) strsplit(line, ",")[[1]])
    output_data <- do.call(rbind, split_lines)
    colnames(output_data) <- c("community_id", "theme")
    output_data <- as.data.frame(output_data, stringsAsFactors = FALSE)
    output_data$community_id <- as.integer(output_data$community_id)
    
    # Return Output
    return(output_data)
  }

# NOTES: We need to adjust this function to provide ChatGPT more structure when review the prompt. 
#        We may also need to provide smaller subsets to avoid overwhelming the API.

#################
#  DATA IMPORT  #
#################

# Loading Era 22 for Testing Purposes
  load("/workspace/caffeine_citation/data/era22_prompt.Rda")

# Formatting data
  era22_prompt <- era22_data[c(4,6:8)]
  colnames(era22_prompt)[[1]] <- c("community_id")
  
  #########################
  #   LABELING CLUSTERS   #
  #########################
  
  # Chunking Data
    chunks <- chunk_data_by_community(era22_prompt, chunk_size = 100)
  
  # Running in Parallel
    results <- mclapply(
      chunks[c(1:4)],
      function(chunk) generate_cluster_name(chunk, model = "gpt-4"),
      mc.cores = 4
    )
  
  # Combine results into a single data frame
    final_results <- do.call(rbind, results)
  

