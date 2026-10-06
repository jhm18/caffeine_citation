#Sarah Ricupero & Jonathan H. Morgan
#Data Compilation Script
#20 April 20201

#Clear Out Console Script
  cat("\014")

# Setting Work Directory: Sarah
  setwd("C:/Users/sarah/Documents/CaffeineSNA")
  getwd()

# Setting Working Directory: Jon
  setwd("/Users/jonathan.h.morgan/Desktop/Personal/ACS/Caffeine_Citation Analysis/Data_Scripts/CaffeineSNA")
  getwd()

# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())

################
#   PACKAGES   #
################


################
#  FUNCTIONS   #
################

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

######################
#   IMPORTING DATA   #
######################

# Specifying Simple File Import Function
  file_import = function () {
      directory_list = list.files(getwd(), pattern="*.txt")
  
    # Crating an Input List for each Sub-Folder and getting the list of files in each folder
      files <- vector('list', length(directory_list))
      for (i in seq_along(files)) {
        files[[i]] <- readLines(directory_list[[i]])
      }
    return(files)
  }
  
  files <- file_import()

#######################
#   FORMATTING DATA   #
#######################
  
# i: Index number for files
# j: Index number for articles
# k: Index number for article elements
  # Create output list
  file_outputs <- vector('list', length(files))
  directory_list <- list.files(getwd(), pattern="*.txt")
  names(file_outputs) <-  gsub('.txt','',directory_list)
  for(i in seq_along(file_outputs)){
    # Isolating the file and transforming it from a Character String into a List
      file <- files[[i]]
      file <- base::strsplit(file, " ")
    
    # Subdividing list by Articles and eliminating elements that do not demarcate the articles
      articles <- vector('integer', length(file))
      for(j in seq_along(file)){
        iter <- j
        if (length(file[[j]]) != 0) {
          if (file[[j]][[1]] == "PT") {
            articles[[j]] <- iter
          }else{
            articles[[j]] <- 0
          }
        }else{
          articles[[j]] <- articles[[j]]
        }
        rm(iter)
      }
      articles <- articles[articles != 0]

    # Making an Index of Articles
      x <- articles
      x <- x[-c(length(x))]
      x <- c(1, x)
      y <- articles
      y <- y[-c(1)]
      y <- c(2, y)
    
      article_index <- as.data.frame(cbind(x,y))
      article_index$y <- y - 1
      lastrow <- c(articles[length(articles)],length(file))
      article_index <- rbind(article_index,lastrow)
      rm(x, y)
      
    # Moving Articles into a List
      articles_list <- vector('list', nrow(article_index))
      for (j in seq_along(articles_list)) {
        articles_list[[j]] <- file[article_index[j,1]:article_index[j,2]]
      }
      articles_list <- articles_list[-c(1)]
      names(articles_list) <- seq(1,length(articles_list), 1)
      
    # Create output list
      output_taglist <- vector('list', length(articles_list))
      names(output_taglist) <- names(articles_list)
  
      for(j in seq_along(articles_list)){
        # Identifying Tags & IDs
          tags <- vector('character', length(articles_list[[j]]))
          for(k in seq_along(tags)){
            tags[[k]] <- articles_list[[j]][[k]][1]
          }
    
          tags_index <- as.data.frame(cbind(seq(1, length(tags), 1), tags))
          colnames(tags_index)[[1]] <- c('tag_id')
          tags_index$tag_id <- as.integer(tags_index$tag_id)
    
        # Identifying Blocks to Iterate Across
          tag_blocks <- unique(tags)
          tag_blocks <- tag_blocks[(tag_blocks != "")]
          tag_list <- vector('list', length(tag_blocks))
          names(tag_list) <- tag_blocks
    
        # Creating Index to Populate Blocks
          tag_index <- tags_index[tags_index$tags %in% tag_blocks, ]

          x <- tag_index$tag_id
          x <- x[-c(length(x))]
          x <- c(x, tag_index[nrow(tag_index), 1])
          y <- tag_index$tag_id
          y <- y[-c(1)]
          y <- y-1
          y <- c(y,  length(articles_list[[j]]))
          
          index <- as.data.frame(cbind(x,y))
          index$Check <- index$y - index$x
          
        # Remove blank characters from article list
          article <- articles_list[[j]]
          for(m in 1:length(article)){
            article[[m]] <- article[[m]][article[[m]] != ""]
          }
          
        # Populating Tags List: ER is a Spacing Variable (End of Record)
          for(k in seq_along(tag_list)) {
            if(index[k,3] == 0 ){
              tag_list[[k]] <- article[index[k,2]]
            }else{
              tag_list[[k]] <- article[c(index[k,1]:index[k,2])]
            }
          }
          
          #Export taglist to output
          output_taglist[[j]] <- tag_list
          
          rm(tag_index, tags, tag_blocks, index, x, y, tags_index, tag_list, article)
      }
      
    # Populate file output list
      file_outputs[[i]] <- output_taglist
          
      rm(articles, article_index)
    }
  
  save(file_outputs, file = "caffeine_articles_20April2021.Rda")
  
# Note: The Function thus far looks okay but we should verify with the first article record in the first file.
#       We can then strip out all these blank characters to get a better sense of what we are looking at.
#       And, we need to decide how we ultimately want to organizes the data. Maybe, separate lists by element with each element of the list
#       corresponding to an article.  The classic aproach is to make a tabular dataset with really ugly text variables; but, that' shard to iterate.
