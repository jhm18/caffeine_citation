#Pajek Functions: read_net, write_net, read_clu, write_clu, read_vect, write_vec
#Jonathan H. Morgan
#30 April 2023

################################
#   BASE FUNCTIONS AND LISTS   #
################################

# Utilities
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Indicating to the user what network files, partition files, and vector files they have in thier directory
  Net_Files = list.files(pattern="*.net")

  Partition_Files = list.files(pattern="*.clu")

  Vector_Files = list.files(pattern="*.vec")

################
#   read_net   #
################

# Checking Network Files
  print("Network Files in Your Directory")
  Net_Files

# Reading in Network
  read_net <- function(net_file) {
    # Pulling-In Network File
      net <- readLines(net_file)
  
    # Scanning for Lines with *
      astrisk_lines <- vector("logical", length(net))
      for(i in seq_along(astrisk_lines)){
        astrisk_lines[[i]] <- grepl("*", net[[i]], fixed = TRUE)
      }
      star_index <- cbind(as.data.frame(seq(1, length(astrisk_lines), 1)), astrisk_lines)
      colnames(star_index)[[1]] <- c("Obs_ID")
  
    # Creating Cut Points 
      cut_points <- star_index[(star_index[,2] == TRUE),]
      if(nrow(cut_points) > 2){
        types_index <- cut_points$Obs_ID
        cut_points <- cut_points[[1]]
        end_points <- cut_points[-c(1)]
        end_points <- end_points - 1
        end_points <- c(end_points, length(net))
        cut_points <- cbind(as.data.frame(cut_points), end_points)
        cut_points[,1] <- cut_points[,1] + 1
      }else{
        types_index <- cut_points$Obs_ID
        cut_points <- cut_points[[1]]
        start_points <- cut_points + 1
        end_points <- c(cut_points[[2]] - 1, length(net))
        cut_points <- data.frame(cut_points = start_points, end_points = end_points)
      }
      
    # Getting Types
      types <- vector('character', length(types_index))
      for(i in seq_along(types)){
        types[[i]] <- strsplit(net[types_index[[i]]], ' ')[[1]][[1]]
      }
      cut_points$type <- types 
  
    # Extracting the nodes list from the network file
      nodes_list <- vector('list', length(types[(types == "*Vertices")]))
      node_cut_points <- cut_points[(cut_points[,3] == "*Vertices"),]
      for(i in seq_along(nodes_list)){
        # Isolating node cut-points
          vertices_cut_points <- node_cut_points[i,]
    
        # Isolating Nodes Information
          nodes_list[[i]] <- net[c(vertices_cut_points[[1]]:vertices_cut_points[[2]])]
          nodes_list[[i]] <- trim(nodes_list[[i]])
    
        # Splitting Values Out
          nodes_list[[i]] <- strsplit(as.character(nodes_list[[i]]),'n/')
          for(j in seq_along(nodes_list[[i]])){
            element <- strsplit(as.character(nodes_list[[i]][[j]]),'"')[[1]] 
            if(length(element) > 2) {
              supplemental_data <- strsplit(element[[3]],' ')[[1]] 
              supplemental_data <- supplemental_data[supplemental_data != ""]
              element <- c(element[[1]], element[[2]], supplemental_data)
              rm(supplemental_data)
            } else{
              if(length(element) < 2){
                element <- strsplit(as.character(nodes_list[[i]][[j]]),' ')[[1]]
                element <- element[element != ""]
              }else{
                element <- element
              }
            }
      
            nodes_list[[i]][[j]] <- element
          }
    
        # Cleaning-Up Split
          nodes_list[[i]] <- lapply(nodes_list[[i]], function(x) x[x != ""])
    
        # Isolating Shape Information
          shapes <- nodes_list[[i]]
          for (j in seq_along(shapes)){
            shapes[[j]] <- nodes_list[[i]][[j]][-c(1:5)]
          }
    
          shapes <- lapply(shapes, function(x) paste(x,collapse=" "))
    
        # Transforming Nodes into a Matrix
          for (j in seq_along(nodes_list[[i]])){
            nodes_list[[i]][[j]] <- nodes_list[[i]][[j]][1:5]
          }
    
          nodes_list[[i]] <-  as.data.frame(matrix(unlist(nodes_list[[i]]), nrow = length(nodes_list[[i]]), byrow = TRUE), stringsAsFactors = FALSE)
          colnames(nodes_list[[i]]) <- c('ID', 'Label', 'x-coord', 'y-coord', 'z-coord')
    
        # Transforming Shape Data into a Matrix
          shapes <-  as.data.frame(matrix(unlist(shapes), nrow = length(shapes), byrow = TRUE),  stringsAsFactors = FALSE)
          colnames(shapes) <- c('shapes information')
    
        # Binding Shape Data to Nodes List
          nodes_list[[i]] <- cbind(nodes_list[[i]], shapes)
    
        # Removing shape information if there no information
          if(sum(nodes_list[[i]]$`shapes information` == "") > 1){
            nodes_list[[i]] <- nodes_list[[i]][-c(ncol(nodes_list[[i]]))]
          }else{
            nodes_list[[i]] <- nodes_list[[i]][,]
          }
          rm(shapes, j)          
      } 
  
    # Creating Edges File
      edges_list <- vector('list', length(types[(types != "*Vertices")]))
      edge_cut_points <- cut_points[(cut_points[,3] != "*Vertices"),]
      for(i in seq_along(edges_list)){
        # Isolating edge cut-points
          tie_cut_points <- edge_cut_points[i,]
    
        # Isolating Nodes Information
          edges_list[[i]] <- net[c(tie_cut_points[[1]]:tie_cut_points[[2]])]
          edges_list[[i]] <- trim(edges_list[[i]])
    
        # Splitting Values Out
          edges_list[[i]] <- strsplit(as.character(edges_list[[i]]),'n/')
          
        # Checking for Stem-Leaf Format
          edge_element_strings <- lapply(edges_list[[i]], function(x) strsplit(as.character(x),' ')[[1]])
          edge_element_strings <- lapply(edge_element_strings, function(x) x[x != ""])
          edge_element_lengths <- lapply(edge_element_strings, function(x) length(x))
          edge_element_lengths <- unique(as.integer(edge_element_lengths))
   
          if(length(edge_element_lengths) > 1){
            # Splitting by Element to Account for Stem-Leaf Notation
              ties__list <- vector('list', length(edges_list[[i]]))
              for(j in seq_along(edges_list[[i]])){
                # Identify Stem & Leaves
                  stem_leaves <- base::strsplit(edges_list[[i]][[j]], ' ')[[1]]
                  stem <- stem_leaves[[1]]
                  leaves <- stem_leaves[2:length(stem_leaves)]
              
                # Transforming into DataFrame
                  stem <- rep(stem, length(leaves))
                  weights <- rep(1, length(leaves))
                  ties__list[[j]] <- data.frame(person_j = stem, person_i = leaves, weight = weights)
              }
              
            # Binding List into a Single DataFrame
              edges_list[[i]] <- do.call('rbind', ties__list)
              colnames(edges_list[[i]])[c(1:3)] <- c('Person i', 'Person j', 'Weight')
              
            # Adding Type Type information
              tie_type <- tie_cut_points[[3]]
              edges_list[[i]]$`Tie Type` <- tie_type
          }else{
            # Splitting Values Out
              edges_list[[i]] <- strsplit(as.character(edges_list[[i]]),' ') 
              
            # Cleaning-Up Split
              edges_list[[i]] <- lapply(edges_list[[i]], function(x) x[x != ""])
              
            # Transforming into a Matrix
              edges_list[[i]] <-  as.data.frame(matrix(unlist(edges_list[[i]]), nrow = length(edges_list[[i]]), byrow = TRUE), stringsAsFactors = FALSE)
              colnames(edges_list[[i]])[c(1:3)] <- c('Person i', 'Person j', 'Weight')
              
            # Removing edge color information as it adds little value when importing data
              edges_list[[i]] <- edges_list[[i]][-c(4:5)]
              
            # Adding Type Type information
              tie_type <- tie_cut_points[[3]]
              edges_list[[i]]$`Tie Type` <- tie_type
          }
      }
  
    # Collapsing Nodes List & Writing-Out Vertices File
      nodes <- do.call("rbind", nodes_list)
      if(length(nodes_list) > 1){
        # Getting length necessary for ID vectors
          node_lengths <- lapply(nodes_list, function(x) nrow(x))
    
        # Creating ID vectors
          id_vectors <- vector('list', length(node_lengths))
          for (i in seq_along(node_lengths)){
            id_vectors[[i]] <- rep(i, node_lengths[[i]])
          }
    
        # Stacking IDs into a Common Vector
          id_vector <- unlist(id_vectors)
    
        # Adding ID vector
          nodes$data_id <- id_vector
      }else{
        nodes <- nodes
      }
  
    # Defining Type Correction Function
      type_setter <- function(data_type){
        for(i in seq_along(colnames(data_type))){
          # Checking if Numeric
            if(sum(grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", data_type[[i]])) == length(data_type[[i]])){
              # Converting Column to Numeric
                data_type[[i]] <- as.numeric(data_type[[i]])
        
              # Checking if Integer
                if(sum(!is.na(as.numeric(data_type[[i]]))) == length(data_type[[i]])){
                  data_type[[i]] <- as.integer(data_type[[i]])
                }else{
                  data_type[[i]] <- data_type[[i]]
                }
            }else{
              # Keeping Type
                data_type[[i]] <- data_type[[i]]
            }
        }
    
        return(data_type)
      }
  
    # Fixing Node File Types
      nodes <- type_setter(nodes)
  
    # Outputting Vertices
      vertices <-  assign(x = "vertices", value = nodes,.GlobalEnv) 
  
    # Collapsing Edges List & Writing-Out Edges File
      edges <- do.call("rbind", edges_list)
      if(length(edges_list) > 1){
        # Getting length necessary for ID vectors
          edge_lengths <- lapply(edges_list, function(x) nrow(x))
    
        # Creating ID vectors
          id_vectors <- vector('list', length(edge_lengths))
          for (i in seq_along(edge_lengths)){
            id_vectors[[i]] <- rep(i, edge_lengths[[i]])
          }
    
        # Stacking IDs into a Common Vector
          id_vector <- unlist(id_vectors)
    
        # Adding ID vector
          edges$data_id <- id_vector
      }else{
        edges <- edges
      }
  
    # Fixing Node File Types
      edges <- type_setter(edges)
  
    # Outputting Edges
      ties <-  assign(x = "ties", value = edges,.GlobalEnv) 
  }

  #read_net(Net_Files[[5]])

##################
#    write_net   #
##################

write_net <- function(tie_type, data_id, x_coord, y_coord, z_coord, 
                      node_color, node_border, 
                      `person i`, `person j`, weight, tie_color, 
                      net_name, `sort and simplify`) {
  # Generating Meta-Data
    vertices <- c('*Vertices')
    substr(tie_type, 1, 1) <- toupper(substr(tie_type, 1, 1))
    tie_type <- paste0('*', tie_type)
  
    `sort and simplify` <- as.logical(`sort and simplify`)
  
  # Preparing nodelist
    nodelist <- as.data.frame(data_id, stringsAsFactors = FALSE)
    id <- as.numeric(row.names.data.frame(nodelist))
    nodelist <- as.data.frame(cbind(id, nodelist))
  
  # Adding Coordinates data
    x <- as.character(x_coord)
    y <- as.character(y_coord)
    z <- as.character(z_coord)
  
  # Adding Color Information
    substr(node_color, 1, 1) <- trim(toupper(substr(node_color, 1, 1)))
    substr(node_border, 1, 1) <- trim(toupper(substr(node_border, 1, 1)))
    substr(tie_color, 1, 1) <- trim(toupper(substr(tie_color, 1, 1)))
  
  # Checking that ic and bc are either 1 or the same length as the vertices
    if (length(node_color) != length(data_id) & length(node_color) > 1){
      node_color <- ' '}
    if (length(node_color) != length(data_id) & sum(nchar(node_color) - nchar(gsub('\\,', '', node_color))) > 0){
      node_color <- ' '}
    if (length(node_color) != length(data_id) & sum(grepl("[[:alnum:]]", node_color) == TRUE) == 0){
      node_color <- ' '
    } else {
      node_color <- paste('ic', node_color) 
    }
  
    if (length(node_border) != length(data_id) & length(node_border) > 1){
      node_border <- ' '}
    if (length(node_border) != length(data_id) & sum(gregexpr(",", node_border, fixed=TRUE)[[1]] > 0) > 0){
      node_border <- ' '}
    if (length(node_border) != length(data_id) & sum(grepl("[[:alnum:]]", node_border) == TRUE) == 0){
      border_color <- ' '
    } else {
      node_border <- paste('bc', node_border) 
    }
  
  # Joining colors to nodelist
    nodelist$ic <- node_color
    nodelist$bc <- node_border
  
    rm(node_color, node_border)
  
  # Checking that strength is a non-blank value, if blank supply a tie strength of 1
    if (length(weight) == 1 & sum(grepl("[[:alnum:]]", weight) == TRUE) != length(weight)) {
      weight <- 1
    } else {
      weight <- weight
    }
  
  # Preparing edgelist
    sender <- as.numeric(`person i`)
    target <- as.numeric(`person j`)
    strength <- as.numeric(weight)
  
  # Creating ties list
    ties <- as.data.frame(cbind(sender, target, strength), stringsAsFactors = FALSE)
  
  # Sorting by sender and target is specified by user
    if (`sort and simplify` == TRUE) {
      ties <- ties[order(ties[,1], ties[,2] ), ]
    } else {
      ties <- ties
    }
  
  # Checking for duplicated ties and deleting them if there are any
    if (`sort and simplify` == TRUE) {
      ties <- ties [!duplicated(ties[c(1,2)]),]
    } else {
      ties <- ties
    }
  
  # Transforming to Characeter
    sender <- as.character(`person i`)
    target <- as.character(`person j`)
    strength <- as.character(weight)
  
  # Performing length checks and assigning tie color
    if (length(tie_color) != length(`person i`) & length(tie_color) > 1){
      tie_color <- ' '}
    if (length(tie_color) != length(`person i`) & sum(gregexpr(",", tie_color, fixed=TRUE)[[1]] > 0) > 0){
      tie_color <- ' '}
    if (length(tie_color) != length(`person i`) & sum(grepl("[[:alnum:]]", tie_color) == TRUE) == 0){
      tie_color <- ' '
    } else {
      tie_color <- paste('c', tie_color) 
    }
  
  # Joining color to ties
    ties$c <- tie_color
    rm(tie_color)
  
  # Create label
    label <- paste0('"', nodelist$data_id , '"')  
  
  # Moving into Pajek format
    pajek_list <- vector("list", 4)
    pajek_list[[1]] <- paste(vertices, length(id))
    pajek_list[[2]] <- paste(id, label, x, y, z, nodelist$ic, nodelist$bc, sep= ' ')
    pajek_list[[2]] <- trim(pajek_list[[2]])
    pajek_list[[3]] <- tie_type
    pajek_list[[4]] <- paste(ties[[1]], ties[[2]], ties[[3]], ties[[4]], sep = ' ')
  
  # Outputting as a large character
    vertices <- pajek_list[[1]]
    nodelist <- pajek_list[[2]]
    tie_type <- pajek_list[[3]]
    edges <- pajek_list[[4]]
  
    nodelist <- append(vertices, nodelist)
    edges <- append(tie_type, edges)
    pajek_list <- append(nodelist, edges)
  
    rm(vertices, tie_type, nodelist, id, x, y, z, sender, target, strength, ties, label)
  
  # Writing Files
    fileConn <- file(paste0(net_name, ".net"))
    writeLines(pajek_list, fileConn)
    close(fileConn)
}

# Write_net Parameters
#   Tie Type: Arcs or Edges
#   The Id from your data you are using. The function will write a sequential Pajek ID based on row number.
#   x_coord: Provide the vector if you have coordinates or simply write an empty a blank, ' '. 
#   y_coord: Same as x
#   z_coord: Same as x and y
#   node_color: Specifies the vertex's fill color
#   node_border: Specifies the vertex's border color
#   `person i`: Specify who person i is or in a directed network who the sender is.
#   `person j`: Specify who person j is or in a directed network the target is.
#   weight:  Specify the vector with the tie weights, if the ties have no weights input 1 (Pajek's default)
#   tie_color: Specifies the color of the ties.
#   net_name: Specify the file name of the output file. The file will be written to your work directory. The function will specify that it it is a .net file.

# write_net('Edges', nodes$Label, nodes$`x-coord`, nodes$`y-coord`, nodes$`z-coord`, 'blue', 'white', ties$`Person i`, ties$`Person j`, ties$Weight, 'gray', 'test_net')

################
#   read_clu   #
################

print("Partition Files in Your Directory")
Partition_Files

read_clu <- function(directory=getwd(), clu_file = "") {
  # Setting work directory to directory
    setwd(directory)  
  
  # Determine if a single partition or all partitions in the directory will be read-in
    if (clu_file != ""){
      # Creating full file name
        clu_file_name <- paste0(directory,"/",clu_file)
        
      # Check if file contains correct file designation 
        if(grepl(".clu", clu_file, fixed = TRUE) == TRUE){
          clu_file_name <- clu_file_name
        }else{
          clu_file_name <- paste0(clu_file_name, ".clu")
        }
        
      # Read-In File
        partition_file <- as.integer(base::readLines(clu_file_name)[2:length(base::readLines(clu_file_name))])
        
      # Output to the global environment
        network_partition <-  assign(x = "network_partition", value =  partition_file,.GlobalEnv)
    }else{
      # Pulling all partitions from the directory
        Partition_List <- vector("list", length(Partition_Files))
        for (i in seq_along(Partition_List)){
          Partition_List[[i]] <- readLines(Partition_Files[[i]])
        }
  
      # Naming Vector List with file names
        for (i in seq_along(Partition_List)){
          Names <- gsub('.clu', '', Partition_Files)
          names(Partition_List)[[i]] <- paste0(Names[[i]])
        }
  
        vertices <- vector("list", length(Partition_List))
        for (i in seq_along(vertices)){
          vertices[[i]] <-  vector("list", 2)
        }
  
      # Moving vertices into list format to assign them a size designator
        for (i in seq_along(vertices)){
          vertices[[i]][[1]] <- Partition_List[[i]][[1]]
          vertices[[i]][[2]] <- Partition_List[[i]][-c(1)]
    
          vertices[[i]][[1]] <- strsplit(as.character(vertices[[i]][[1]]),' ') 
          vertices[[i]][[1]] <- lapply(vertices[[i]][[1]], function(x) x[x != ""])
          vertices[[i]][[1]] <- as.numeric(vertices[[i]][[1]][[1]][[2]])
      }
  
        sizes <- vector("list", length(Partition_Files))
        for (i in seq_along(vertices)){
          sizes[[i]] <- vertices[[i]][[1]]
        }
  
        clu_id <- unique(unlist(sizes))
  
        sizes <- unlist(sizes)
        lengths <- vector("list", length(clu_id))
        for (i in seq_along(lengths)){
          lengths[[i]] <- sum(sizes == clu_id[[i]], na.rm=TRUE)
        }
  
        Partitions <- vector("list", length(clu_id))
        for (i in seq_along(Partitions)){
          obj_name <- paste0("Partititions_", i)  # create a unique R object name that you'll use through the iteration
          id <-  seq(1,clu_id[[i]], by = 1)
          Partitions[[i]] <-  assign(x = obj_name, value = id)   # save iteration to the uniquely named R object
          Partitions[[i]] <- as.data.frame(cbind(clu_id[[i]], Partitions[[i]]))
          colnames(Partitions[[i]]) <- c('Netwwork Size', 'ID')
          names(Partitions)[[i]] <- paste0("Partition_", i)
          rm(id, obj_name, i)
        }
        rm(sizes)
  
        vertices <- lapply(vertices, function(x) x[-c(1)])
        for (i in seq_along(vertices)){
          vertices[[i]] <- vertices[[i]][[1]]
          names(vertices)[[i]] <- paste0(Names[[i]])
        }
  
      # Reading in Separate Partitions by Size that will be merged into the partition data.frames.
        Partition_List <- vector("list", length(clu_id))
        for (i in seq_along(clu_id)){
          Partition_List[[i]] <- vector("list", lengths[[i]])
          names(Partition_List)[[i]] <- paste0("Network_", i)
        }
  
        for (i in seq_along(Partition_List)){
          Partition_List[[i]] <- vertices[lengths(vertices) == clu_id[[i]]]
        }
  
      # Adding Partitions to the Partition Data.Frames
        for (i in seq_along(Partition_List)){
          Partition_List[[i]] <- do.call(cbind.data.frame, Partition_List[[i]])
        }
  
        for (i in seq_along(Partitions)){
          Partitions[[i]] <- cbind(Partitions[[i]], Partition_List[[i]])
        }
  
    # Outputting list as separate data.farmes
      list2env(Partitions,envir=.GlobalEnv)
  
      rm(Partition_List, Partitions, vertices, lengths, clu_id)
  }
}

# read_clu 
# If a specific network partition has been specified using the clu_file argument, read_clu reads that file.
# If no partition has been specified, read_clu reads all partitions in your directory, separates them by size 
# in the event the partition files refer to different networks, 
# and outputs them as separate partition data.frames that can be cbinded to the node list.

#read_clu(directory=getwd(), clu_file = "random_wgt1")
#read_clu()

################
#   read_vec   #
################

print("Vector Files in Your Directory")
Vector_Files

read_vec <- function(directory=getwd(), vec_file = "") {
  # Setting work directory to directory
    setwd(directory)  
  
  # Determine if a single partition or all partitions in the directory will be read-in
    if (vec_file != ""){
      # Creating full file name
        vec_file_name <- paste0(directory,"/",vec_file)
      
      # Check if file contains correct file designation 
        if(grepl(".vec", vec_file, fixed = TRUE) == TRUE){
          vec_file_name <- vec_file_name
        }else{
          vec_file_name <- paste0(vec_file_name, ".vec")
        }
      
      # Read-In File
        vector_file <- as.integer(base::readLines(vec_file_name)[2:length(base::readLines(vec_file_name))])
      
      # Output to the global environment
        network_vector <-  assign(x = "network_vector", value =  vector_file,.GlobalEnv)
    }else{
      # Pulling all vectors from the directory
        Vector_List <- vector("list", length(Vector_Files))
        for (i in seq_along(Vector_List)){
          Vector_List[[i]] <- readLines(Vector_Files[[i]])
        }
      
      # Naming Vector List with file names
        for (i in seq_along(Vector_List)){
          Names <- gsub('.vec', '', Vector_Files)
          names(Vector_List)[[i]] <- paste0(Names[[i]])
        }
      
        vertices <- vector("list", length(Vector_List))
        for (i in seq_along(vertices)){
          vertices[[i]] <-  vector("list", 2)
        }
      
      # Moving vertices into list format to assign them a size designator
        for (i in seq_along(vertices)){
          vertices[[i]][[1]] <- Vector_List[[i]][[1]]
          vertices[[i]][[2]] <- Vector_List[[i]][-c(1)]
        
          vertices[[i]][[1]] <- strsplit(as.character(vertices[[i]][[1]]),' ') 
          vertices[[i]][[1]] <- lapply(vertices[[i]][[1]], function(x) x[x != ""])
          vertices[[i]][[1]] <- as.numeric(vertices[[i]][[1]][[1]][[2]])
        }
      
        sizes <- vector("list", length(Vector_Files))
        for (i in seq_along(vertices)){
          sizes[[i]] <- vertices[[i]][[1]]
        }
      
        clu_id <- unique(unlist(sizes))
      
        sizes <- unlist(sizes)
        lengths <- vector("list", length(clu_id))
        for (i in seq_along(lengths)){
          lengths[[i]] <- sum(sizes == clu_id[[i]], na.rm=TRUE)
        }
      
        Vectors <- vector("list", length(clu_id))
        for (i in seq_along(Vectors)){
          obj_name <- paste0("Vectors_", i)  # create a unique R object name that you'll use through the iteration
          id <-  seq(1,clu_id[[i]], by = 1)
          Vectors[[i]] <-  assign(x = obj_name, value = id)   # save iteration to the uniquely named R object
          Vectors[[i]] <- as.data.frame(cbind(clu_id[[i]], Vectors[[i]]))
          colnames(Vectors[[i]]) <- c('Netwwork Size', 'ID')
          names(Vectors)[[i]] <- paste0("Vector_", i)
          rm(id, obj_name, i)
        }
        rm(sizes)
      
        vertices <- lapply(vertices, function(x) x[-c(1)])
        for (i in seq_along(vertices)){
          vertices[[i]] <- vertices[[i]][[1]]
          names(vertices)[[i]] <- paste0(Names[[i]])
        }
      
      # Reading in Separate Vectors by Size that will be merged into the partition data.frames.
        Vector_List <- vector("list", length(clu_id))
        for (i in seq_along(clu_id)){
          Vector_List[[i]] <- vector("list", lengths[[i]])
          names(Vector_List)[[i]] <- paste0("Network_", i)
        }
      
        for (i in seq_along(Vector_List)){
          Vector_List[[i]] <- vertices[lengths(vertices) == clu_id[[i]]]
        }
      
      # Converting Vectors to Doubles 
        for (i in seq_along(Vector_List)){
          Vector_List[[i]] <- lapply(Vector_List[[i]], function(x) as.double(x))
        }
      
      # Adding Vectors to the Vector Data.Frames
        for (i in seq_along(Vector_List)){
          Vector_List[[i]] <- do.call(cbind.data.frame, Vector_List[[i]])
        }
      
        for (i in seq_along(Vectors)){
          Vectors[[i]] <- cbind(Vectors[[i]], Vector_List[[i]])
        }
      
      # Outputting list as separate data.farmes
        list2env(Vectors,envir=.GlobalEnv)
      
        rm(Vector_List, Vectors, vertices, lengths, clu_id)
    }
}

# read_vec 
# If a specific network vector has been specified using the vec_file argument, read_vec reads that file.
# If no partition has been specified, read_vec reads all vectors in your directory, separates them by size 
# in the event the vector files refer to different networks, 
# and outputs them as separate vector data.frames that can be cbinded to the node list.

#read_vec(directory = getwd(), vec_file = "wgt_1_weighted_degree")
#read_vec()

#################
#   write_clu   #
#################

write_clu <- function(cat_variables, var_names) {
  # Creating Names for the files that will be exported based on the column names of the variables if the variables 
  # are from a data.frame or based on the var_names field if they a vector of some kind.
    Names <- character(length=length(cat_variables))
    if (class(cat_variables) == "data.frame") {
      Names <- colnames(cat_variables)
    } else {
      Names <- var_names
    }
  
  # Transforming variables into character varaibl- var_nameses
    if (class(cat_variables) == "data.frame") {
      cat_variables <- lapply(cat_variables, function(x) as.character(x))
    } else {
      cat_variables <- as.character(cat_variables)
    }
  
  # Creating meta-information
    vertices <- c('*Vertices')
  
  # Pulling file lengths to specify the files
    if (class(cat_variables) == "list") {
      lengths <- vector("list", length(cat_variables))
    } else {
      lengths <- 1
    }
  
    for (i in seq_along(cat_variables)){
      if (class(cat_variables) == "list") {
        lengths[[i]] <- length(cat_variables[[i]])
      } else {
        lengths <- length(cat_variables)
      }
    }
  
  # Creating Meta Data
    meta_list <- vector("list", length(lengths))
    for (i in seq_along(lengths)){
      meta_list[[i]] <- paste(vertices, lengths[[i]]) 
    }
  
  # Moving into Pajek format
    pajek_list <- vector("list", length(lengths))
    for (i in seq_along(pajek_list)){
      if (class(cat_variables) == "list") {
        pajek_list[[i]] <- append(meta_list[[i]], cat_variables[[i]]) 
      } else {
        pajek_list[[i]] <- unlist(append(meta_list, cat_variables))
      }
    }
    rm(lengths, meta_list, vertices) 
  
  # Checking names to ensure there are no unamed files
    for (i in seq_along(Names)){
      if (grepl("[[:alnum:]]", Names[[i]]) == FALSE) {
        Names <- paste0('partition_', i)
      }else {
        Names[[i]] == Names[[i]]
      }
    }
  
  # Writing Files
    fileConn <- vector("list", length(pajek_list))
    for (i in seq_along(fileConn)){
      fileConn[[i]] <- file(paste0(Names[[i]], ".clu"))
      writeLines(pajek_list[[i]], fileConn[[i]])
      close(fileConn[[i]])
    }
}

# write_clu
# Writes categorical variables such as gender and race out as Pajek partition files.
# The function supports writing partition files based on a subset of variables from a data.frame.
# If writing partitions from a data.frame, the function will retain the variable names from the dataset.
# The function also supports writing clu files from vectors. To write-out multiple vectors at one time,
# simply move the function into a list.

#write_clu(Partition_1[[3]], 'test_clu')

#################
#   write_vec   #
#################

write_vec <- function(con_variables, var_names) {
  # Creating Names for the files that will be exported based on the column names of the variables if the variables 
  # are from a data.frame or based on the var_names field if they a vector of some kind.
    Names <- character(length=length(con_variables))
    if (class(con_variables) == "data.frame") {
      Names <- colnames(con_variables)
    } else {
      Names <- var_names
    }
  
  # Transforming variables into character varaibl- var_nameses
    if (class(con_variables) == "data.frame") {
      con_variables <- lapply(con_variables, function(x) as.character(x))
    } else {
      con_variables <- as.character(con_variables)
    }
  
  # Creating meta-information
    vertices <- c('*Vertices')
  
  # Pulling file lengths to specify the files
    if (class(con_variables) == "list") {
      lengths <- vector("list", length(con_variables))
    } else {
      lengths <- 1
    }
  
    for (i in seq_along(con_variables)){
      if (class(con_variables) == "list") {
        lengths[[i]] <- length(con_variables[[i]])
      } else {
        lengths <- length(con_variables)
      }
    }
  
  # Creating Meta Data
    meta_list <- vector("list", length(lengths))
    for (i in seq_along(lengths)){
      meta_list[[i]] <- paste(vertices, lengths[[i]]) 
    }
  
  # Moving into Pajek format
    pajek_list <- vector("list", length(lengths))
    for (i in seq_along(pajek_list)){
      if (class(con_variables) == "list") {
        pajek_list[[i]] <- append(meta_list[[i]], con_variables[[i]]) 
      }else {
        pajek_list[[i]] <- unlist(append(meta_list, con_variables))
      }
    }
    rm(lengths, meta_list, vertices) 
  
  # Checking names to ensure there are no unamed files
    for (i in seq_along(Names)){
      if (grepl("[[:alnum:]]", Names[[i]]) == FALSE) {
        Names <- paste0('vector_', i)
      }else {
        Names[[i]] == Names[[i]]
      }
    }
  
  # Writing Files
    fileConn <- vector("list", length(pajek_list))
    for (i in seq_along(fileConn)){
      fileConn[[i]] <- file(paste0(Names[[i]], ".vec"))
      writeLines(pajek_list[[i]], fileConn[[i]])
      close(fileConn[[i]])
    }
}

# write_vec
# Writes continuous variables such as size or income out as Pajek vector files.
# The function operates in the same way as write_clu.

#write_vec(Vector_2[[3]], ' ')