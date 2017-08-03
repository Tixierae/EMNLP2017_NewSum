cores_dec = function(g, degeneracy, directed_mode){
  
  operating_system = .Platform$OS.type
  
  if (degeneracy == "weighted_k_core"){
    
    if (directed_mode == "all"){
      my_number = 0
    } else if (directed_mode == "in") {
      my_number = 1
    } else {
      my_number = 2
    }
    
    command = paste('java -jar kcore-1.0.jar edgelist.txt', my_number)
    command_unix = paste('java -jar kcore-1.0.jar edgelist.txt', my_number)
    
    if (operating_system=="unix"){
      
      # calls to a Linux environment - shinyapps
      system(paste0(command_unix))
      
    } else if (operating_system=="windows"){
      
      system(paste0(command), intern = FALSE, wait = TRUE)
      
    }
    
    file_read = read.csv("edgelist_cores.csv", header=FALSE, stringsAsFactors = FALSE, sep=" ")
    file_read = file_read[,-2]
    
    cores_g = as.numeric(file_read[,2])
    names(cores_g) = as.character(file_read[,1])
    
  } else  if (degeneracy == "unweighted_k_core") {
    
    # k-core decomposition for unweighted graphs
    # based on Batagelj and Zaversnik's (2002) algorithm #1
    cores_g = graph.coreness(g, mode = directed_mode)
    
  } else  if (degeneracy == "k_truss"){
    
    # k-truss decomposition
    # based on Wang and Cheng (2012) algorithm #2
    
    # create input text file
    to_input = get.edgelist(g, names=FALSE) # we want vertice IDs
    to_input = rbind(c(vcount(g), ecount(g)) ,to_input)
    write.table(to_input,file="edgelist_k_truss.txt",row.names=FALSE, col.names=FALSE, sep=" ")
    
    # run k_truss decomposition
    
    if (operating_system=="unix"){
      
      # calls to a Linux environment - shinyapps
      system("./ktruss.out edgelist_k_truss")
      
    } else if (operating_system=="windows"){
      
      system(paste("cmd.exe /c", "k_truss.exe edgelist_k_truss"), intern = FALSE, wait = TRUE)
      
    }
    
    # read output text file back in
    lines = readLines("edgelist_k_truss-out.txt")
    
    # only retain the lines of interest (truss number of each edge)
    lines_split = lapply(lines, function(x){unlist(strsplit(x, split=" "))})
    lines_split_sizes = unlist(lapply(lines_split, length))
    to_retain = which(lines_split_sizes==1)
    lines_retained = lines[to_retain]
    
    first_line = unlist(strsplit(unlist(lines_split[1]), split="\\."))
    lines_retained = c(unlist(strsplit(first_line[length(first_line)],split="n"))[2],lines_retained)
    
    # structure into data frame (source, target, truss number)
    truss_numbers_df = read.table(text = gsub("[[:punct:]]", " ", grep(",", lines_retained, value = TRUE)))
    
    # convert node IDs into node names
    my_names = V(g)$name
    
    truss_numbers_df_names = t(apply(truss_numbers_df,1,function(x){c(my_names[x[1]],my_names[x[2]],as.numeric(x[3]))}))
    
    # add the 2-truss (the graph itself)
    edge_list_g = get.edgelist(g)
    two_truss = cbind(edge_list_g,rep(2,nrow(edge_list_g)))
    full_truss_numbers_df_names = rbind(two_truss,truss_numbers_df_names)
    
    # compute node truss numbers (max k of the k-trusses the node belongs to)
    node_truss_numbers = unlist(lapply(my_names, function(x){max(as.numeric(full_truss_numbers_df_names[c(which(full_truss_numbers_df_names[,1]==x),which(full_truss_numbers_df_names[,2]==x)),3]))}))
    
    # for compatibility with the other subfunctions
    cores_g = node_truss_numbers
    names(cores_g) = my_names
    
  } else if (degeneracy == "weighted_page_rank"){
    
    # the name is "cores_g" for consistency with the other subfunctions
    cores_g = round(page_rank(g, directed = directed, damping = 0.85, weights = as.numeric(E(g)$weight))$vector,3)
    
  }
  
  # sort vertices by decreasing core number
  cores_g = sort(cores_g, decreasing = TRUE)
  
  output = list(cores = cores_g)
  
}