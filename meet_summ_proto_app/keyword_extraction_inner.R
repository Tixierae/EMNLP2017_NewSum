keyword_extraction_inner <-
function(g, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes){
  
  # method can be one of (TR, main, inf, CR, dens)
  
  # if use_percentage is TRUE: percentage is used (real between 0 and 1)
  # otherwise, number_to_retain (integer) - currently not using
  
  # which_nodes is used in coreRank only (for selecting the neighbors), and can be "all", "in", or "out"
  
  # percentage is used for CR everytime (even if use_percentage is FALSE)
  
  if (method=="TR"){
    
    # TextRank
    all_nodes_tr = sort(page_rank(g, directed=FALSE, weights = E(g)$weights)$vector, decreasing=TRUE)
    
    if (use_elbow){
      
      best_cutoff = get_elbow_point(all_nodes_tr)$elbow_point
      
    } else {
      
      if (use_percentage){
        
        best_cutoff = round(length(all_nodes_tr)*percentage)
        
      } else {
        
        best_cutoff = number_to_retain
        
      }
      
    }
    
    extracted_keywords = names(all_nodes_tr)[1:best_cutoff]
	  scores = as.numeric(all_nodes_tr[1:best_cutoff])
    
  } else {
    
    g_data_frame = get.data.frame(g, what="both")
    nodes = g_data_frame$vertices[,c("comm","core_no")]
	# sort data frame by decreasing core number values
    nodes = nodes[order(nodes[,"core_no"],decreasing=TRUE),]
	
    core_numbers = nodes[,"core_no"]
    unique_core_numbers = unique(core_numbers)
    my_levels = unlist(lapply(unique_core_numbers, function(x) length(which(core_numbers==x))))
    names(my_levels) = as.character(unique_core_numbers)
    
    my_levels = my_levels[order(as.numeric(names(my_levels)), decreasing=TRUE)]
    
    if (method=="dens"){
      
      optimal_level = best_level_density(core_numbers, g)$optimal_level
      index_my_level = which(names(my_levels)==optimal_level)
      
      # extract corresponding core
      extracted_keywords = rownames(nodes[1:cumsum(my_levels)[index_my_level],])
	  
	  scores = c()
      names_levels = names(my_levels)
      
      for (h in 1:index_my_level){
        
        scores = c(scores,rep(as.numeric(names_levels[h]),as.numeric(my_levels[h])))
        
      }
      
    }
    
    if (method=="main"){
      
      # extract main core
      extracted_keywords = rownames(nodes[1:my_levels[1],])
      scores = rep(as.numeric(names(my_levels)[1]),as.numeric(my_levels[1]))
      
    }
    
    if (method=="inf"){
      
      # inflexion method
      
      if (length(my_levels)>1){
        
        cumsum_my_levels = cumsum(my_levels)
        first_inflexion_point = which(diff(my_levels)[2:length(diff(my_levels))]<0)[1]
        
        if (!is.na(first_inflexion_point)){
          
          best_level_index = which(names(my_levels)==names(first_inflexion_point))-1
          best_level_inflexion = names(my_levels[best_level_index])
          
        } else {
          
          # use main core
          best_level_index = 1
          best_level_inflexion = names(my_levels[best_level_index])
          
        }
        
        extracted_keywords = rownames(nodes[1:cumsum_my_levels[best_level_inflexion],])
		
		scores = c()
        names_levels = names(my_levels)
      
        for (h in 1:best_level_index){
        
          scores = c(scores,rep(as.numeric(names_levels[h]),as.numeric(my_levels[h])))
        
        }
        
      } else {
        names_levels = names(my_levels)
        # use main core
        extracted_keywords = rownames(nodes[1:my_levels[1],])
	    	scores = rep(as.numeric(names_levels[1]),as.numeric(my_levels[1]))
		
      }
      
    }
    
    if (method=="CR"){
      
      # CoreRank
      
      rownames_nodes = rownames(nodes)
      sum_core_no = c()
      
      # iterate through each node
      for (node_temp in rownames_nodes){
        neighbors_temp = neighbors(g, node_temp, which_nodes)
        # edge_weights = E(g)[incident(g, node_temp, which_nodes)]$weight
        sum_core_no = c(sum_core_no, sum(V(g)[neighbors_temp]$core_no))
      }
      names(sum_core_no) = rownames_nodes
      sorted_sum_core_no = sort(sum_core_no, decreasing = TRUE)
      
      if (is.na(number_to_retain)){
      
        # since it is quite expensive, we return both elbow and percentage for CR
        best_cutoff_elbow = get_elbow_point(sorted_sum_core_no)$elbow_point
        best_cutoff_percentage = round(length(sorted_sum_core_no)*percentage)
        
      } else {
          
          best_cutoff = number_to_retain
          
        }
      
	  # returns both CRP and CRE as a list (to avoid having to recompute twice the scores)
      extracted_keywords = list(names(sorted_sum_core_no)[1:best_cutoff_percentage],names(sorted_sum_core_no)[1:best_cutoff_elbow])
      scores = list(as.numeric(sorted_sum_core_no[1:best_cutoff_percentage]), as.numeric(sorted_sum_core_no[1:best_cutoff_elbow]))
	  
    }
    
  }
  
  output = list(extracted_keywords=extracted_keywords, scores=scores)
  
}