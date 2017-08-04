keyword_extraction = function(g, community_algo, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes){
    
    # use_elbow and use_percentage are only used for TR (for CR, both CRE and CRP are returned)
    # therefore, for CR, one should always have specified a "percentage" value
    
    # "method" can be any of "TR", "main", "inf", "CRP", "CRE", "dens" 
    
    # if use_percentage is TRUE: percentage is used (real between 0 and 1)
    # otherwise, number_to_retain (integer) - currently not using
    
    # which_nodes is used in coreRank only (for selecting the neighbors), and can be "all", "in", or "out"
    
    if ((method=="CRP")|(method=="CRE")){
      
      method_inner = "CR"
      
    } else {
      
      method_inner = method
      
    }
    
    if (community_algo=="none"){
      
      output = keyword_extraction_inner(g, method=method_inner, use_elbow, use_percentage, percentage, number_to_retain, which_nodes)
      
      if (method_inner=="CR"){
        
        if (method=="CRE"){
          
          extracted_keywords = output$extracted_keywords[[2]]
          scores = output$scores[[2]]
          
        } else if (method=="CRP"){
          
          extracted_keywords = output$extracted_keywords[[1]]
          scores = output$scores[[1]]
          
        }
        
      } else {
        
        extracted_keywords = output$extracted_keywords
        scores = output$scores
        
      }
      
    } else {
      
      g_data_frame = get.data.frame(g, what="both")
      nodes = g_data_frame$vertices[,c("comm","core_no")]
      # sort data frame by decreasing core number values
      nodes = nodes[order(nodes[,"core_no"],decreasing=TRUE),]
      
      my_index = which(is.na(nodes[,"comm"]))
      
      if (length(my_index)>0){
      
        nodes_for_rec = nodes[c("comm","core_no")][-my_index,]
      
      } else {
        
        nodes_for_rec = nodes
        
      }
      
      big_comms = names(summary(as.factor(nodes_for_rec[,"comm"])))
      l_b_g = length(big_comms)
      
      extracted_keywords = vector(length=l_b_g, mode="list")
      scores = vector(length=l_b_g, mode="list")
      
      for (i in 1:l_b_g){
        
        nodes_temp = nodes_for_rec[which(nodes_for_rec[,"comm"]==big_comms[i]),]
        # subgraph induced by the community
        g_subgraph = induced_subgraph(g, as.numeric(V(g)[row.names(nodes_temp)]))
        
        # important to specify "which_nodes" when CR is used
        output = keyword_extraction_inner(g_subgraph, method=method_inner, use_elbow, use_percentage, percentage, number_to_retain, which_nodes)
        
        if (method_inner=="CR"){
          
          if (method=="CRE"){
            
            extracted_keywords[[i]] = output$extracted_keywords[[2]]
            scores[[i]] = output$scores[[2]]
            
          } else if (method=="CRP"){
            
            extracted_keywords[[i]] = output$extracted_keywords[[1]]
            scores[[i]] = output$scores[[1]]
            
          }
          
        } else {
          
          extracted_keywords[[i]] = output$extracted_keywords
          scores[[i]] = output$scores
          
        }
        
      } # end i loop
      
    }
    
    output = list(extracted_keywords = extracted_keywords, scores = scores)
    
  }