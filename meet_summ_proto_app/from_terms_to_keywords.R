from_terms_to_keywords <-
  function(terms_list, window_size, to_overspan, to_build_on_processed, community_algo, weighted_comm, directed_comm, rw_length, size_threshold, degeneracy, directed_mode, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes, overall_wd){
    
    edges_df = from_terms_to_graph(terms_list = terms_list, w = window_size, overspan = to_overspan, processed = to_build_on_processed)$output
    
    # ensures that the encoding is UTF-8 so that it works for French
    write.table(edges_df,paste0(overall_wd,"/edgelist.txt"), col.names = FALSE, row.names = FALSE, quote=FALSE, fileEncoding = 'utf-8')
    
    output = assign_attributes_to_graph_initial(edges_df, weighted = TRUE, directed = TRUE)
    
    g = output$g
    v_g_name = output$v_g_name
    l_v_g_name = output$l_v_g_name
    edges_df_und = output$edges_df_und
    
    # assign attributes to graph depending on parameter values
    
    if (community_algo!="none"){
      
      # perform community detection
      output = assign_attributes_to_graph_comm_a(g=g, v_g_name=v_g_name, l_v_g_name=l_v_g_name, input_community=community_algo, weight_comm=weighted_comm, directed_comm = directed_comm, rw_length = rw_length, edges_df_und = edges_df_und)
      
      g = output$g
      membership = output$membership
      my_sizes = output$sizes
      
      # perform graph decomposition
      g = assign_attributes_to_graph_comm_b(g, membership, my_sizes, size_threshold, method, degeneracy, directed_mode, overall_wd)$g
      
    } else {
      
      # perform graph decomposition
      g = assign_attributes_to_graph_nocomm(g, method, degeneracy, directed_mode, v_g_name, l_v_g_name)$g
      
    }
    
    # extract keywords from graph
    output = keyword_extraction(g, community_algo, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes)
    
    output = list(output=output, g=g)
    
  }