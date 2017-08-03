best_level_density = function(core_or_truss_numbers, my_graph){
  
  density_per_level = list()
  l_fac = levels(as.factor(core_or_truss_numbers))
  
  i = 1
  for (level in l_fac){
    my_subgraph = induced_subgraph(my_graph, which(V(my_graph)$core_no>=as.numeric(level)))
    n_vert = vcount(my_subgraph)
    density_per_level[[i]] = round(ecount(my_subgraph)/(n_vert*(n_vert-1)),4)
    i = i + 1
  }
  
  density_per_level = unlist(density_per_level)
  names(density_per_level) = l_fac
  
  sorted_density_per_level = sort(density_per_level, decreasing=TRUE)
  
  # if there are only two levels, retain the one yielding the best density
  if (length(sorted_density_per_level)<3){
    optimal_level = names(sorted_density_per_level)[which(sorted_density_per_level==max(sorted_density_per_level))][1]
  } else {
    # if there are more than one elbow point, retain the first one
    optimal_level = names(sorted_density_per_level)[get_elbow_point(sorted_density_per_level)$elbow_point[1]]
  
  }
  
  output = list(optimal_level = optimal_level)
  
}