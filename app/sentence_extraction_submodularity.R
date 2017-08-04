sentence_extraction_submodularity = function(units_splitted, units_splitted_stemmed, keywords_temp, scores_temp, to_stem, budget, start_time, scaling_factor, weighted_sum_concepts, negative_terms, lambda){
  
  # costs are unit sizes
  # scores are needed for the computation of the objective
  
  scores_internal = scores_temp
  
  # prune out units not containing any concept to save iterations
  unit_presence_index = unlist(lapply(units_splitted_stemmed, function(x) any(keywords_temp %in% x))) 
  
  index_absent = which(unit_presence_index==FALSE)
  if (length(index_absent)>0){
    units_splitted = units_splitted[-index_absent]
    units_splitted_stemmed = units_splitted_stemmed[-index_absent]
    start_time = start_time[-index_absent]
  }
  
  # if at least one unit remains
  if (length(units_splitted)>0){
    
      start_time_save = start_time
      
	  # ensure that singletons that exceed budget get truncated (otherwise their scores are increased)!
	  
      singletons_scores = unlist(lapply(units_splitted_stemmed, function(x) concept_submodularity_objective(units=x[min(length(x),budget)], concepts=keywords_temp, my_scores=scores_internal, to_stem=to_stem, weighted_sum_concepts=weighted_sum_concepts, negative_terms=negative_terms, lambda)$score))
      
      # if multiple singletons are best, select the first one
      max_singletons_scores = max(singletons_scores)
      first_best = which(singletons_scores==max_singletons_scores)[1]
      v_star = units_splitted[[first_best]][min(length(units_splitted[[first_best]]),budget)] # make sure that v_star is not a list!
      v_star_start_time = start_time[first_best]
      
      # initialization
      G = list()
      G_unstemmed = list()
      concept_submodularity_objective_G = 0
      selected_units_start_times = c()
      U = units_splitted_stemmed
      
      while (length(U)>0){
        
        if (length(U)==length(units_splitted_stemmed)){
          # at first iteration, G is empty so:
          numerators_left = singletons_scores
          
        } else {
          
          numerators_left = unlist(lapply(U, function (x) {
            
            concept_submodularity_objective(units=unlist(c(G,x)), concepts=keywords_temp, my_scores=scores_internal, to_stem=to_stem, weighted_sum_concepts=weighted_sum_concepts, negative_terms=negative_terms, lambda)$score
            
          }))
          
        }
        
        # compute ratios of objective vs scaled cost
        ratios = unlist(lapply(1:length(U), function (x) {
          
          cost_x = length(U[[x]])
          num = numerators_left[x] - concept_submodularity_objective_G
          denom = cost_x^scaling_factor
          ratio = round(num/denom,4)
          
          return(ratio)
          
        }))
        
        # select unit associated with the max ratio
        
        # what if we have several best? -> select the first one that has lowest cost
        index_max = which(ratios==max(ratios))
        candidate_ks = U[index_max]
        candidate_ks_unstemmed = units_splitted[index_max]
        candidate_start_times = start_time[index_max]
        candidate_ks_costs = unlist(lapply(candidate_ks, length))
        
        min_candidate_ks_costs = min(candidate_ks_costs)
        first_best = which(candidate_ks_costs==min_candidate_ks_costs)[1]
        k = candidate_ks[first_best]
        k_unstemmed = candidate_ks_unstemmed[first_best]
        start_time_best = candidate_start_times[first_best]
        index_max = index_max[first_best]
        k_cost = min_candidate_ks_costs
        
        # check constraints
        summary_cost = sum(unlist(lapply(G, length)))
        
        budget_constraint = k_cost + summary_cost <= budget # here PB
        
        objective_constraint = (numerators_left[index_max] - concept_submodularity_objective_G)  >= 0
        
        if (objective_constraint){
          if(budget_constraint){
            G = c(G,k)
            G_unstemmed = c(G_unstemmed,k_unstemmed)
            selected_units_start_times = c(selected_units_start_times, start_time_best)
          } else {
            # truncate element that had been selected and iterate
            U[[index_max]] = U[[index_max]][1:(budget-summary_cost)]
            units_splitted[[index_max]] = units_splitted[[index_max]][1:(budget-summary_cost)]
            next
          }
        }
        
        # update     
        concept_submodularity_objective_G = concept_submodularity_objective(units=unlist(G), concepts=keywords_temp, my_scores=scores_internal, to_stem=to_stem, weighted_sum_concepts=weighted_sum_concepts, negative_terms=negative_terms, lambda)$score
        
        # regardless of the constraints remove k from U
        U = setdiff(U,k)
        units_splitted = setdiff(units_splitted, k_unstemmed)
        start_time = setdiff(start_time, start_time_best)
        
        if (sum(unlist(lapply(G, length)))>=budget){
          break
        }
        
      } # end while
      
      G = unlist(lapply(G_unstemmed, function(x) paste(x, collapse=" ")))
      
      # of G and v_star, select the one maximizing the objective
      if (concept_submodularity_objective_G >= max_singletons_scores) {
        
        G_final = G
        start_times = selected_units_start_times
        
      } else {
        
        G_final = paste(v_star, collapse=" ")
        start_times = v_star_start_time
        
      }
    
  # if no unit contains a concept
  } else {
    
    G_final = ""
    start_times = NA
    
  }
  
  output = list(surviving_sentences=G_final, start_times = start_times)
  
}