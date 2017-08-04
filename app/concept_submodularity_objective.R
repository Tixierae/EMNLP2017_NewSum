concept_submodularity_objective = function(units, concepts, my_scores, to_stem, weighted_sum_concepts, negative_terms, lambda){
  
  # units elements need to be split by whitespace
  
  ################
  ### COVERAGE ###
  ################
  
  if (length(units)>0){
    
    if (weighted_sum_concepts){
      sum_my_scores_coverage = sum(my_scores[units[units %in% concepts]])
    } else {
      sum_my_scores_coverage = sum(my_scores[concepts %in% units]) 
    }
     
    ########################
    ### DIVERSITY REWARD ###
    ########################
    
    # percentage of unique concepts covered
    diversity_score = length(which(concepts %in% unlist(units)))/length(concepts)
    
    my_score_final = sum_my_scores_coverage + lambda * diversity_score
    
  } else {
    
    my_score_final = 0
  }
  
  output = list(score = my_score_final)
  
}