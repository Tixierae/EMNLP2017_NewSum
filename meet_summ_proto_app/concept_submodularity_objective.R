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
    
    # if (negative_terms){
    # # note: the problem when using negative scores is that the function is not monotone anymore - you can have a bigger sentence that has a lower score than a smaller sentence
    
    # index_present = which(concepts %in% x)
    # index_absent = setdiff(1:length(concepts), index_present)
    
    # score_unit_neg = sum(my_scores[index_absent])
    
    # score_unit = score_unit_pos - score_unit_neg
    
    # } else {
    
    #  score_unit = score_unit_pos
    
    
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