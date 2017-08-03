sentence_extraction = function(utterances, extracted_keywords_temp, scores_temp, to_stem, custom_stopwords, redundancy_threshold, max_length, start_time){
  
  # start_time_internal = start_time
  
  # scores_internal = scores_temp
  # names(scores_internal) = extracted_keywords_temp
  # # the names of the scores, i.e. the keywords
  # names_scores_internal = extracted_keywords_temp
  # l_n_s = length(names_scores_internal)
  
  # # assign scores to utterances based on scores of the words they contain
  # # at most, a word can contribute twice to the total score of the utterance
  # # this (tries to) ensure that the top sentences contain many DIFFERENT corewords and not the same coreword repeated over and over again
  
    # utterances_scores = c()
    
    # for (utterance in utterances){
      # utterance_score = 0
      # if (to_stem == FALSE){
        # utterance_words = unlist(strsplit(tolower(utterance), split=" "))
      # } else {
        # utterance_words = wordStem(unlist(strsplit(tolower(utterance), split=" ")))
      # }
     
      # for (word in names_scores_internal){
        
        # if (word %in% utterance_words){
            # index = which(word==names_scores_internal)
            # utterance_score = utterance_score + min(length(which(word==utterance_words)),2)*as.numeric(scores_internal[index])
        # }
      # }
      
      # utterances_scores = c(utterances_scores, utterance_score)
      
    # }
    
    # # rank utterances in decreasing order
    # my_index = order(utterances_scores, decreasing=TRUE)
    # utterances = utterances[my_index]
    # utterances_scores = utterances_scores[my_index]
    # start_time_internal = start_time_internal[my_index]
    
    # # greedily select from top until length constraint is satisfied
    # output_internal = sentence_selection_greedy(sentences=utterances_internal, scores_utterances=utterances_scores, max_length=max_length, redundancy_threshold = redundancy_threshold, custom_stopwords = custom_stopwords, start_time=start_time_internal)
    
    # surviving_sentences = output_internal$surviving_sentences
    # start_times = output_internal$start_times
    
    # output = list(surviving_sentences=surviving_sentences, start_times = start_times)
	
  utterances_internal = utterances
  start_time_internal = start_time
  
  # normalize scores
  scores_internal = round(scores_temp/sum(scores_temp),4)
  names(scores_internal) = extracted_keywords_temp
  # the names of the scores, i.e. the keywords
  names_scores_internal = extracted_keywords_temp
  l_n_s = length(names_scores_internal)
 
  penalize_zero_entries = (1/(l_n_s*5))
 
  # count how many corewords are contained in each utterance
  if (to_stem == FALSE){
    my_counts = unlist(lapply(gsub("[^[:alnum:][:space:]]", "", tolower(utterances_internal)), function(y) {sum(unlist(lapply(names_scores_internal, function (x) {length(which(x==unlist(strsplit(y, split=" "))))})))}))
  } else {
    my_counts = unlist(lapply(gsub("[^[:alnum:][:space:]]", "", tolower(utterances_internal)), function(y) {sum(unlist(lapply(names_scores_internal, function (x) {length(which(x==wordStem(unlist(strsplit(y, split=" ")))))})))}))
  }
 
  # prune out utterances not containing any coreword
  index_remove = which(my_counts==0)
 
  if (length(index_remove)>0){
   
    utterances_internal = utterances_internal[-index_remove]
    start_time_internal = start_time_internal[-index_remove]
   
  } else {
   
    utterances_internal = utterances_internal
    start_time_internal = start_time_internal
   
  }
  
  if (length(utterances_internal)>0){
 
    # assign scores to utterances based on core number of the corewords they contain
    # penalize sentences with zero entries
    # this (tries to) ensure that the top sentences contain many DIFFERENT corewords and not the same coreword repeated over and over again
   
    if (to_stem == FALSE){
     
      scores_utterances = unlist(lapply(utterances_internal, function(x) {
        my_score_temp = lapply(names_scores_internal, function(y){length(which(y==unlist(strsplit(tolower(x),split=" "))))*scores_internal[y]})
        sum(unlist(my_score_temp))-(penalize_zero_entries)*length(which(my_score_temp==0))
      }))
     
    } else {
     
      scores_utterances = unlist(lapply(utterances_internal, function(x) {
        my_score_temp = lapply(names_scores_internal, function(y){length(which(y==wordStem(unlist(strsplit(tolower(x),split=" ")))))*scores_internal[y]})
        sum(unlist(my_score_temp))-(penalize_zero_entries)*length(which(my_score_temp==0))
      }))
     
    }
   
    # rank remaining sentences in decreasing order
    my_index = order(scores_utterances, decreasing=TRUE)
    utterances_internal = utterances_internal[my_index]
    scores_utterances = scores_utterances[my_index]
    start_time_internal = start_time_internal[my_index]

    # greedily select from top until length constraint is satisfied
    output_internal = sentence_selection_greedy(sentences=utterances_internal, scores_utterances=scores_utterances, max_length=max_length, redundancy_threshold = redundancy_threshold, custom_stopwords = custom_stopwords, start_time=start_time_internal)
    surviving_sentences = output_internal$surviving_sentences
    start_times = output_internal$start_times
 
  } else {
    
    surviving_sentences = NA
    start_times = NA
    
  }
  
  output = list(surviving_sentences=surviving_sentences, start_times = start_times)
  
}