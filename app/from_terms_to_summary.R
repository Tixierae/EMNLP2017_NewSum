from_terms_to_summary = function(terms_list, utterances, start_time, window_size, to_overspan, to_build_on_processed, community_algo, weighted_comm, directed_comm, rw_length, size_threshold, degeneracy, directed_mode, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes, redundancy_threshold, to_stem, filler_words, my_stopwords, max_summary_length){
    
  graph_keywords_scores = from_terms_to_keywords(terms_list, window_size, to_overspan, to_build_on_processed, community_algo, weighted_comm, directed_comm, rw_length, size_threshold, degeneracy, directed_mode, method, use_elbow, use_percentage, percentage, number_to_retain, which_nodes)
  
  g = graph_keywords_scores$g
  
  keywords_scores = graph_keywords_scores$output
  
  extracted_keywords = keywords_scores$extracted_keywords
  scores = keywords_scores$scores
  
  if (any(unlist(lapply(scores, max))!=0)==FALSE){
    # if all scores are at zero (sometimes happens with weighted k-core and degree taking into account edge direction)
    # assign the same score to each node of a given community based on the size of the community
    
    my_lengthes_temp = unlist(lapply(scores, length))
    
    scores = lapply(my_lengthes_temp, function(x){rep(1,x)*x})
    
  }
  
  if (class(extracted_keywords)=="list"){
    # if communities have been detected
    # max_length should not be shared equally among all communities - some topics are more important than others
   
    normalized_max_scores = unlist(lapply(scores, max))
    normalized_max_scores = normalized_max_scores/sum(normalized_max_scores)
    
    max_lengthes = round(max_summary_length*normalized_max_scores)
    
    index = order(unlist(lapply(scores, max)), decreasing=TRUE)
    
    extracted_keywords = extracted_keywords[index]
    scores = scores[index]
    max_lengthes = max_lengthes[index]
    
    my_summary = vector(length=length(extracted_keywords),mode="list")
    my_start_times = vector(length=length(extracted_keywords),mode="list")
    
    for (gg in 1:length(extracted_keywords)){
      
      output = sentence_extraction(utterances = utterances, extracted_keywords_temp = unlist(extracted_keywords[gg]),scores_temp = unlist(scores[gg]),to_stem = to_stem,custom_stopwords = my_stopwords,redundancy_threshold = redundancy_threshold, max_length = max_lengthes[gg], start_time = start_time) 
      
      my_summary[[gg]] = output$surviving_sentences
      my_start_times[[gg]] = output$start_times
      
    }
    
    my_summary = unlist(my_summary)
    my_start_times = unlist(my_start_times) 
    
  } else {
    
    output = sentence_extraction(utterances=utterances,extracted_keywords_temp=extracted_keywords, scores_temp=scores, to_stem=to_stem, custom_stopwords=my_stopwords, redundancy_threshold=redundancy_threshold, max_length=max_summary_length, start_time=start_time)
    
    my_summary = output$surviving_sentences
    my_start_times = output$start_times
    
    my_logical = !unlist(lapply(my_summary, is.null))
    my_summary = my_summary[my_logical]
    my_start_times = my_start_times[my_logical]
    
  }
  
  # remove any duplicated utterance
  my_logical = !duplicated(my_summary)
  my_summary = my_summary[my_logical]
  my_start_times = my_start_times[my_logical] 
  
  # remove any NA utterance
  my_logical = !unlist(lapply(my_summary, is.na))
  my_summary = my_summary[my_logical]
  my_start_times = my_start_times[my_logical]
  
  # make sure that summary size meets the constraint
  summary_length = sum(unlist(lapply(my_summary,function(x){length(unlist(strsplit(x, split=" ")))})))
  to_remove = summary_length - max_summary_length 
  
  if(to_remove>0){
    
    my_summary_reverse = rev(my_summary)
    my_start_times_reverse = rev(my_start_times)
    
    while(to_remove>=0){
      
      first_sent_rev_tokenized = unlist(strsplit(my_summary_reverse[1], split=" "))
      l_first_sent_rev = length(first_sent_rev_tokenized)
      
      # if removing the full sentence is too much (by at least three words), trim it so that we remove just what is necessary
      if (l_first_sent_rev > (to_remove + 3) ){
        
        first_sent_rev = paste0(first_sent_rev_tokenized[1:(l_first_sent_rev-to_remove)], collapse=" ")
        my_summary_reverse = my_summary_reverse[-1]
        my_summary_reverse = c(first_sent_rev, my_summary_reverse)
        
      } else {
        
        my_summary_reverse = my_summary_reverse[-1]
        my_start_times_reverse = my_start_times_reverse[-1]
        
      }
      
      to_remove = to_remove - l_first_sent_rev
      
    }
    
    my_summary = rev(my_summary_reverse)
    my_start_times = rev(my_start_times_reverse)
    
  }
  
  # sort summary in chronological order
  temporal_index = order(my_start_times, decreasing=FALSE)
  my_summary = my_summary[temporal_index]
  
  # output in right format for scoring with ROUGE
  my_summary = paste0(my_summary, ".")
  
  output = list(my_summary = my_summary, g = g)
  
}