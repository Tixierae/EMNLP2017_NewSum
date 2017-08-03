from_keyword_to_summary_submodularity = function(graph_keywords_scores_temp, utterances, start_time, to_stem, max_summary_length, scaling_factor, weighted_sum_concepts, negative_terms, lambda, is_trad_doc){
  
  extracted_keywords = graph_keywords_scores_temp$extracted_keywords
  scores = graph_keywords_scores_temp$scores
  
  # eliminate repeated consecutive words (e.g., "and and")
  utterances = unlist(lapply(utterances, function(x){gsub("\\b(\\w+)\\s+\\1\\b","\\1",x)}))
  
  # eliminate repeated consecutive bigrams (e.g., "and you and you")
  utterances = unlist(lapply(utterances, function(x){gsub("(\\b.+?\\b)\\1\\b", "\\1", x, perl=T)}))
  
  # remove extra whitespace, leading and trailing whitespace
  utterances = unlist(lapply(utterances, function(x){
    x = str_trim(x,"both")
    x = gsub("\\s+"," ",x)
    return(x)
  }
  ))
  
  # if graph clustering has been performed
  if (class(extracted_keywords)=="list"){
    
    if (any(unlist(lapply(scores, max))!=0)==FALSE){
      # if all scores are at zero (sometimes happens with weighted k-core and degree taking into account edge direction)
      # assign the same score to each node of a given community based on the size of the community
      my_lengthes_temp = unlist(lapply(scores, length))
      scores = lapply(my_lengthes_temp, function(x){rep(1,x)*x})
    }
    
    # max_length should not be shared equally among all communities - some topics are more important than others
    
    normalized_max_scores = unlist(lapply(scores, max))
    index = order(normalized_max_scores, decreasing=TRUE)
    normalized_max_scores = normalized_max_scores/sum(normalized_max_scores)
    max_lengthes = round(max_summary_length*normalized_max_scores)
    extracted_keywords = extracted_keywords[index]
    scores = scores[index]
    max_lengthes = max_lengthes[index]
    
    my_summary = vector(length=length(extracted_keywords),mode="list")
    my_start_times = vector(length=length(extracted_keywords),mode="list")
    
    units = utterances
    units_splitted = lapply(units, function (x) unlist(strsplit(x, split=" ")))
    # since to_stem is equal to TRUE in our experiments, I set it here globally
    units_splitted_stemmed = lapply(units_splitted, wordStem)
    
    for (gg in 1:length(extracted_keywords)){
      
      keywords_temp = unlist(extracted_keywords[gg])
      scores_temp = unlist(scores[gg])
      names(scores_temp) = keywords_temp
      # normalize so that it sums up to 1 (might be needed if we want a lambda between 0 and 1)
      # round up to avoid carrying many decimals (to improve efficiency)
      scores_temp = round(scores_temp/sum(scores_temp),4)
      budget = max_lengthes[gg]
      # ? redundancy_threshold
      
      output = sentence_extraction_submodularity(units_splitted, units_splitted_stemmed, keywords_temp, scores_temp, to_stem, budget, start_time, scaling_factor, weighted_sum_concepts, negative_terms, lambda)
      
      my_summary[[gg]] = output$surviving_sentences
      my_start_times[[gg]] = output$start_times
      
      # remove the utterances already selected so that they cannot get selected again (it also improves efficiency)
      
      index_used = which(unlist(lapply(units, function(x) x %in% my_summary[[gg]])))
      
      if (length(index_used)>0){
        units = units[-index_used]
        units_splitted = units_splitted[-index_used]
        units_splitted_stemmed = units_splitted_stemmed[-index_used]
      }
      
    }
    
  } else {
    
    units = utterances
    if (!is_trad_doc){
      units_splitted = lapply(units, function (x) unlist(strsplit(x, split=" ")))
    } else {
      units_splitted = lapply(units, function (x){
        
        # remove everything within curly brackets ({vocalsound}, etc)
        x = gsub("{.*?}", '', x, perl = TRUE)
        
        # remove punctuation
        x = gsub("[^[:alnum:][:space:]]", " ", x)
        
        # remove single s (from the 's)
        x = gsub(' s ', ' ', x)
        
        # remove leading and trailing white space
        x = str_trim(x,"both")
        
        # remove extra white space
        x = gsub("\\s+"," ",x)
        
        # convert to lower case
        x = tolower(x)
        
        x = unlist(strsplit(x, split=" "))
        
      })
      
    }
    
    units_splitted_stemmed = lapply(units_splitted, wordStem)
    
    keywords_temp = extracted_keywords
    scores_temp = scores
    names(scores_temp) = keywords_temp
    scores_temp = round(scores_temp/sum(scores_temp),4)
    budget = max_summary_length
    
    output = sentence_extraction_submodularity(units_splitted, units_splitted_stemmed, keywords_temp, scores_temp, to_stem, budget, start_time, scaling_factor, weighted_sum_concepts, negative_terms, lambda)
    
    my_summary = output$surviving_sentences
    my_start_times = as.numeric(output$start_times) # in the case of traditional documents, also corresponds to the indexes of the utterances selected
    if (is_trad_doc){
      counter = 1
      my_summary_final = c()
      for (elt in my_summary){
        my_size = length(unlist(strsplit(elt, split= ' ')))
        my_summary_final[counter] = paste(unlist(strsplit(utterances[my_start_times[counter]], split=' '))[1:min(my_size,length(unlist(strsplit(utterances[my_start_times[counter]], split=' '))))], collapse=' ')
        counter = counter + 1 
      }
      
    my_summary = my_summary_final
    
    }
    
  }
  
  my_summary = unlist(my_summary)
  my_start_times = unlist(my_start_times)
  
  # if any, remove empty utterances
  index_to_remove = which(unlist(lapply(my_summary, nchar))==0)
  if (length(index_to_remove>0)){
    my_summary = my_summary[-index_to_remove]
    my_start_times = my_start_times[-index_to_remove]
  }
  
  index_to_remove = which(my_start_times==-999)
  if (length(index_to_remove>0)){
    my_summary = my_summary[-index_to_remove]
    my_start_times = my_start_times[-index_to_remove]
  }
  
  # sort summary in chronological order
  temporal_index = order(my_start_times, decreasing=FALSE)
  my_summary = my_summary[temporal_index]
  
  # output in right format for scoring with ROUGE
  my_summary = paste0(my_summary, ".")
  
  output = list(my_summary = my_summary)
  
}