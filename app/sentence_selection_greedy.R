sentence_selection_greedy = function(sentences, scores_utterances, max_length, redundancy_threshold, custom_stopwords, start_time){
 
  # go down and select one sentence at a time until a certain length (number of words) has been reached
  # if multiple sentences contain the same number of corewords, select the longest one not redundant with respect to the current summary
 
  redundancy_threshold_internal = redundancy_threshold
 
  surviving_sentences = c()
  sentences_start_times = c()
  total_length = 0
 
    for (i in 1:length(sentences)){
     
      sentence = sentences[i]
      sentence_start_time =  start_time[i]
      
      sentence_tokenized = unlist(strsplit(sentence, split=" "))
      l_sentence = length(sentence_tokenized)
      
      # compute redundancy with respect to current summary in terms of stemmed non-stopword overlap
      list_words_current_summary = unlist(lapply(surviving_sentences, function(x) {strsplit(tolower(x), split=" ")}))
      list_words_current_summary_cleaned = setdiff(list_words_current_summary,custom_stopwords)
      list_words_current_summary_cleaned = unique(wordStem(list_words_current_summary_cleaned))
     
      list_words_sentence = unlist(strsplit(tolower(sentence), split=" "))
      list_words_sentence_cleaned = setdiff(list_words_sentence, custom_stopwords)
      list_words_sentence_cleaned = unique(wordStem(list_words_sentence_cleaned))
     
      redundancy = length(intersect(list_words_current_summary_cleaned,list_words_sentence_cleaned))/length(list_words_sentence_cleaned) 
     
      # if the sentence is too much redundant with current summary, try next one
      # otherwise add it to current summary, and update summary length
      if (redundancy >= redundancy_threshold_internal){
       
        next
       
      } else {
       
        surviving_sentences = c(surviving_sentences, sentence)
        sentences_start_times = c(sentences_start_times,  sentence_start_time)
        total_length = total_length + l_sentence
       
      }
  
    if(total_length>=max_length){
      break
    }
      
  }
    
  output = list(surviving_sentences = surviving_sentences, start_times =  sentences_start_times)
 
}