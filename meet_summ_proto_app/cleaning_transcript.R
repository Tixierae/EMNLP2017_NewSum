cleaning_transcript <-
function(my_transcript_df, time_prune, custom = custom, pos, to_stem, overlap_threshold, detected_language, is_trad_doc){
  
  # - time_prune: duration in seconds below which utterances are considered uninformative and are removed
  # - custom: character vector of stopwords to remove
  # - pos: Boolean (whether to keep only nouns and adjectives)
  # - to_stem: Boolean (whether to apply Porter's stemmmer)
  # - overlap_threshold: decimal between 0 and 1 (percentage overlap threhsold)
  
  # colum names should be "start", "end", "role", and "text"
  
  # remove all utterances less than "time_prune" seconds in length since they are not informative
  utterance_lengthes = my_transcript_df[,"end"] - my_transcript_df[,"start"]
  index_short = which(utterance_lengthes < time_prune) 
  
  if (length(index_short)>0){
    my_transcript_df = my_transcript_df[-index_short,]
  }
  
  # clean text column - keep an unprocessed version for distance computation when building the gows
  text_my_transcript_df = as.character(my_transcript_df[,"text"])
  
  text_my_transcript_df_cleaned_processed = lapply(text_my_transcript_df, function(x){unlist(cleaning_meeting_text(X=x, custom=custom,pos=pos,stem=to_stem, detected_language=detected_language, remove_single=TRUE, is_trad_doc=is_trad_doc)$processed)})
  
  text_my_transcript_df_cleaned_unprocessed = lapply(text_my_transcript_df, function(x){unlist(cleaning_meeting_text(X=x, custom="",pos=FALSE,stem=FALSE, detected_language=detected_language, remove_single=FALSE, is_trad_doc=is_trad_doc)$unprocessed)})
  
  # find indexes of empty elements in processed vector
  index_empty = lapply(1:length(text_my_transcript_df_cleaned_processed),function(x){if(length(text_my_transcript_df_cleaned_processed[[x]])==0){return(x)}})
  index_empty = unlist(index_empty)
  
  # collapse consecutive utterances (from the same speaker - optional) if they (carry the same information) continue what was said before
  collapse_output = utterance_collapse(my_df = my_transcript_df, utterance_vector_processed = text_my_transcript_df_cleaned_processed, utterance_vector_unprocessed = text_my_transcript_df_cleaned_unprocessed, overlap_threshold = overlap_threshold)
  
  output = list(collapse_output = collapse_output, index_empty = index_empty)
  
}
