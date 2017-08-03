from_cleaned_transcript_to_terms_list <-
function(cleaned_transcript_text){
  
  temp = as.character(cleaned_transcript_text)
  temp = paste(temp, collapse = " . ")
  temp = unlist(strsplit(temp, split = " "))
  output = list(terms_list_partial = temp)
  
}
