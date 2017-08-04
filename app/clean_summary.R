clean_summary = function(x){
  
  # remove punctuation except apostrophes
  x = gsub("[^[:alnum:][:space:]']", " ", x)
  
  # remove not-intra-word apostrophes
  x = gsub(" '", " ", x)
  x = gsub("' ", " ", x)
  
  # collapse intra-word apostrophes
  x = gsub("'", "", x)
  
  # remove leading and trailing white space
  x = str_trim(x,"both")
  
  # remove extra white space
  x = gsub("\\s+"," ",x)
  
  # convert to lower case
  x = tolower(x)
  
  return(x)
  
}