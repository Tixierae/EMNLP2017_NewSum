clean_utterances = function(utterances, my_stopwords, filler_words, is_trad_doc){
  
	utterances = unlist(lapply(utterances, function(x){
   
    if (!is_trad_doc){
   
	# remove between word dashes
	x = gsub("- ", " ", x, perl = TRUE) 
	x = gsub(" -", " ", x, perl = TRUE)
		
	# remove everything within curly brackets ({vocalsound}, etc)
	x = gsub("{.*?}", '', x, perl = TRUE)
	
	# remove space before apostrophes
	x = gsub("\\b\\s+'\\b", "'", x, perl=TRUE)
    
	# remove punctuation except apostrophes (the only punctuation mark featured in the ASR output)
	x = gsub("[^[:alnum:][:space:]']", " ", x)
	
	# remove apostrophes that are not intra-word
	x = gsub("' ", " ", x, perl = TRUE)
	x = gsub(" '", " ", x, perl = TRUE)
	
	# convert to lower case
	x = tolower(x)
	
	}

	# remove leading and trailing white space
	x = str_trim(x,"both")

	# remove extra white space
	x = gsub("\\s+"," ",x)
    
	}))
   
  if (!is_trad_doc){
  # eliminate filler words (some stopwords would also be useful to eliminate, unfortunately many of them are also crucial for understanding)
  filler_words_bis = paste0("\\<",filler_words,"\\>")
  
  utterances = unlist(lapply(utterances, function(x){
    for (i in 1:length(filler_words)){
      x = gsub(filler_words_bis[i]," ",x)
    }
    return(x)
  }))
	
  # eliminate repeated (consecutive words, e.g., "and and")
  utterances = unlist(lapply(utterances, function(x){gsub("\\b(\\w+)\\s+\\1\\b","\\1",x)}))
  
  # eliminate repeated consecutive bigrams
  utterances = unlist(lapply(utterances, function(x){gsub("(\\b.+?\\b)\\1\\b", "\\1", x, perl=T)}))
  
  # remove extra whitespace, leading and trailing whitespace
  utterances = unlist(lapply(utterances, function(x){
    x = str_trim(x,"both")
    x = gsub("\\s+"," ",x)
    return(x)
  }
  ))
  
  # # eliminate words at the beginning if they are stopwords (search is conducted up to three stopwords)
  # utterances = unlist(lapply(utterances, function(x){
    # x_splitted = unlist(strsplit(x, split=" "))
    # if(x_splitted[1]%in%my_stopwords){
      # x_splitted = x_splitted[-1]
      # if(x_splitted[1]%in%my_stopwords){
        # x_splitted = x_splitted[-1]
        # if(x_splitted[1]%in%my_stopwords){
          # x_splitted = x_splitted[-1]
        # }
      # }
    # }
    # return(paste(x_splitted,collapse=" "))
  # }))
  
  # eliminate words at the end if they are stopwords (search is conducted up to three stopwords)
  utterances = unlist(lapply(utterances, function(x){
    x_splitted = unlist(strsplit(x, split=" "))
    if(x_splitted[length(x_splitted)]%in%my_stopwords){
      x_splitted = head(x_splitted,-1)
      if(x_splitted[length(x_splitted)]%in%my_stopwords){
        x_splitted = head(x_splitted,-1)
        if(x_splitted[length(x_splitted)]%in%my_stopwords){
          x_splitted = head(x_splitted,-1)
        }
      }
    }
    return(paste(x_splitted,collapse=" "))
  }))
  
  }
  
  output = list(utterances = utterances)

 }