cleaning_meeting_text <-
function(X, custom, pos, stem, detected_language, remove_single, is_trad_doc) {
    
	# if English, uses Porter's stemmer
    if (detected_language == 'french') {
		language_stemmer = 'french'
	} else if (detected_language == 'english') {
		language_stemmer = 'porter'
	}
  
    if (!is_trad_doc){
	  
		# remove between word dashes
		x = gsub("- ", " ", X, perl = TRUE) 
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
	
	} else {
	
		x = X
	
	}

	# remove leading and trailing white space
	x = str_trim(x,"both")

	# remove extra white space
	x = gsub("\\s+"," ",x)

	# make a copy of tokens without further preprocessing
	xx = unlist(strsplit(x, split=" "))
	
	# convert to lower case
	x = tolower(x)
	
	if (is_trad_doc){
		# remove punctuation except dashes
		x = gsub("[^[:alnum:][:space:]-]", " ", x)
		
		# remove dashes that are not intra-word
		x = gsub("- ", " ", x, perl = TRUE)
	    x = gsub(" -", " ", x, perl = TRUE)
	}
	
    # remove stopwords
    stopwords_bis = paste0("\\<",custom,"\\>")
    
    x = unlist(lapply(x, function(elt){
      for (i in 1:length(stopwords_bis)){
        elt = gsub(stopwords_bis[i]," ",elt)
      }
      return(elt)
    }))
    
    # remove extra whitespace, leading and trailing whitespace
    x = unlist(lapply(x, function(elt){
      elt = str_trim(elt,"both")
      elt = gsub("\\s+"," ",elt)
      return(elt)
    }
    ))
    
    x = unlist(strsplit(x, split=" "))

    if (length(x)>0){
      
      if(any(nchar(x)>0)){
    
	if (pos == TRUE) {
		# retain nouns and adjectives
		x_tagged = tagPOS(x)$output
		index = which(x_tagged%in%c("NN","NNS","NNP","NNPS","JJ","JJS","JJR"))
		if (length(index)>0){
			x = x[index]
		}
	}

	if (stem == TRUE){
		x = wordStem(x, language = language_stemmer)
		xx = wordStem(xx, language = language_stemmer)
	}

      }
    }
    
	if (detected_language == 'english'){
	
	# remove blanks and one letter elements
		index = c(which(x==""),which(nchar(x)<2))
		if (length(index)>0){
		  x = x[-index]
		}
	
	} else if (detected_language == 'french'){
	
	if (remove_single == TRUE){
	    # remove blanks and one letter elements
		index = c(which(x==""),which(nchar(x)<2))
		if (length(index)>0){
		  x = x[-index]
		}

	} else if (remove_single == FALSE){
		# remove only blanks
		index = which(x=="")
		if (length(index)>0){
		  x = x[-index]
		}
	}
	
	}
    
	output = list(unprocessed=xx, processed=x)

}