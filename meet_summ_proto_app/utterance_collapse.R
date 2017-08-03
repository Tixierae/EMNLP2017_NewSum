utterance_collapse <-
  function(my_df, utterance_vector_processed, utterance_vector_unprocessed, overlap_threshold) {
    
    my_df_new_proc = list()
    my_df_new_unproc = list()
    
    i = 1
    k = 1
    
    while(i <= nrow(my_df)){
      success = FALSE
      if (length(utterance_vector_unprocessed[[i]])>0){
        
        utterance_proc = utterance_vector_processed[[i]]
        utterance_unproc = utterance_vector_unprocessed[[i]]
        role = as.character(my_df[i,"role"])
        my_start = my_df[i,"start"]
        my_end = my_df[i,"end"]
        
        if ((i+1)<=nrow(my_df)){
          
          for (j in (i+1):nrow(my_df)){
            
            #if (as.character(my_df[j,"role"]) == role) {
            
            if (length(utterance_vector_unprocessed[[j]])>0){
              
              utterance_candidate_proc = utterance_vector_processed[[j]]
              utterance_candidate_unproc = utterance_vector_unprocessed[[j]]
              
              # # we compute overlap in terms of processed text (without stopwords and stemmed)
              # overlap = intersect(wordStem(utterance_proc),wordStem(utterance_candidate_proc))
              
              # attempt: overlap in terms of last four and first four unprocessed words
              
              if (success==TRUE){
                
                utterance_proc = unlist(strsplit(utterance_proc,split=" "))
                utterance_unproc = unlist(strsplit(utterance_unproc,split=" "))
                
              }
              
              if (length(utterance_unproc)>=4){
                text_before = utterance_unproc[(length(utterance_unproc)-3):length(utterance_unproc)]
              } else {
                text_before = utterance_unproc
              }
              
              if (length(utterance_candidate_unproc)>=4){
                text_after = utterance_candidate_unproc[1:4]
              } else {
                text_after = utterance_candidate_unproc
              }
              
              overlap = intersect(text_before, text_after)
              
              #if (length(overlap)/max(length(utterance_candidate_proc), length(utterance_proc)) >= overlap_threshold){
              
              if (length(overlap)/min(4, length(utterance_unproc),length(utterance_candidate_unproc))>=overlap_threshold){
                
                # if success
                
                if (length(utterance_proc)==0){
                  
                  utterance_proc = " "
                  
                }
                
                if (length(utterance_candidate_proc)==0){
                  
                  utterance_candidate_proc = " "
                  
                }
                
                utterance_proc = str_trim(string_join(paste(utterance_proc,collapse=" "), paste(utterance_candidate_proc,collapse=" "))$output)
                
                utterance_unproc = string_join(paste(utterance_unproc, collapse=" "), paste(utterance_candidate_unproc, collapse=" "))$output
                
                i = i + 1
                
                my_end = my_df[j,"end"]
                
                success = TRUE
                
              } else if ((as.character(my_df[j,"role"]) == role)&(length(utterance_proc)>0)&(length(utterance_candidate_proc)>0)) {
			  # if same speaker, and last word == first word (excluding stopwords), success
			  
				if (utterance_proc[length(utterance_proc)] == utterance_candidate_proc[1]){
				
					utterance_proc = str_trim(string_join(paste(utterance_proc,collapse=" "), paste(utterance_candidate_proc,collapse=" "))$output)
                
					utterance_unproc = string_join(paste(utterance_unproc, collapse=" "), paste(utterance_candidate_unproc, collapse=" "))$output
					
					 i = i + 1
                
					my_end = my_df[j,"end"]
                
					success = TRUE
				
				} else {
					# as soon as we don't get a success we exit the j loop
					break
				
				}
			  
			  } else {
                
                # as soon as we don't get a success we exit the j loop
                break
                
              }
              
            } else {
              
              break
              
            }
            
            
            # } else {
            
            # break
            
            # }
            
          }
          
        }
        
        my_df_new_proc[[k]] = c(my_start, my_end, role, paste(utterance_proc,collapse=" "))
        
        my_df_new_unproc[[k]] = c(my_start, my_end, role, paste(utterance_unproc,collapse=" "))
        
        i = i + 1
        k = k+1 
        
      } else {
        
        i = i + 1
        
      }
      
    } 
    
    # convert lists into data frames
    reduced_my_df_proc = data.frame(matrix(unlist(my_df_new_proc), nrow=length(my_df_new_proc), byrow=T),stringsAsFactors = FALSE)
    reduced_my_df_unproc = data.frame(matrix(unlist(my_df_new_unproc), nrow=length(my_df_new_unproc), byrow=T), stringsAsFactors = FALSE)
    
    output = list(reduced_my_df_proc = reduced_my_df_proc, reduced_my_df_unproc = reduced_my_df_unproc)
    
  }
