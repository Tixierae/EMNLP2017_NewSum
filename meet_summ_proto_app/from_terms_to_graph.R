from_terms_to_graph <- 
function(terms_list,w,overspan,processed){
	# this function returns a data frame that can be passed to igraph::graph.data.frame to build a graph

	# terms: tokenized processed and unprocessed text (with punctuation in both cases), as list of two character vectors
	# w: size of the sliding window
	# overspan (boolean): whether the window should overspan sentences

	terms_list_processed = terms_list$processed

	if (processed==TRUE){
		terms = terms_list_processed
	} else {
		terms = terms_list$unprocessed
	}

	edges_info = hash()

	# store the offset position of words only (punctuation excluded)
	index_words = unlist(lapply(1:length(terms),function(index_temp){if(!grepl(terms[index_temp],";,.!!!?:")){return(index_temp)}}))

	# the window is applied to the preprocessed or unprocessed text (based on user's selection), but in any case punctuation is excluded (in the sense that punctuation marks cannot be added as vertices, and do not participate to the computation of distance)
	# we only have to take punctuation into account if overspan==FALSE
	# hence the indexing thing

	# create initial graph (first w terms)
	index_temp = index_words[1:(1+w-1)]
	combis = index_temp[combn(w,2)]
	candidate_edges = terms[combis]

	k = 1
	for (i in seq(1,length(candidate_edges),by=2)){
		# if overspan==FALSE, and if the two candidate nodes are separated by a punctuation mark indicating sentence end, disregard the edge
		if (overspan==FALSE){
			diff = combis[i+1]-combis[i]
			if (diff>1){
				if (any(c(";",".","!","?","...")%in%terms[combis[i]:combis[i+1]])){
					next
				}
			}
		}

		try_edge = c(candidate_edges[i],candidate_edges[i+1])
		
		if (try_edge[2]==try_edge[1]){
		  # if self-edge, skip
		  next
		}
		
		# if processed==FALSE, and if one of the two candidate nodes does not appear in the processed text (i.e., if it is not a noun or adjective, or if it is a stopword), disregard the edge
		if (processed==FALSE){
			if ((try_edge[1]%in%terms_list_processed==FALSE)|(try_edge[2]%in%terms_list_processed==FALSE)){
			next
		}
		}
		
		# if current edge list exists
		if (k>1){
		
	      # look up edge
		  value = edges_info[[paste(try_edge,collapse=" ")]]
		
		   if (!is.null(value)){
		    # if edge has already been seen, update its weight
		    edges_info[[paste(try_edge,collapse=" ")]] = edges_info[[paste(try_edge,collapse=" ")]] + 1
		    
		  } else {
		    # if edge has never been seen, create it and assign it a unit weight
		    edges_info[[paste(try_edge,collapse=" ")]] = 1
		    k = k+1
		  }

		} else {
		  
		# create edge and assign it a unit weight
		edges_info[[paste(try_edge, collapse=" ")]] = 1
		k = k+1 
		
		}
	}

	# then iterate over the remaining terms
	for (i in index_words[(w+1):length(index_words)]){

		# term to consider
		considered_term = terms[i]

		pos_end = which(index_words==i)

		index_temp = index_words[(pos_end-w+1):pos_end]
		combis = index_temp[combn(w,2)]

		# split combis into pairs
		combi_pairs = split(combis, ceiling(seq_along(combis)/2))

		# exclude pairs that do not include i (the index of considered_term) and collapse back into vector
		combis = as.numeric(unlist(combi_pairs[unlist(lapply(combi_pairs, function(x) i%in%x))]))

		candidate_edges = terms[combis]

		for (j in seq(1,length(candidate_edges),by=2)){
			# if overspan==FALSE, and if the two candidate nodes are separated by a punctuation mark indicating sentence end, disregard the edge
			if (overspan==FALSE){
				diff = combis[j+1]-combis[j]
				if (diff>1){
					if (any(c(";",".","!","?","...")%in%terms[combis[j]:combis[j+1]])){
						next
					}
				}
			}

			try_edge = c(candidate_edges[j],candidate_edges[j+1])

			if (try_edge[2]==try_edge[1]){
				# if self-edge, skip
				next
			}

			# if processed==FALSE, and if one of the two candidate nodes does not appear in the processed text (i.e., if it is not a noun or adjective, or if it is a stopword), disregard the edge
			if (processed==FALSE){
				if ((try_edge[1]%in%terms_list_processed==FALSE)|(try_edge[2]%in%terms_list_processed==FALSE)){
					next
				}
			}

			# if edge has met all the criteria, look it up in the current edge list
			value = edges_info[[paste(try_edge, collapse=" ")]]
			
			if (!is.null(value)){
			  # if edge has already been seen, update its weight
			  edges_info[[paste(try_edge,collapse=" ")]] = edges_info[[paste(try_edge,collapse=" ")]] + 1
			  
			} else {
			  # if edge has never been seen, create it and assign it a unit weight
			  edges_info[[paste(try_edge,collapse=" ")]] = 1
			  k = k+1
			}

		} # end j loop

	} # end i loop

	# convert hash table to list then to data frame
	edges_info_list = as.list(edges_info)
	names_edges_info_list = names(edges_info_list)
	
	shared_length = length(edges_info)
	my_col_names = c("from","to","weight")
	edges_df = as.data.frame(matrix(nrow=shared_length, ncol=length(my_col_names)))
	
	for (i in 1:shared_length){
	  
	  from_to = unlist(strsplit(names_edges_info_list[[i]], split=" "))
	  
	  edges_df[i,] = c(from_to, edges_info_list[[i]])
	  
	}
	
	colnames(edges_df) = my_col_names

	output = list(output=edges_df)

}