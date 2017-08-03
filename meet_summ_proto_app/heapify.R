heapify = function(input_vector, min){
  
  # below is the maxheapify algorithm
  # minheapify = maxheapify with sign change at the beginning and at the end
  
  if (min==TRUE){input_vector = - input_vector}
  
	count = length(input_vector)

	start = floor(count/2)
	 
	while (start >= 1){
	  root = start
	  theEnd = count
	  
	  while ((root * 2) <= theEnd){
	    
	    child = root * 2
	    
	    if ((child + 1 <= theEnd) & (input_vector[child] < input_vector[child+1])){
	      child = child + 1
	    }
	    
	    if (input_vector[root] < input_vector[child]){
	      
	      temp = input_vector[root]
	      input_vector[root] = input_vector[child]
	      names(input_vector)[root] = names(input_vector)[child]
	      input_vector[child] = temp
	      names(input_vector)[child] = names(temp)
	      
	      root = child
	      
	    } else {
	      break
	    }
	    
	  }
	  
		start = start - 1
	}
	
	if (min==TRUE){input_vector = - input_vector}
	
output = list(heap=input_vector)
}