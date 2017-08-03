get_elbow_point = function(sorted_scores){
  
  # ! sorted_scores need be in decreasing order
  
  if (length(sorted_scores)<3){
    
    # if there are only two points return the first one (i.e., main core)
    elbow_point = 1
    
  } else {
  
    sorted_scores = as.numeric(sorted_scores)
    
    # first point
    first_point = c(1,sorted_scores[1])
    # last point
    last_point = c(length(sorted_scores),sorted_scores[length(sorted_scores)])
    
    # compute distance between each point of "sorted_scores" and the "first-last" line
    k = 1
    distances = list()
    
    for (point in sorted_scores){
      
      numerator = abs((last_point[2] - first_point[2])*k - (last_point[1] - first_point[1])*point + last_point[1]*first_point[2] - last_point[2]*first_point[1])
      denominator = ((last_point[2] - first_point[2])^2 + (last_point[1] - first_point[1])^2 )^0.5
      distances[[k]] = numerator/denominator
      k = k + 1
      
    }
    
    distances = unlist(distances)
    
    # the point most distant from the line is the elbow point (https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line)
    candidate_elbow_points = which(distances==max(distances))
    
    if (length(candidate_elbow_points)>=2){
      
      # in case the points form a perfect line, select the second one as to not raise an error in the upper level function "best level density"
      elbow_point = candidate_elbow_points[2]
      
    } else {
      
      elbow_point = candidate_elbow_points
      
    }
    
  }
  
  output = list(elbow_point = elbow_point)
  
}