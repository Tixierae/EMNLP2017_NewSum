string_join = function(s1,s2) {
  ss1 = strsplit(s1,'\\s+')[[1]]
  ss2 = strsplit(s2,'\\s+')[[1]]
  if (length(ss1)==0) {return(s2)}
  if (length(ss2)==0) {return(s1)}
  n = 0
  for (i in seq(min(length(ss1),length(ss2)),1))
    if (all(ss1[seq(to=length(ss1),len=i)]==ss2[seq(1,len=i)])) {
      n = i
      break
    }
  output = paste(collapse=' ',c(ss1,if (n==0) ss2 else ss2[-1:-n]))
  output = list(output = output)
}