tagPOS <-
function(character){
	# part-of-speech tagger based on openNLP Apache
	# required package: "openNLP"
	s = as.String(character)
	a = annotate(s, list(sent_token_annotator, word_token_annotator))
	aa = annotate(s, pos_tag_annotator, a)
	output = list(output = unlist(lapply(aa[aa$type=="word"]$features, `[[`, "POS")))
}