overlap_html = function(my_summary, golden_summary, custom_stopwords, is_trad_doc){

	# separate punctuation from words (special characters need to be triple escaped)
	my_summary = gsub('"',"'",my_summary) # replace double quotes by single quotes
	golden_summary = gsub('"',"'",golden_summary)

	# separate punctuation from words
	my_summary = gsub("(?<=[,:;!.?'-])  (?=[,:;!.?'-])",' ',gsub("([,:;!?'-]|[.]+)", " \\1 ", my_summary),perl=T)
	golden_summary = gsub("(?<=[,:;!.?'-])  (?=[,:;!.?'-])",' ',gsub("([,:;!?'-]|[.]+)", " \\1 ", golden_summary),perl=T)

	if (!is_trad_doc){
		golden_split = unlist(strsplit(gsub('\\s+',' ',tolower(golden_summary)), split=" "))
	} else {
		golden_split = unlist(strsplit(gsub('\\s+',' ',golden_summary), split=" "))
	}

	summary_split = unlist(strsplit(gsub('\\s+',' ',my_summary), split=' ')) # remove extra whitespace and tokenize

	golden_split_stem = tolower(wordStem(golden_split))
	summary_split_stem = tolower(wordStem(summary_split))

	overlap = setdiff(unique(intersect(golden_split_stem,summary_split_stem)),custom_stopwords)

	# remove punctuation-only elements from overlap
	index_remove = which(unlist(lapply(overlap, function(x) nchar(gsub("[^[:alnum:][:space:]]", "", x))))==0)
	if(length(index_remove)>0){
	  overlap = overlap[-index_remove]
	}

	index_bold_golden = which(golden_split_stem%in%overlap)
	index_bold_summary = which(summary_split_stem%in%overlap)

	summary_split[index_bold_summary] = paste("<b>",summary_split[index_bold_summary],"</b>")
	my_html = summary_split

	my_html = unlist(lapply(my_html, function(x) gsub('<b> ','<b>',x)))
	my_html = unlist(lapply(my_html, function(x) gsub(' </b>','</b>',x)))

	golden_split[index_bold_golden] = paste("<b>",golden_split[index_bold_golden],"</b>")
	golden_html = golden_split

	golden_html = unlist(lapply(golden_html, function(x) gsub('<b> ','<b>',x)))
	golden_html = unlist(lapply(golden_html, function(x) gsub(' </b>','</b>',x)))

	output = list(my_html=my_html, golden_html=golden_html)
	
}