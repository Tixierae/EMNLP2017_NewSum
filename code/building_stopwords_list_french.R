library(tm)
library(doParallel)
library(foreach)
standard_stopwords = stopwords('fr')

path_root = 'C:/Users/mvazirg/Desktop/french_corpus_linagora'

my_path = paste0(path_root,'/ACSYNT')
all_files_acsynt = list.files(my_path)
all_text_acsynt = unlist(lapply(paste0(my_path,'/',all_files_acsynt), function(x) readLines(x, encoding='UTF-8')))

my_path = paste0(path_root,'/CorpusUBS_Acc')
all_files_ubs = list.files(my_path) # all but starting with dot: pattern='^[^.]+$'
all_files_ubs = setdiff(all_files_ubs ,'Accueil_UBS_texte_corrigé')
all_text_ubs = unlist(lapply(paste0(my_path,'/',all_files_ubs), function(x) readLines(x, encoding='UTF-8')))

my_path = paste0(path_root,'/CorpusUBS_Acc/Accueil_UBS_texte_corrigé')
all_files_ubs_corr = list.files(my_path)
all_text_ubs_corr = unlist(lapply(paste0(my_path,'/',all_files_ubs_corr), function(x) readLines(x, encoding='UTF-8')))

my_path = paste0(path_root,'/tcof/2/Corpus')
# all but last 6 files (which are lists of topics I guess)
all_files_tcof = head(list.files(my_path, recursive = TRUE),-6)
all_text_tcof = unlist(lapply(paste0(my_path,'/',all_files_tcof), function(x) readLines(x, encoding='latin1')))

# convert character vectors to strings
all_text_acsynt = paste(all_text_acsynt, collapse=' ')
all_text_ubs = paste(all_text_ubs, collapse=' ')
all_text_ubs_corr = paste(all_text_ubs_corr, collapse=' ')
all_text_tcof = paste(all_text_tcof, collapse=' ')

# get a unique string
all_text = paste(c(all_text_acsynt,all_text_ubs,all_text_ubs_corr,all_text_tcof), collapse=' ')

# remove punctuation, strip extra whitespace
all_text = gsub('[[:punct:]]', ' ', all_text)
all_text = gsub('\\s+',' ', all_text)

# convert to lower case
all_text = tolower(all_text)

# write to file
writeLines(all_text, con=paste0(path_root, '/all_text_for_stopwords_bis.txt'))

all_text_read = readLines(paste0(path_root, '/all_text_for_stopwords_bis.txt'))

all_words = unlist(strsplit(all_text_read ,split=" "))

##### creating custom list of stopwords #####

all_words_unique = unique(all_words)

# we're not interested in the stopwords we already know
my_stopwords = standard_stopwords
all_words_unique = setdiff(all_words_unique, my_stopwords)

nc = detectCores()
cl = makeCluster(nc)
registerDoParallel(cl)
ptm = proc.time()

unique_word_counts = foreach(i = 1:length(all_words_unique), .export = c("all_words_unique","all_words")) %dopar% {                                                      
  length(which(all_words==all_words_unique[i]))
}

stopCluster(cl)
proc.time() - ptm

unique_word_counts = unlist(unique_word_counts)
names(unique_word_counts) = all_words_unique
unique_word_counts = sort(unique_word_counts, decreasing=TRUE)

# save
write.csv(unique_word_counts, paste0(path_root,"/unique_word_counts.csv"))

# then I edited the .csv file manually in Excel

custom_stopwords = read.csv(paste0(path_root,"/custom_stopwords_meeting_french.csv"), header=FALSE, stringsAsFactors=FALSE)[,1]

filler_words = read.csv('C:/Users/mvazirg/Documents/meet_summ_proto/filler_words_french.csv', header=FALSE, stringsAsFactors=FALSE)[,1]

full_stopwords = c(filler_words, standard_stopwords)

# save
write.csv(full_stopwords, paste0(path_root,"/custom_stopwords_full_french.csv"), row.names=FALSE, quote=FALSE)

# read back 
test = read.csv(paste0(path_root,"/custom_stopwords_full_french.csv"), header=TRUE, stringsAsFactors = FALSE)[,1]
