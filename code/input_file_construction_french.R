# script to construct an input file for French

path_root = 'C:/Users/mvazirg/Desktop/french_corpus_linagora'

my_text = readLines(paste0(path_root,'/Réunions RetD/Retranscription réunion retd 05.07.txt'))

# just make up start and end times since they are not available
start = 1:264
end = start*10

# make up roles (assuming two participants with one line per participant)
role = rep(c('part1','part2'),264/2)

asr_info_french = data.frame(start=start, end=end, role=role, text=my_text, stringsAsFactors=FALSE)

# save as .csv and space delimited text file
write.csv(asr_info_french, paste0(path_root,'/asr_info_french.csv'), row.names=FALSE)

# read back
test = read.csv(paste0('C:/Users/mvazirg/Desktop/french_corpus_linagora/asr_info_french.csv'), header=TRUE)

# save as space delimited text file
write.table(asr_info_french, paste0(path_root,'/asr_info_french.txt'), sep=' ', row.names=FALSE)

# read back
test = read.table(paste0(path_root,'/asr_info_french.txt'), sep=' ', header=TRUE)