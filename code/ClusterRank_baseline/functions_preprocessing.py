# functions for creating and analyzing graphs of words
import csv
import nltk
import re
import time

#nltk.download('punkt') # for tokenization
#nltk.download('maxent_treebank_pos_tagger') # for POS tagging
#nltk.download('stopwords')

def clean_text(text, path_name_stopwords, punct, remove_numb = False, sent_token = False, remove_punct = False, pos_filtering = False, remove_stopwords = False):
    
    stemmer = nltk.PorterStemmer()    
    
    # This function applies standard pre-processing steps to raw text

    # If text is not split based on sentences (sent_token = False),
    # text is converted to lower case, stopwords are removed, Porter stemming is applied.
    # Punctuation removal and POS-based filtering (nouns and adjectives are kept) is optional
    # via the remove_punct and pos_filtering arguments.
    # A list of tuples is returned:
    # each tuple contains the original and stemmed terms that were kept
    # the order corresponds to the order of appearance in the text

    # If text is split based on sentences (sent_token = True),
    # for each sentence the following actions are performed:
    #                    - lower case conversion
    #                    - punctuation removal (preserving intra-word dashes)
    #                    - tokenization
    #                    - stopword removal
    #                    - Porter stemming.
    # The final output is a list of 2-tuples (original sentence, cleaned tokens)

    # In each case extra-word number removal (dates like "2015" are kept) is optional

    # stpwds = set(nltk.corpus.stopwords.words("english"))

    stpwds = list()
    with open(path_name_stopwords, 'r+') as csvfile:
        rows = csv.reader(csvfile, delimiter=' ')
        for row in rows:
            stpwds.append(row)
    stpwds = [item for sublist in stpwds for item in sublist]
    stpwds = stpwds[1:len(stpwds)]
    
    # sanitize
    print 'Decoding...'
    t = time.time()
    if type(text) == 'unicode':
      text = text.decode("utf-8")
    print 'Done in %f' % (time.time() - t)

    # strip extra white space
    print 'Removing extra whitespaces'
    t = time.time()
    text = re.sub(" +"," ",text)
    print 'Done in %f' % (time.time() - t)

    # strip leading and trailing white space
    print 'Stripping leading and trailing white space...'
    t = time.time()
    text = text.strip()
    print 'Done in %f' % (time.time() - t)

    if sent_token == True:
        # turn text into a list of sentences
        sent_detector = nltk.data.load('tokenizers/punkt/english.pickle')
        print 'Tokenizing...'
        t = time.time()
        sentences = sent_detector.tokenize(text)
        print 'Done in %f' % (time.time() - t)

        tokens_cleaned_sentences = list()
       
        for i, sentence_temp in enumerate(sentences):
            # convert to lower case
            print 'Converting to lower case...'
            t = time.time()
            sentence_temp = sentence_temp.lower()
            print 'Done in %f' % (time.time() - t)
            # remove punctuation
            print 'Removing punctuation...'
            t = time.time()
            sentence_temp = "".join(l for l in sentence_temp if l not in punct)
            print 'Done in %f' % (time.time() - t)
            # tokenize
            print 'Word-level tokenizing...'
            t = time.time()
            tokens = nltk.word_tokenize(sentence_temp)
            print 'Done in %f' % (time.time() - t)
            if remove_stopwords == True:
                # remove stopwords
                print 'Removing stopwords...'
                t = time.time()
                tokens_keep = [token for token in tokens if token not in stpwds]
                print 'Done in %f' % (time.time() - t)
            else:
                tokens_keep = tokens
            # stem
            print 'Stemming...'
            t = time.time()
            for j, token_keep_temp in enumerate(tokens_keep):
                tokens_keep[j] = stemmer.stem(token_keep_temp)
            print 'Done in %f' % (time.time() - t)

            tokens_cleaned_sentences.append(tokens_keep)

        return(zip(sentences, tokens_cleaned_sentences))
        
    else:   
    
        # convert to lower case
        print 'Converting to lower case...'
        t = time.time()
        text = text.lower()
        print 'Done in %f' % (time.time() - t)

        if remove_punct == True:
            # remove punctuation (preserving intra-word dashes)
            print 'Removing punctuation...'
            t = time.time()
            text = "".join(l for l in text if l not in punct)
            print 'Done in %f' % (time.time() - t)
    
        # strip extra white space
        print 'Stripping extra white space...'
        t = time.time()
        text = re.sub(" +"," ",text)
        print 'Done in %f' % (time.time() - t)
    
        # strip leading and trailing white space
        print 'Stripping...'
        t = time.time()
        text = text.strip()
        print 'Done in %f' % (time.time() - t)

        # tokenize
        print 'Tokenizing...'
        t = time.time()
        tokens = nltk.word_tokenize(text)
        print 'Done in %f' % (time.time() - t)

        if pos_filtering == True: # TODO very slow. Optimize
            # POS-tag and keep only nouns and adjectives following TextRank paper
            print 'POS filtering...'
            t = time.time()
            tagged_tokens = nltk.pos_tag(tokens)
            tokens_keep = list()
            for i in range(len(tagged_tokens)):
                item = tagged_tokens[i]
                if (
                item[1] == "NN" or
                item[1] == "NNS" or
                item[1] == "NNP" or
                item[1] == "NNPS" or
                item[1] == "JJ" or
                item[1] == "JJS" or
                item[1] == "JJR"
                ):
                 tokens_keep.append(item[0])
            print 'Done in %f' % (time.time() - t)
        else:
            tokens_keep = tokens
    
        if remove_stopwords == True: # FIXME tokens_keep isn't influenced by pos_filtering
          # remove stopwords
          print 'Removing stopwords...'
          t = time.time()
          tokens_keep = [token for token in tokens_keep if token not in stpwds]
          print 'Done in %f' % (time.time() - t)
        else:
          tokens_keep = tokens
          
        # apply Porter stemmer
        print 'Porter stemming...'
        t = time.time()
        tokens_keep_stemmed = list()
        for i in range(len(tokens_keep)):
            tokens_keep_stemmed.append(stemmer.stem(tokens_keep[i]))  
        print 'Done in %f' % (time.time() - t)

        return(zip(tokens_keep, tokens_keep_stemmed))