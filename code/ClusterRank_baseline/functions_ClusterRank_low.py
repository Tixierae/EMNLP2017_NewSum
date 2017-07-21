# low level functions for ClusterRank algorithm (Garg et al. 2009)
import math

def get_windows_combinations(window_threshold):
    windows_combinations = list()
    windows_combinations.append((1,1))
    
    while (max(windows_combinations)[0] == max(windows_combinations)[1] == window_threshold)==False:
        ti = max(range(len(windows_combinations)))
        if windows_combinations[ti][0] == windows_combinations[ti][1]:
            windows_combinations.append((1,windows_combinations[ti][1]+1))
        if windows_combinations[ti][0] < windows_combinations[ti][1]:
            windows_combinations.append((windows_combinations[ti][1],windows_combinations[ti][0]))
        if windows_combinations[ti][0] > windows_combinations[ti][1]:
            windows_combinations.append((windows_combinations[ti][1]+1,windows_combinations[ti][0]))
    
    return windows_combinations
 
def compute_all_IDF(all_words_unique, cleaned_sentences, len_all_sentences):
    
    # initialize dictionary to store IDF values of each term
    all_words_unique_idf = dict(zip(all_words_unique,[0]*len(all_words_unique)))

    for i, current_word in enumerate(all_words_unique):
        # initialize counter
        numb_sent_with_current_word = 0
        # iterate over all sentences and count
        for j, current_sentence in enumerate(cleaned_sentences):
            if current_word in current_sentence:
                numb_sent_with_current_word = numb_sent_with_current_word + 1
        current_word_idf = round(math.log(len_all_sentences/numb_sent_with_current_word), 5)       
        all_words_unique_idf[current_word] = current_word_idf
        
    return all_words_unique_idf
    
def compute_weights_words_in_block(sent_in_block, cleaned_sentences, all_words_unique_idf, max_idf):
    # get words belonging to sentences in block
    to_flatten = [cleaned_sentences[j] for j in sent_in_block]
    words_in_block = [word for sublist in to_flatten for word in sublist]
    
    words_in_block_weights = list()
    
    for i, current_word in enumerate(words_in_block):
        # get number of sentences in block in which current word appears
        
        # if current word appears only in one sentence of the document
        # we know for sure it can only appear in one sentence of the block
        if all_words_unique_idf[current_word] == max_idf:
            f_current_word = 1
        else:
            f_current_word = 0
            for j in range(len(sent_in_block)):
                if current_word in cleaned_sentences[sent_in_block[j]]:
                    f_current_word = f_current_word + 1
        
        # compute weight (TF_IDF) of current word and store it
        words_in_block_weights.append(f_current_word * all_words_unique_idf[current_word])

    return dict(zip(words_in_block, words_in_block_weights))

def get_sent_indexes(IDs_block, membership):
    # for a given block this function returns the indexes of the sentences belonging to that block (as a list)
    r_l_m = range(len(membership))
    sent_in_block = list()
    for n in range(len(IDs_block)):
        to_add = [j for j in r_l_m if membership[j] == IDs_block[n]]
        sent_in_block.append(to_add)
    # flatten list
    sent_in_block = [number for sublist in sent_in_block for number in sublist]
    
    return sent_in_block

def compute_pair_sim(weights_words_in_block_above, weights_words_in_block_below):
    # this function computes the cosine similarity between two blocks
    
    # find common words between the two blocks
    shared_words = (list(set(weights_words_in_block_above.keys())
                         .intersection(weights_words_in_block_below.keys())))
                         
    if len(shared_words)==0:
        sim = 0
        
    else:
    
        # get product of word weights for the shared words
        product_shared_words_weights = list()
        for o, current_shared_word in enumerate(shared_words):
            
            current_product = round(weights_words_in_block_above[current_shared_word]
                              *weights_words_in_block_below[current_shared_word], 5)
            
            product_shared_words_weights.append(current_product)
        
        # compute cosine similarity (based on TF-IDF word weights) between the two blocks
        sim_num = sum(product_shared_words_weights)
        sim_denom_1 = [round(weight ** 2, 5) for weight in weights_words_in_block_above.values()]
        sim_denom_2 = [round(weight ** 2, 5) for weight in weights_words_in_block_below.values()]
        sim_denom = round(math.sqrt(sum(sim_denom_1)) * math.sqrt(sum(sim_denom_2)), 5)

        if sim_denom==0:
            sim = 0
        else:
            sim = round(sim_num / sim_denom, 5)
    
    return sim   