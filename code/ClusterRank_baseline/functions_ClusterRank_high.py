# high level functions to run ClusterRank algorithm (Garg et al. 2009)
import functions_ClusterRank_low

def clustering_step(len_all_sentences, 
                    cleaned_sentences, 
                    all_words_unique_idf, 
                    max_idf, window_threshold, 
                    sim_threshold, 
                    original_sentences):
                        
    # this function performs the first step of the ClusterRank algorithm: clustering
    
    ##################
    # initialization #
    ##################
    
    # membership list will be updated at each iteration:
    # start with each sentence as a cluster on its own
    membership = list(range(len_all_sentences))
    
    # get window combinations
    combis = functions_ClusterRank_low.get_windows_combinations(window_threshold)
    
    # start with a (1,1) window
    pp = 0
    window_above = combis[pp][0]
    window_below = combis[pp][1]
        
    ########################
    # clustering algorithm #
    ########################
    n_clusters = 7 # initialize value
    
    while ((max(window_above, window_below) <= window_threshold) and (n_clusters > 2*window_threshold)):
        # we stop if windows are of size equal to window_threshold and if the number of clusters is less than or equal to window_threshold*2
        print "current window:", (window_above, window_below)
                    
        # identify how many clusters there are and assign them temporary IDs
        cluster_IDs = list(set(membership))
        n_clusters = len(cluster_IDs)    
        print "there are",n_clusters,"clusters"
        
        sim_all_merge_points = list()
        # for each possible merge point
        for merge_point in range(1,len(cluster_IDs)):
            
            # define the 2 blocks        
            
            # address initial problem of window "going outside text"
            if merge_point < window_above:
                IDs_block_above = cluster_IDs[:merge_point]
            else:
                IDs_block_above = cluster_IDs[(merge_point - window_above):merge_point]
            
            IDs_block_below = cluster_IDs[merge_point:(merge_point + window_below)]
        
            # get indexes of the sentences belonging to each block
            sent_in_block_above = functions_ClusterRank_low.get_sent_indexes(IDs_block_above, membership)
            sent_in_block_below = functions_ClusterRank_low.get_sent_indexes(IDs_block_below, membership)
               
            # compute the weights of the words in each block
            weights_words_in_block_above = functions_ClusterRank_low.compute_weights_words_in_block(sent_in_block_above,
                                                                                                    cleaned_sentences, 
                                                                                                    all_words_unique_idf, 
                                                                                                    max_idf)
                                                                          
            weights_words_in_block_below = functions_ClusterRank_low.compute_weights_words_in_block(sent_in_block_below,
                                                                                                    cleaned_sentences, 
                                                                                                    all_words_unique_idf, 
                                                                                                    max_idf)
            sim = functions_ClusterRank_low.compute_pair_sim(weights_words_in_block_above, weights_words_in_block_below)

            sim_all_merge_points.append(sim)
               
        max_sim = max(sim_all_merge_points)
        
        if max_sim > sim_threshold:
            print "winning pair found"
            
            # The best merge point is the one corresponding to the most similar pair. 
            # If two or more such points exist, select the first.
            # 1 is added because the similarity list starts at 0 while the merge point list starts at 1.
            best_merge_point = sim_all_merge_points.index(max_sim) + 1               
            
            # Get the IDs of the clusters above and below the best merge point
            # (this gives us our two winning blocks).
            if best_merge_point < window_above:
                IDs_win_block_above = cluster_IDs[:best_merge_point]
            else:
                IDs_win_block_above = cluster_IDs[(best_merge_point - window_above):best_merge_point]
            
            IDs_win_block_below = cluster_IDs[best_merge_point:(best_merge_point + window_below)]
            
            # get indexes of the sentences belonging to each winning block
            sent_in_win_block_above = functions_ClusterRank_low.get_sent_indexes(IDs_win_block_above, membership)
            sent_in_win_block_below = functions_ClusterRank_low.get_sent_indexes(IDs_win_block_below, membership)
            
            sent_in_win_pair = sent_in_win_block_above + sent_in_win_block_below
            
            # assign these sentences similar membership values (<=> merge the 2 blocks)
            # the membership value of the 1st sentence of the first block is used and will replace
            # the membership values of all the other sentences in the winning pair
            
            new_membership_value = sent_in_win_block_above[0] 
            
            for i, index_temp in enumerate(sent_in_win_pair):
                membership = [new_membership_value if value == index_temp else value for value in membership]
                
        else:
            print "no winning pair found, updating windows"
            # we don't have a winning pair, change the windows and try again
            if (pp + 1) < len(combis):
                pp = pp + 1
                window_above = combis[pp][0]
                window_below = combis[pp][1]
            else:
                print "cannot try anymore. stop."
                window_above = window_threshold + 1

    ###################
    # post-processing #
    ###################

    # group original sentences by cluster
    cluster_content = list()
    for i, label in enumerate(set(membership)):
        cluster_content.append((label, 
                                [sentence for i, sentence in enumerate(cleaned_sentences) if membership[i] == label],
                                [sentence for i, sentence in enumerate(original_sentences) if membership[i] == label],     
                                ))
    
    # get respective sizes of clusters
    cluster_size = [len(element[1]) for element in cluster_content]
    print "final number of clusters:", len(cluster_size)
    print "cluster sizes:", cluster_size
    
    # cluster_content contains: (1) the cluster ID, (2) a list of list of cleaned tokens (one for each sentence in the cluster),
    # and a (3) list of the original sentences belonging to the cluster 
    
    return {"cluster_content": cluster_content, "cluster_size": cluster_size}

def graph_building_step(cluster_content, cluster_IDs, all_words_unique_idf, max_idf):
    import igraph
    import functions_ClusterRank_low
    
    # each cluster is represented as a node in a fully connected graph
    g = igraph.Graph.Full(n = len(cluster_IDs), directed=False, loops=False)
    
    # assign cluster IDs as vertex names
    g.vs["names"] = cluster_IDs
        
    # assign edge weights based on cluster similarity
    
    edge_weights = list()
    
    for i, current_edge in enumerate(list(g.es)):
        
        source_node_id = current_edge.source
        target_node_id = current_edge.target
        
        source_node = g.vs["names"][source_node_id]
        target_node = g.vs["names"][target_node_id]
                
        # compute weights of words in source node
                
        cleaned_sentences = cluster_content[cluster_IDs.index(source_node)][1]
        
        if len(cleaned_sentences[0])==0:
            sim = 0
            
        else:
        
            sent_in_block = range(len(cleaned_sentences))
        
            weights_words_in_source_node = functions_ClusterRank_low.compute_weights_words_in_block(sent_in_block, 
                                                                                                cleaned_sentences, 
                                                                                                all_words_unique_idf, 
                                                                                                max_idf)
                                                                                                            
            cleaned_sentences = cluster_content[cluster_IDs.index(target_node)][1]
            
            if len(cleaned_sentences[0])==0:
                sim = 0
            
            else:
                
                sent_in_block = range(len(cleaned_sentences))
                                   
                # compute weights of words in target node
                weights_words_in_target_node = functions_ClusterRank_low.compute_weights_words_in_block(sent_in_block, 
                                                                                                cleaned_sentences, 
                                                                                                all_words_unique_idf, 
                                                                                                max_idf)
        
                sim = functions_ClusterRank_low.compute_pair_sim(weights_words_in_source_node, weights_words_in_target_node)
                                                                
        edge_weights.append((source_node,target_node,sim))
    
    # normalize edge weights
    
    normalized_edge_weights = list()  
    
    for i, element in enumerate(edge_weights):
        source = element[0]
        target = element[1]
        weight_current_edge = element[2]
        
        # sum of weights of all edges involving the source and target
        sum_other_weights = sum( [weight[2] for weight in edge_weights 
                                  if (source in weight[0:2] or target in weight[0:2])]
                               )
                               
        if sum_other_weights>0:
            n_e_w = round(weight_current_edge /sum_other_weights, 5)
        else:
            n_e_w = 0
        
        normalized_edge_weights.append((source, target, n_e_w))
    
    return {"graph": g, "n_e_weights": normalized_edge_weights}
    
def sentence_scoring_step(cluster_IDs, cluster_content, normalized_edge_weights, ranks, all_words_unique_idf, max_idf):   
    
    # initialize dictionary to store centroids of each cluster
    all_centroids = dict(zip(cluster_IDs,[0]*len(cluster_IDs)))
    
    # initialize dictionary to store scores of sentences in each cluster
    all_sent_scores = dict(zip(cluster_IDs,[0]*len(cluster_IDs)))
    
    # iterate over clusters
    for i, current_cluster in enumerate(cluster_content):
        
        ##################################
        # 1. compute centroid of cluster #
        ##################################
        # build the centroid of each cluster by only taking into account words shared
        # with other clusters
        
        # get words belonging to current_cluster
        words_in_cc = [word for sublist in current_cluster[1] for word in sublist]
        
        if len(words_in_cc)==0:
            all_sent_scores[current_cluster[0]] = 0
        
        else:
        
            # iterate over those words
            weights_words_in_cc = dict(zip(words_in_cc,[0]*len(words_in_cc)))
            
            for j, word_in_cc in enumerate(words_in_cc):
                
                # iterate over clusters different from current_cluster
                other_clusters = list()
                for k, current_cluster_comp_current in enumerate([cluster for cluster in cluster_content if cluster[0]!=current_cluster[0]]):
                    
                    # if current_cluster_comp_current contains word_in_cc
                    if (word_in_cc in [word for sublist in current_cluster_comp_current[1] for word in sublist]):
                        
                        # get normalized weight of edge linking current_cluster and current_cluster_comp_current
                        current_n_e_w = [element[2] for element in normalized_edge_weights if (
                                        (element[0]==current_cluster[0] and element[1]==current_cluster_comp_current[0]) or 
                                        ((element[0]==current_cluster_comp_current[0] and element[1]==current_cluster[0]))
                                        )]
                                    
                        # get PageRank score of current_cluster_comp_current
                        current_cluster_comp_current_PR_score = ranks[cluster_IDs.index(current_cluster_comp_current[0])]
                    
                        current_product = current_cluster_comp_current_PR_score * current_n_e_w[0]  * all_words_unique_idf[word_in_cc]        
                        
                        other_clusters.append(round(current_product,5))
                           
                weights_words_in_cc[word_in_cc] = round(sum(other_clusters),5)
            
            # save centroid of current cluster
            all_centroids[current_cluster[0]] = weights_words_in_cc
            
            #########################################################
            # 2. compute scores of all sentences in current cluster #
            #########################################################
            
            # get PageRank score of current_cluster
            current_cluster_PR_score = ranks[cluster_IDs.index(current_cluster[0])]
        
            # iterate over the sentences of current_cluster
            sent_scores_in_cc = list()
            
            for j, sentence in enumerate(current_cluster[1]):
                
                # compute similarity of sentence with cluster's centroid
                
                # weights of the words in cluster are taken as computed at the previous step
                weights_words_in_centroid = all_centroids[current_cluster[0]]
                
                # weights of the words in sentence are simply taken as words IDF
                weights_words_in_sentence = dict((word, all_words_unique_idf[word]) for word in sentence)
                
                # compute cosine similarity between sentence and cluster's centroid
                sim = functions_ClusterRank_low.compute_pair_sim(weights_words_in_sentence, weights_words_in_centroid)
    
                sent_scores_in_cc.append(round(sim * current_cluster_PR_score, 7))   
            
            all_sent_scores[current_cluster[0]] = sent_scores_in_cc
        
    return {"centroids": all_centroids, "scores": all_sent_scores}
                                                                 
def Cluster_Rank(raw_text, path_name_stopwords, sim_threshold, window_threshold):

    #######################################
    ### Cluster Rank (Garg et al. 2009) ###
    #######################################

    import string
    import functions_preprocessing
    import functions_ClusterRank_low
    import functions_ClusterRank_high

    def flatten(x):
        result = []
        for el in x:
            if hasattr(el, "__iter__") and not isinstance(el, basestring):
                result.extend(flatten(el))
            else:
                result.append(el)
        return result
    
    ######################
    ### 0. preliminary ###
    ######################
    
    # get cleaned utterances from transcript
    all_sentences = functions_preprocessing.clean_text(text = raw_text,
                                                       path_name_stopwords = path_name_stopwords,
                                                       punct = string.punctuation,
                                                       remove_numb = False, 
                                                       sent_token = True,
                                                       remove_punct = True,
                                                       pos_filtering = False,
                                                       remove_stopwords = True)
    
    len_all_sentences = len(all_sentences)
    cleaned_sentences = [item[1] for item in all_sentences]
    original_sentences = [item[0] for item in all_sentences]
    
    # flatten list of lists and extract unique elements from it
    # (returns all the unique terms in the document)
    all_words_unique = list(set([word for sublist in cleaned_sentences for word in sublist]))
    
    # compute IDF for each unique term
    all_words_unique_idf = functions_ClusterRank_low.compute_all_IDF(all_words_unique,
                                                                     cleaned_sentences, 
                                                                     len_all_sentences)
                                                                     
    # save max IDF (corresponding to words appearing only once) for later
    max_idf = max(all_words_unique_idf.values())
    
    #####################
    ### 1. clustering ###
    #####################
        
    clustering_step_output = functions_ClusterRank_high.clustering_step(len_all_sentences, 
                                                                        cleaned_sentences, 
                                                                        all_words_unique_idf, 
                                                                        max_idf, 
                                                                        window_threshold, 
                                                                        sim_threshold, 
                                                                        original_sentences)
                                                    
    cluster_content = clustering_step_output["cluster_content"]
    
    #########################
    ### 2. graph building ###
    #########################
    cluster_IDs = [element[0] for element in cluster_content]
    
    graph_building_step_output = functions_ClusterRank_high.graph_building_step(cluster_content,cluster_IDs,all_words_unique_idf,max_idf)
    
    g = graph_building_step_output["graph"]
    normalized_edge_weights = graph_building_step_output["n_e_weights"]
    
    ###################
    ### 3. PageRank ###
    ###################
    ranks = g.pagerank(directed=False, damping=0.85, weights=[element[2] for element in normalized_edge_weights])
    ranks = [round(element, 5) for element in ranks]
    
    # get indexes corresponding to PageRank scores in decreasing order
    # indexes_dec_order = sorted(range(len(ranks)), key=ranks.__getitem__,reverse=True)
    
    ## optional:
    ## inspect top 3 most important clusters in terms of PageRank scores
    #operator.itemgetter(*indexes_dec_order[:3])(cluster_content)
    #
    ## three biggest clusters
    #operator.itemgetter(*indexes_dec_order[:3])(clustering_step_output["cluster_size"])
    
    ###########################
    ### 4. Sentence scoring ###
    ###########################
    
    sentence_scoring_step_output = functions_ClusterRank_high.sentence_scoring_step(cluster_IDs, cluster_content, normalized_edge_weights, ranks, all_words_unique_idf, max_idf)
       
    sentence_scores = sentence_scoring_step_output["scores"]
    
    # link the sentences within each cluster to their scores
    
    sentences_final = list()
    scores_final = list()
    
    for i, current_cluster in enumerate(cluster_content):
        sentences_final.append(current_cluster[2])
        scores_final.append(sentence_scores[current_cluster[0]])
    
    
    sentences_final = [item for sublist in sentences_final for item in sublist]
    
    scores_final = flatten(scores_final)
    
    return sorted(zip(scores_final,sentences_final), reverse=True)