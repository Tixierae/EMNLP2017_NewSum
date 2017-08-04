assign_attributes_to_graph_initial = function(edges_df, weighted, directed){
	
   edges_df_sorted = t(apply(edges_df[,1:2], 1, sort))
	index_duplicated = which(duplicated(edges_df_sorted))

	# remove edges that would be the same if direction was not taken into account
	edges_df_und = edges_df[-index_duplicated,]

	if (directed==FALSE){

		edges_df = edges_df_und

	}

	# make graph out of data frame

	if (weighted == TRUE){

		# keep edge weights and add labels
		g = graph.data.frame(edges_df, directed=directed)
		E(g)$label = as.character(E(g)$weight)

	} else {

		# discard "weight" attribute (the third column), and don't assign labels
		g = graph.data.frame(edges_df[,1:2], directed=directed)

	}

	v_g_name = V(g)$name
	l_v_g_name = length(v_g_name)

	output = list(g=g, v_g_name=v_g_name, l_v_g_name=l_v_g_name, weighted=weighted, edges_df_und=edges_df_und)
	
}