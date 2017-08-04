assign_attributes_to_graph_nocomm = function(g, method, degeneracy, directed_mode, v_g_name, l_v_g_name){
	
	# if method=="TR", no need to apply k-core or k-truss
	if (method!="TR"){
		
		# perform k-core or k-truss decomposition
		cores = cores_dec(g, degeneracy, directed_mode)$cores
		V(g)[names(cores)]$core_no = cores
		
		# get node membership
		membership = 0*1:l_v_g_name

		# assign membership to each node
		for (i in 1:l_v_g_name){
			membership[i] = as.numeric(cores[v_g_name[i]])
		}

		V(g)$core_no = membership
		
	} else {
		
		V(g)$core_no = rep(NA, l_v_g_name)
		
	}

	V(g)$comm = rep(NA, l_v_g_name)

	output=list(g=g)
	
}