### rotation test


# input: an ordered vector. Output: rotation_list

rotation = function(ordered_vec, id = F){
  return_list = list()
  vec = ordered_vec
  n = length(ordered_vec)
  for (i in 1:(n-1)) {
    ordered_vec = c(ordered_vec[2:n],ordered_vec[1])
    return_list = append(return_list, list(ordered_vec))
  }
  # if you wish to get the original layer, aka id object in 
  if(id) return_list = append(list(vec), return_list)
  return(return_list)
} #works


# output: list with all the rotations paced, in form of permutations
rotation_creator = function(synced_list){
  rotations = list()
  n_layers = length(synced_list)
  for(i in 1:n_layers){
    rotations = append(rotations, list(rotation(synced_list[[i]],id=T) ) )
  }
  
  # matrix for pacing the layers similar
  pace_mat = layer_pacer(synced_list)
  
  # convert the layers' perms to a single list, like permutation-list
  n_rots = nrow(pace_mat)+1
  count = 1
  temp_perm = rotations[[1]]
  for(lay in rotations[-1]){
    for(i in 2:n_rots ){
      pace_numb = pace_mat[i-1,count+1]
      temp_perm[[i]] = c(temp_perm[[i]], lay[[pace_numb]] )
    }
    count = count +1
  }
  temp_perm = temp_perm[-1] #remove id-object
  return(temp_perm)
}
  
  

# wrapper for layer rotation

rotation_test = function(cluster,
                         layers,
                         data,
                         neighbours_list,
                         min_cluster,
                         method,
                         seed = "time",
                         test_method = "rotation"){
  
  # create neighbour mat
  neighbour_mat = nb_list2mat(neighbours_list)
  
  # remove layers with only a single object, they can't be rotated
  lengths_layers = sapply(layers, length)
  layers = layers[lengths_layers>1]
  
  if(length(layers)>0){
  
      # remove all layers already here if the cluster only contains 2 layers
      if(length(layers)==2) if( length(layers[[2]]) == 1 ) layers = layers[-2]
      
      
      # if cluster has only a single layer
      if(length(layers) == 1){
        ordered_layers = simple_ordering(layers[1],neighbours_list)
        if(length(ordered_layers[[1]])== length(layers[[1]])){ #check if the vec could fully be ordered
          rotation_list = rotation(ordered_layers[[1]])
        }else rotation_list = list()
      }else{ 
        ### if the cluster has several layers    
        
        
            # finer layers
            layers_finer = finer_layers(layers,neighbours_list)
            
            # order layer
            ordered_layers = order_layers(layers_finer[-length(layers_finer)], 
                                          neighbours_list)
            
            # if only the border should be rotated
            if(test_method == "rotation_border"){
              ordered_layers = ordered_layers[1]
              if(length(ordered_layers[[1]]) == length(layers_finer[[1]]) ){ 
                rotation_list = rotation(ordered_layers[[1]])
              }else rotation_list = list()
              
            } else{ # if several layers should be rotated
              
            # control that all layers (except core-layer) could be ordered
              if(all( sapply(ordered_layers,length) == sapply(layers_finer[-length(layers_finer)], length) )){
                
                layer_lengths = sapply(ordered_layers, length)
                ordered_layers = ordered_layers[layer_lengths>1]
                
                if(length(ordered_layers)>1){
                
                # sync the layers
                synced_layers = sync_layers(ordered_layers,neighbours_list)
                
                # create rotations
                rotation_list = rotation_creator(synced_layers)
                }else rotation_list = rotation(ordered_layers[[1]])
            }else rotation_list = list()
          }
      }
  }else rotation_list = list()

    if(length(rotation_list)>0 ){
  # test the rotations with re-using the random test function
  rotation_result = random_test(cluster,
                                layer = c(),
                                number_perm = "rotation",
                                rotation_list,
                                data,
                                neighbour_mat,
                                min_cluster,
                                method,
                                seed)
  } else rotation_result = c("can not be rotated")
  
  return(rotation_result)
}






















