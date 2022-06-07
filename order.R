

### ordering of layer/border

# finer layers for ordering
finer_layers = function(layers,
                        neighbour_list){
  # to change and return
  ret_layers = rev(layers)
  
  # to use as fix
  rev_layers =  rev(layers)
  
  neighbour_mat = nb_list2mat(neighbour_list)
  n_layers = length(layers)
  for(i in 2:n_layers){
    layer1 = rev_layers[[i]]
    layer2 = rev_layers[[i-1]]
    
    # check which obj that are not neighbours to the next layer (layer2)
    sub_neighs = as.matrix(neighbour_mat[layer2, layer1])
    sums_neighs = colSums(sub_neighs) #sum the number of neighs to layer2
    new_layer1 = layer1[which(sums_neighs != 0)]
    ret_layers[[i]] = new_layer1
  }
  return(rev(ret_layers))
}


# order the observations in a sequence
simple_ordering = function(main_layer, neighbour_list){
index = 1
stop = 0
while(index <= length(main_layer) & stop == 0 ){
#order = sample(main_layer,1)
order = main_layer[index]
remain = main_layer[!(main_layer %in% order)]
n_revs = 0
while(length(remain)>0 & n_revs < length(main_layer)){
  obj = order[length(order)]
  objs_neigh = neighbour_list[[obj]]
  chos_neigh = (objs_neigh[objs_neigh %in% remain])[1]
  chos_neigh
  if(is.null(chos_neigh) | is.na(chos_neigh)){
    order= rev(order)
    n_revs = n_revs+1
    }else{
    if(length(order)==1){order = append(order,chos_neigh)} else{
      neigh2 = neighbour_list[[order[length(order)-1]]]
      if(chos_neigh %in% neigh2){order = c(order[1:(length(order)-1)],
                                           chos_neigh,
                                           order[length(order)])} else{
          order = append(order,chos_neigh)
                                           }
    }
  }
  remain = main_layer[!(main_layer %in% order)]
}
index = index +1
if(length(remain)==0) stop = 1
}
if(length(order)!= length(main_layer)) {
  print("Not possible to order all")
}
return(order)
}


# order all layers at once
order_layers = function(layers, neighbours_list){
  ret_layers = list()
  for(i in layers){
    ord_layer = simple_ordering(i,neighbours_list)
    ret_layers = append(ret_layers,list(ord_layer))
  }
  # remove core if it can't be ordered
  if(length(rev(layers)[[1]]) != length(rev(ret_layers)[1]) ){
    ret_layers = ret_layers[-length(ret_layers)]
    print("core can't be ordered, removed")
  }
  return(ret_layers)
} #works
  


# gives back 1 if the objects are neighbours
is.neigh = function(obj1,obj2,neighbour_list){
  ret = 0
  if(obj1 %in% neighbour_list[[obj2]]) ret = 1
  return(ret)
}



# sync layers
sync_layers = function(layers, neighbour_list){
  ret_layers = layers
  n_layers  = length(layers)
  for(i in 1:(n_layers-1) ){
    
    #layer1
    objs_layer1 = ret_layers[[i]]
    n1 = length(objs_layer1)
    
    #layer2
    layer2 = ret_layers[[i+1]]
    n2 = length(layer2)
    
    #create list of all possible rotations of layer2
    rots = append(list(layer2),list(rev(layer2))) # add the reverse of the original layer
    rots = append(rots, rotation(layer2)) #add the standard rotations
    rots = append(rots, rotation(rev(layer2))) #add the alternated rotations
    n_rots = length(rots)
    
    sync_seq = seq(from=1, to=n2, length.out = n1)
    
    results = rep(0,n_rots)
    for(j in 1:n_rots) {
      objs_layer2 = rots[[j]]
      sum_holder = 0
      for(s in 1:n1){
      sum_holder = is.neigh(objs_layer1[s],
                            objs_layer2[sync_seq[s]],
                            neighbour_list) + sum_holder
      }
      results[j] = sum_holder
    }
    
    sync_max = which.max(results)
    rot_sync = rots[sync_max]
    ret_layers[i+1] = rot_sync
    }
    return(ret_layers)
}



# pace layers; output is a matrix with the rotation that each layer should use at each round

layer_pacer = function(layers){#input; synced layers
  lengths = sapply(layers, length)
  n_layers = length(layers)
  n1 = lengths[1]
  result = matrix(2:n1, (n1-1) , n_layers)
  for (i in 2:n_layers) {
    n2 = lengths[i]
    result[,i] = round( seq(from=1, to = n2, length.out = n1)[-1])
  }
  
return(result)
  
  } #works





