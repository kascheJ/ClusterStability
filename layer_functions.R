##### Layer algorithms for later rotation


# function for the first layer, called "boundary layer"
boundary_func = function(cluster_vec,neighbour){
    neighbour_cols = neighbour[,cluster_vec] # only cluster cols
    total_number = c(colSums(neighbour_cols)) # use above to calc neighbour to cluster objs
    cluster_neighbour = neighbour_cols[cluster_vec,] 
    cluster_number = c(colSums(cluster_neighbour)) # neighbours within cluster
    boundary = cluster_vec[total_number > cluster_number] # a border object have neighbours outside of the cluster
    return(boundary)
}

# function for the remaining layers
next_layer = function(cluster_vec, neighbour, layer){
  without_layer = cluster_vec[!(cluster_vec %in% layer)] # all objs left to assign
  if(length(without_layer)>1){
    cluster_mat = neighbour[cluster_vec,without_layer] # for picking out those who neigbour boundary
    inner_mat = neighbour[without_layer,without_layer] # for counting neighbours within those left to assign
    total_number = c(colSums(cluster_mat)) # counting boundary and assign neighbours
    inner_number = c(colSums(inner_mat)) # counting left to assign neighbours
    next_layer = without_layer[total_number > inner_number] # pick out next layer
  }else{next_layer = without_layer}
  return(next_layer)
}


# create layers of given cluster with help of above functions
layer_creator = function(cluster, neighbours_mat){
  boundary = boundary_func(cluster,neighbours_mat) # speciell function for start layer
  layer_list = list(boundary) # save the layers in list
  obj_left = length(cluster) - length(boundary) # how many left to assign to a layer
  used_obj = boundary # which objects have we used already?
  new_layer = c() # saver for expanding used_obj
  while(obj_left > 0){ #while we still have objects left to assign to layer
    new_layer = next_layer(c(cluster),neighbours_mat,used_obj) # create layer
    layer_list = append(layer_list, list(new_layer)) # append list of layers
    used_obj = c(used_obj, new_layer) # update used objects/polygons
    obj_left = length(cluster) - length(used_obj) # update No. of objects still to be assigned
  }
  return(layer_list) # return the list of this clusters layer
}



# coloring of the layers and also works for the clusters
layer_plot = function(layers, polygons){
  colors = rep(0, nrow(polygons)) # create vector to fill with coloring
  j = length(layers) # how many layers do we have?
  # get colors
  library(RColorBrewer)
  color_bank = c(brewer.pal(n=9,name= "Set1"),
                 brewer.pal(n=12,name="Set3"),
                 brewer.pal(n=8,name="Dark2"))
  for(i in 1:j){
    colors[layers[[i]]] = c(color_bank[i]) # fill every layer with a unique color
  }
  return(plot(polygons, col = colors)) # return a plot
}





