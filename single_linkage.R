
##### Linkage algorithm



## data preparing functions

# function to convert neighbour-list to matrix form of zeros and ones
nb_list2mat = function(nb_list){
  n = length(nb_list)
  mat = matrix(0,n,n)
  for(i in 1:n){
    mat[ nb_list[[i]] ,i ] = 1
  }
  return(mat)
}


# function  for creating dissimilarity matrix
create_diff = function(data, cols = c(11:18)){
  n = nrow(data)
  diff_matrix = matrix(0,n,n)
  
  data2 = as.matrix(data[,cols])
  for(i in 1:n){
    diff_matrix[i,] = rowSums(abs(t(t(data2)-c(data2[i,]))))
  }
  diff_matrix[upper.tri(diff_matrix)]= t(diff_matrix)[upper.tri(diff_matrix)]
  return(diff_matrix)
}


## algorithm functions


#function that takes the minimum non-zero value
which_min_nonzero = function(vec){
 non0_vec = vec[vec != 0]
 min_non0 = min(non0_vec)
 return(c(which(vec==min_non0, arr.ind = T))[1:2])
} #works



#function for storing clusters
cluster_storing = function(new_clustering, storing_list){ #arr_ind from which
  l = length(storing_list) #for while loop
  #new_clustering = unique(c(new_clustering))
  if(l>0){
    b = 2^10000
    counter = 0
    for(a in 1:l){
      if(any(new_clustering[1] == storing_list[[a]]) | any(new_clustering[2] == storing_list[[a]]) ){ 
        storing_list[[a]] = union(storing_list[[a]], new_clustering)
        if(a>b){
          storing_list[[a]] = union(storing_list[[a]],storing_list[[b]])
          storing_list = storing_list[-b]
          break
        }
        b = a # stores the first union in b
        counter = counter +1
      }
    }
    if(counter == 0){storing_list = append(storing_list,list(unique(c(new_clustering))))}
  }
    if(l==0){storing_list = list(unique(c(new_clustering)))}
    return(storing_list)
} #works


#neighbour matrix changer
neigh_change = function(new_clustering, mat){ #lower triangular matrix and clustering objects
  vec = new_clustering #vectorize
  temp_mat = mat[,vec]
  temp_mat[,2] = ifelse(temp_mat[,1]==1|temp_mat[,2]==1, 1, 0)
  new_mat = mat
  new_mat[,vec[2]] = c(temp_mat[,2])
  new_mat[vec[2],] = c(temp_mat[,2]) # new row
  new_mat[vec[2],vec[2]] = c(0)
  new_mat[,vec[1]] = c(0)
  new_mat[vec[1],] = c(0)
  return(new_mat)
  } # works



# dissimilatiry matrix change

dis_change = function(new_clustering, mat, method, clusters){ #lower triangular matrix and clustering objects
  vec = new_clustering #vectorize
  temp_mat = mat[,vec]
  
  if(method == "min"){
    temp_mat[,2] = ifelse(temp_mat[,2] > temp_mat[,1] & temp_mat[,1] != 0,
                        temp_mat[,1],
                        temp_mat[,2])
  }
  if(method %in% c("max","complete")){
    temp_mat[,2] = ifelse(temp_mat[,2] > temp_mat[,1],
                          temp_mat[,2],
                          temp_mat[,1])
  }
  if(method == "mean"){
    length_clu = 2
    for (i in clusters) {
      if(vec[2] %in% i) length_clu = length(i) 
    }
    temp_mat[,2] = temp_mat[,2]*(length_clu-1)
    temp_mat[,2] = rowSums(temp_mat)/length_clu
  }
  new_mat = mat
  new_mat[,vec[2]] = c(temp_mat[,2])
  new_mat[vec[2],] = c(temp_mat[,2]) # new row
  new_mat[vec[2],vec[2]] = c(0)
  new_mat[,vec[1]] = c(0)
  new_mat[vec[1],] = c(0)
  return(new_mat)
}




#### learning function
clustering_linkage = function(dissimilar, neighbour, min_cluster, method, verbose=FALSE){
  storage_list = list() #empty list to return
  k = nrow(dissimilar)
  low_mat = lower.tri(dissimilar)
  while (k > min_cluster ) {
 
    # dissimilarity matrix with spatial restriction
    dis_neigh = dissimilar*neighbour
    # find minimum link (non-zero)
    new_cluster_obj = which_min_nonzero(dis_neigh)
    # merge the clusters with the min link
    storage_list = cluster_storing(new_cluster_obj,storage_list)
    # update neighbour matrix
    neighbour = neigh_change(new_cluster_obj,neighbour)
    # update dissimilarity matrix
    dissimilar = dis_change(new_cluster_obj,
                            dissimilar, 
                            method = method,
                            clusters = storage_list)
    
    k = k-1 #counter
    
    if(verbose) {print(k)}
  }
  return(storage_list)
}





## coloring of the layers and also works for the clusters
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








