### randomization

#function to randomize dissimiliarty-matrix
random_diff_matrix = function(cluster, permutation, data){
    temp_data = as.matrix(data)
    temp_data[cluster,] = data[permutation,]
    n = nrow(data)
    diff_matrix = matrix(0,n,n)
    for(i in 1:n){
      diff_matrix[i,] = rowSums(abs(t(t(temp_data)-c(temp_data[i,]))))
    }
    diff_matrix[upper.tri(diff_matrix)]= t(diff_matrix)[upper.tri(diff_matrix)]
    return(diff_matrix)
}


### function for evaluation of re-clustering
clu.eval = function(clu_random, cluster){
   seter = 1 #counter
   result = rep(FALSE,length(clu_random))
   lost_polygons = c()
   # Check if any polygons have been re-placed
   for(obj in clu_random){
      result[seter] = (sum(cluster %in% obj)==length(cluster))
      if(sum(cluster %in% obj)>0 & sum(cluster %in% obj)<length(cluster) ){
         lost_polygons[seter] = (sum(cluster %in% obj)) 
      }
      seter = seter + 1
   }
   max = which.max(lost_polygons) # the cluster that is the "main" cluster
   lost_polygons[max] = 0 # the main cluster does not contain any lost objects
   wrong_clu = which(lost_polygons >0) # which index have been placed in new cluster
   
   # loop to find which polygons that have been lost
   lost_polygons_vec = c()
   for(i in wrong_clu){
      lost_polygons_vec = append( lost_polygons_vec, 
                                  cluster[cluster %in% clu_random[[i]]])
   }
return(list(lost_polygons_vec, result))}

perm.fixer = function(cluster, 
                      layer = c(), 
                      number_perm, 
                      rotation_list,
                      seed = "time"){
   library(combinat) #necassary package
   if(seed != "time") set.seed(seed) #set seed if given
   
   #random border or cluster
   if(length(layer)>0) obj = layer else obj = cluster
   
   # when cluster size is small
   if(number_perm == "all") permutations = permn(obj)
   if(length(rotation_list)>0){ 
      permutations = rotation_list
      obj = rotation_list[[1]]
   }
   #when cluster size is large
   if(is.numeric(number_perm)){ 
      permutations = list()
      for (i in 1:number_perm) {
         permutations = append(permutations, list(sample(obj, length(obj))))
      }
   }
   if(length(layer)>0 | length(rotation_list)>0 ){
      n = length(permutations)
      for(i in 1:n){
         temp_perm = cluster
         temp_perm[cluster %in% obj] = permutations[[i]]
         permutations[[i]] = temp_perm
      }
   }
   return(permutations)
}


### function to test randomness-condition in cluster
random_test = function(cluster, # pre defined cluster
                       layer = c(), #fill with a layer you want to randomize otherwise the whole cluster is randomized
                       number_perm = "all", # fill with an integer if you want to use a sampled subset of the permutations
                       rotation_list = list(), # fill with manual permutations
                       data, # underlying covariates
                       neighbour_mat, # polygon neighbour matrix
                       min_cluster, # minimum cluster-size used when created "cluster"
                       method = "max",
                       seed = "time"){
   
  #create permutations
   permutations = perm.fixer(cluster, layer, number_perm, rotation_list, seed)
   
  # loop through all permutations/rotations
  counter = 0
  final_result = rep(FALSE,length(permutations))
  lost = matrix(c(cluster, rep(0,length(cluster)*2)),length(cluster),3)
  ratio_lost = rep(0,length(permutations))
  for(perm in permutations){
   # new diff_matrix; changed by permutation/rotation
   diff_matrix = random_diff_matrix(cluster = cluster,
                       permutation = perm,
                       data = as.matrix(data))
   #re-clustering with above diff_matrix
   clu_random = clustering_linkage(dissimilar = diff_matrix,
                                   neighbour = neighbour_mat,
                                   min_cluster = min_cluster,
                                   method = method)
  
   
   ### evaluation of re-clustering
   eval_list = clu.eval(clu_random, cluster)
   lost_polygons_vec = eval_list[[1]]
   result = eval_list[[2]]
   
   ### results 
   {
   #inverse of permutation; how to evaluate
   transfor = cbind(cluster,perm)
   lost_polygons_spat = transfor[transfor[,2] %in% lost_polygons_vec, 1 ]
   
   # store lost polygons in matrix, to return later
   lost[lost[,1] %in% lost_polygons_vec, 2] = lost[lost[,1] %in% lost_polygons_vec, 2] +1
   lost[lost[,1] %in% lost_polygons_spat, 3] = lost[lost[,1] %in% lost_polygons_spat, 3] +1
   
   
   # store ratio of lost/total polygons
   counter = counter + 1
   ratio_lost[counter] = length(lost_polygons_vec)/length(cluster)
   
   # store result of any polygon in specified cluster have been replaced
   #print(any(result)) #any replaced?
   print(counter)
   final_result[counter] = any(result) }
  }
  #convert to percent
  n = length(permutations)
  lost[,-1] = lost[,-1]/n

  colnames(lost) = c("object", "covariates", "spatial")
  #hist(ratio_lost)
  return(list("cluster intact ratio"= mean(final_result),
              "lost polygons matrix" = lost,
              "Ratio lost polygons" = ratio_lost)) # gives back the eval. measures
} # method used when created "cluster"



