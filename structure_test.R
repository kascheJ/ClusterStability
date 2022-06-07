
#### comparison and test of structures

# tests both correlation and distribution with kolmogorov-smirnov

structure_test = function(test_list1,# list with all that is needed to test
                          test_list2, # list with all that is needed to test
                          random_rot = c(), # vec indicator randomization or rotation, with length = 2
                          seed = "time"){ 
  if(random_rot[1]== "rotation"){
  test1 =     rotation_test(cluster = test_list1[[1]],
                           layer = test_list1[[2]],
                           data = test_list1[[3]],
                           neighbours_list = test_list1[[4]],
                           min_cluster = test_list1[[5]],
                           method = test_list1[[6]],
                           seed)
  } else{
  test1 =  random_test(cluster=test_list1[[1]],
                layer = test_list1[[2]] ,           
                number_perm = test_list1[[3]],
                data=test_list1[[4]],
                neighbour_mat = test_list1[[5]],
                min_cluster = test_list1[[6]],
                method = test_list1[[7]],
                seed)
  }
  if(random_rot[2]== "rotation"){
    test2 =     rotation_test(cluster = test_list2[[1]],
                             layer = test_list2[[2]],
                             data = test_list2[[3]],
                             neighbours_list = test_list2[[4]],
                             min_cluster = test_list2[[5]],
                             method = test_list2[[6]],
                             seed)
  }else{
   test2 =  random_test(cluster=test_list2[[1]],
                layer = test_list2[[2]] ,           
                number_perm = test_list2[[3]],
                data=test_list2[[4]],
                neighbour_mat = test_list2[[5]],
                min_cluster = test_list2[[6]],
                method = test_list2[[7]],
                seed)
  }
  
  lost_p1 = test1[[2]]
  lost_p2 = test2[[2]]
  
  lost_merged = merge(lost_p1,lost_p2, by = 1)
  
  correlations = list("covariates" = list(cor.test(lost_merged[,2],lost_merged[,4])),
                      "Spatial" = list(cor.test(lost_merged[,3],lost_merged[,5])))
  
  distrib_test = ks.test(test1[[3]], test2[[3]])
  
  print(correlations[[1]])
  print(correlations[[2]])
  return(list("correlations" = correlations,
              "lost_distribution" = distrib_test) )
}



# test all clusters at a time

cluster_test = function(clusters,
                        data,
                        number_perm = "all",
                        neighbours_list,
                        min_cluster,
                        method,
                        seed = "time",
                        test_method){
  
  
  # To store results
  return_res = list()
  
  # create neighbour-mat
  neighbour_mat = nb_list2mat(neighbours_list)
  
  # create empty layers object
  layers = c()
  
  for (clu in clusters) {
    # layers, if requested
    if(test_method %in% c("random_border","rotation", "rotation_border")){
    layers = layer_creator(clu, neighbour_mat)
    }
    
    # fix number of perms if set is to large
    if(test_method %in% c("random","random_border")){
    n_perms = "all"
    if(test_method== "random") n_objs = clu else n_objs = layers
    if(length(n_objs)>4) n_perms = 24
    test_res = random_test(cluster = clu,
                           layer = layers,
                           data = data,
                           number_perm = n_perms,
                           neighbour_mat = neighbours_mat,
                           min_cluster = 12,
                           method = method,
                           seed= seed)
    } #random end
    
    #rotation start
    if(test_method %in% c("rotation", "rotation_border")){
      
      test_res = rotation_test(cluster = clu,
                               layers,
                               data,
                               neighbours_list,
                               min_cluster,
                               method,
                               seed,
                               test_method = test_method)
    } #rotation end
    
    #append for each clusters result 
    return_res = append(return_res, list(test_res))
    
    
    
    
    
  } #cluster loop end
  
  return(return_res)
} #function end







