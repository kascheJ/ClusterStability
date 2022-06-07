
##### simulated data


# choosing lambdas

lambda_mat = matrix(0.5,1,12)

lambda_mat = lambda_mat %>% 
  rbind(.,round(seq(0.1,0.9, length.out = 12),digits= 2)) %>% 
  rbind(.,seq(0.3,by=0.05, length.out = 12)) %>% 
  rbind(.,seq(0.45,by=0.01, length.out = 12))
lambda_mat
xtable(lambda_mat)



# which objects belong to the same municipality?
municip = factor(orebro_districts$KOM, labels = 1:12)

# simulate data
set.seed(960424)
#random term
epsilon_mat = matrix(runif(185*200,-0.1,0.1),185,200)

#loop for 200 different allocations of clusters
Y1 = matrix(0,185,200)
Y2 = matrix(0,185,200)
Y3 = matrix(0,185,200)
Y4 = matrix(0,185,200)
for (i in 1:200) {
  lambda_mat_mix = lambda_mat[,sample(1:12,12)]
  Y1[,i] = lambda_mat_mix[1,municip] + epsilon_mat[,i]
  Y2[,i] = lambda_mat_mix[2,municip] + epsilon_mat[,i]
  Y3[,i] = lambda_mat_mix[3,municip] + epsilon_mat[,i]
  Y4[,i] = lambda_mat_mix[4,municip] + epsilon_mat[,i]
  
}



# need mode function for summaries
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



### simulation results for Complete Linkage

sim_function = function(data,method, test_method){

rot_results = c()
for (i in 1:50) {
  sim_data =as.matrix(data[,i])
  diff_sim = create_diff(sim_data,1)
  
  clu_sim = clustering_linkage(diff_sim,
                               neighbour_mat,
                               12,
                               method)
  
  
  rot_sim = cluster_test(clusters = clu_sim,
                                 data = sim_data,
                                 number_perm = "all",
                                 neighbours_list = neighbours,
                                 min_cluster = 12,
                                 method = method,
                                 seed = 960424,
                                 test_method = test_method)




  for( obj in rot_sim){
    if(length(obj)>1){
      
      cluster = obj[[2]][,1]
      size = length(cluster)
      municip_belong = municip[cluster]
      
      mean_lost = obj[[2]] %>% 
        as_tibble() %>% 
        cbind(municip[cluster]) %>% 
        group_by(getmode(municip[cluster]) == municip[cluster]) %>% 
        summarise(mean_CLP = mean(spatial)) #changed here
      mean_lost = as.matrix(mean_lost[,2])
      if(length(mean_lost)==1) mean_lost = c(mean_lost,NA)
     
      CIR = obj[[1]]
      mean_CLP = mean(obj[[3]])
      res = c(i,
              size,
              CIR,
              mean_CLP,
              mean_lost )
    rot_results = rbind(rot_results,res)  
    }
    print("hej")
  }
  print(c(i,"############################"))
}
return(rot_results)
}

# save in new object with better name

# rot_results_Y1_max = rot_results
# rot_results_Y2_max = rot_results
# rot_results_Y3_max = rot_results_Y3
# rot_results_Y4_max = rot_results[rot_results[,1]<51,]
# View(rot_results_Y4_max)
View(rot_results_Y2_max)

# rot_results_Y1_min = rot_results
# rot_results_Y2_min = rot_results
# rot_results_Y3_min = rot_results
# rot_results_Y4_min = rot_results

# rot_results_Y1_average = rot_results
# rot_results_Y2_average = rot_results
# rot_results_Y3_average = rot_results
# rot_results_Y4_average = rot_results

#new
rot_results_Y1_min = sim_function(Y1,"min","rotation")
print("new")
rot_results_Y2_min = sim_function(Y2,"min","rotation")
print("new")
rot_results_Y3_min = sim_function(Y3,"min","rotation")
print("new")
rot_results_Y4_min = sim_function(Y4,"min","rotation")
print("new")

rot_results_Y1_max = sim_function(Y1,"max","rotation")
print("new")
rot_results_Y2_max = sim_function(Y2,"max","rotation")
print("new")
rot_results_Y3_max = sim_function(Y3,"max","rotation")
print("new")
rot_results_Y4_max = sim_function(Y4,"max","rotation")

rot_results_Y1_average = sim_function(Y1,"average","rotation")
print("new")
rot_results_Y2_average = sim_function(Y2,"average","rotation")
print("new")
rot_results_Y3_average = sim_function(Y3,"average","rotation")
print("new")
rot_results_Y4_average = sim_function(Y4,"average","rotation")






rand_results_Y1_max = sim_function(Y1,"max","random")
rand_results_Y2_max = sim_function(Y2,"max","random")
rand_results_Y3_max = sim_function(Y3,"max","random")
rand_results_Y4_max = sim_function(Y4,"max","random")




View(rot_results_Y4_max)


