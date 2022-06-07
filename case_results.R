#### case result



#import data


setwd("C:/Users/asus/OneDrive/Skrivbord/StatistikMaster/MThesis/R")
library(readr)
library(readxl)
library(tidyverse)

## election data
election18 = read_xlsx("tab5_riksdag.xlsx")
colnames(election18)


str(election18)

# spatial data
library(sp)
library(shapefiles)
library(rgdal)


districts = readOGR(dsn= paste0(getwd(),"/2018_valgeografi_valdistrikt"),layer =  "alla_valdistrikt")

str(districts)
election18 = election18 %>% filter(Valdistriktsnamn != "Förtidsröster ej fördelade efter valdistrikt") #remove pre-votes


election18_percent = cbind(election18[1:10],
                           election18[12:19]/rowSums(election18[12:19])) 




### Örebro-case

# election result data
orebro_elect = election18 %>%
  filter(Valkretsnamn == "Örebro län") %>% 
  filter(Valdistriktsnamn != "Förtidsröster ej fördelade efter valdistrikt") #remove pre-votes

orebro_elect_percent = cbind(orebro_elect[1:10],
                             orebro_elect[12:19]/rowSums(orebro_elect[12:19])) 

# raw data in matrix form
orebro_data = orebro_elect_percent[,11:18]
orebro_data = as.matrix(orebro_data)

head(orebro_elect_percent)

# take out Örebro in the spatial data
unique(orebro_elect$Valkretskod) # valkretskod = "22"

orebro_districts = districts[districts@data$RVK == "22", ]


plot(orebro_districts)


### loading neccassary functions
source("single_linkage.R" )
source("layer_functions.R")
source("randomness_test.R")
source("order.R")
source("rotation.R")
source("structure_test.R")



# getting the neighbours of orebro
library(spdep)
neighbours = poly2nb(orebro_districts, queen=F) #queen = F: is that they need to share more than one vertex


# converting list to matrix for neighbours
neighbours_mat = nb_list2mat(neighbours)


# create dissimilarity matrix
diff_matrix = create_diff(orebro_elect_percent)


### run clustering with Orebro-data

# Single-linkage
clu_single =clustering_linkage(dissimilar = diff_matrix,
                        neighbour = neighbours_mat,
                        min_cluster = 12,
                        method = "min" )

layer_plot(clu_single,orebro_districts) # plot clustering
+title("Single-linkage with 12 clusters")

# complete-linkage
clu_max =clustering_linkage(dissimilar = diff_matrix,
                         neighbour = neighbours_mat,
                         min_cluster = 12,
                         method = "complete")

layer_plot(clu_max,orebro_districts)
+title("Complete-linkage with 12 clusters")

# average-linkage
clu_mean =clustering_linkage(dissimilar = diff_matrix,
                         neighbour = neighbours_mat,
                         min_cluster = 12,
                         method = "mean")

layer_plot(clu_mean,orebro_districts)
+title("Average-linkage with 12 clusters")

## plot municipalites of Örebro
plot(orebro_districts, col = orebro_districts@data$KOM)
+title("Municipalities in 'Örebro Län' ")

clusterings = list(clu_single,clu_max, clu_mean)

# make a plot of all 3 clusterings 
par(mfrow = c(1,3))
for( clu in clusterings){
  layer_plot(clu,orebro_districts)
}




# cluster 7 in clu_max; crosses the municip borders?

cluster7 = clu_max[[7]]

table_muncip = summary(as.factor(orebro_districts$NAMN_KOM[cluster7]))
xtable(t(as.data.frame(table_muncip))   )     



## calculate the CLP for all clusters in all methods, may not be used

{


par(mfrow=c(1,1))

rand_case_res = list() #result list
counter = 0 #to choose method
methods = c("min","max","mean")

for(obj in clusterings ){
  
  #choosing right method for right list-object
  counter = counter +1
  method = methods[counter]
  rand_case_res = append(rand_case_res, method)
  for(clu in obj){
    n_perms = "all"
    if(length(clu)>2) n_perms = 200
    res =random_test(clu,
                data = orebro_data,
                number_perm = n_perms,
                neighbour_mat = neighbours_mat,
                min_cluster = 12,
                method=method,
                seed = 960424)
    rand_case_res[[counter]] = append(rand_case_res[[counter]], list(res))
    
  }
}
View(rand_case_res)

## table for latex
CIR = matrix("*", 3,8)
for(i in 1:length(rand_case_res)){
  for(k in 2:length(rand_case_res[[i]])){
  CIR[i,k-1] = rand_case_res[[i]][[k]][["cluster intact ratio"]]
    }
  }

rownames(CIR) = c("Single","Complete", "Average")

library(xtable)
xtable(CIR)

}


### boxplots
box_mat = matrix(NA, 200, length(rand_case_res[[2]])-1)
for(k in 2:length(rand_case_res[[2]])){
  vec = rand_case_res[[2]][[k]][["Ratio lost polygons"]]
  len = length(vec)
  box_mat[1:len,k-1] = vec
}
colnames(box_mat) = sapply(clu_max, length)



### only in clu_max

# test for randomness
res_list = cluster_test(clusters = clu_max,
                               data = orebro_data,
                               number_perm = "all",
                               neighbours_list = neighbours,
                               min_cluster = 12,
                               method = "max",
                               seed = 960424,
                               test_method = "random")


View(res_list)





# test for border randomness
border_res_list = cluster_test(clusters = clu_max,
                               data = orebro_data,
                               number_perm = "all",
                               neighbours_list = neighbours,
                               min_cluster = 12,
                               method = "max",
                               seed = 960424,
                               test_method = "random_border")


View(border_res_list)


# test for border rotation

border_rot_list = cluster_test(clusters = clu_max,
                               data = orebro_data,
                               number_perm = "all",
                               neighbours_list = neighbours,
                               min_cluster = 12,
                               method = "max",
                               seed = 960424,
                               test_method = "rotation_border")


View(border_rot_list)



# test for rotation

rot_list = cluster_test(clusters = clu_max,
                               data = orebro_data,
                               number_perm = "all",
                               neighbours_list = neighbours,
                               min_cluster = 12,
                               method = "max",
                               seed = 960424,
                               test_method = "rotation")


View(rot_list)



# focus on cluster 7 though it is the only cluster that works for all methods

clu7_results = list("random" = res_list[[7]],
                    "border_random" = border_res_list[[7]],
                    "rotation" = rot_list[[7]],
                    "border_rotation" = border_rot_list[[7]])
View(clu7_results)



# result tables for latex

table_summary = matrix(0,4,6)
table_distrib = matrix(0,4,4)
table_lost_districts = c()
table_lost_cor = matrix(0,4,4)

for (i in 1:4) {
  dat = clu7_results[[i]]
  ks_res = c()
  cor_res = c()
  for(obj in clu7_results){
    ks_res = c(ks_res,ks.test(dat[["Ratio lost polygons"]],
                              obj[["Ratio lost polygons"]])[["p.value"]])
    
    cor_test = cor.test(dat[["lost polygons matrix"]][,3],
                        obj[["lost polygons matrix"]][,3])
    cor_res = c(cor_res, paste(round(cor_test[["estimate"]],digits = 3),
                               "(",
                               round(cor_test[["p.value"]],digits = 3),
                               ")"))
  }
  table_lost_districts = cbind(table_lost_districts, 
                               dat[["lost polygons matrix"]][,3])
  table_distrib[,i] = ks_res
  table_summary[i,] = summary(dat[["Ratio lost polygons"]])
  table_lost_cor[,i] = cor_res
}

#names
rownames(table_summary) = names(clu7_results)
rownames(table_distrib) = names(clu7_results)
colnames(table_distrib) = rownames(table_distrib)
colnames(table_summary) = names(summary(1:4))
rownames(table_lost_cor) = names(clu7_results)
colnames(table_lost_cor) = rownames(table_lost_cor)
colnames(table_lost_districts) = names(clu7_results)
rownames(table_lost_districts) = clu7_results$random$`lost polygons matrix`[,1]



table_summary
table_distrib
table_lost_cor
table_lost_districts


xtable(table_summary)
xtable(table_distrib)
xtable(table_lost_cor)


xtable(t(round(table_lost_districts[1:11,], digits = 3)))
xtable((round(table_lost_districts[12:22,], digits = 3)))
xtable((round(table_lost_districts[23:33,], digits = 3)))
xtable((round(table_lost_districts[34:44,], digits = 3)))
xtable((round(table_lost_districts[45:53,], digits = 3)))

View(cor.test(rnorm(50), rnorm(50))
)
nrow(table_lost_districts)

# summary table for lost districts in different municipalities

table_lost_districts %>% 
  as_tibble() %>% 
  cbind(orebro_districts$NAMN_KOM[cluster7]) %>% 
  group_by(orebro_districts$NAMN_KOM[cluster7]) %>% 
  summarise(random = mean(random),
            Border_random = mean(border_random),
            rotation = mean(rotation),
            Border_rotation = mean(border_rotation)
            ) %>% 
  xtable(digits = 3)



# summary main muncip lost


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# random results
counter = 1
result_lost = matrix(NA,8, 2)
for(obj in res_list){
  dat = as.tibble(obj[["lost polygons matrix"]])
  cluster = as.matrix(dat[,1])
  dat = cbind(dat,orebro_districts$NAMN_KOM[cluster])
  colnames(dat)[4] = "muncip"
  mode_dat = getmode(dat[,4])
  res =dat %>% 
            group_by(muncip == mode_dat) %>% 
            summarise( mean = mean(spatial))
  res = res[,2]
  result_lost[counter,1:nrow(res)] = as.matrix(res)
          
  counter = counter +1
}

colnames(result_lost) = c("Main municipality", "Other municipalities")
xtable(t(result_lost)
)

# rotation results
counter = 1
result_lost = matrix(NA,8, 2)
for(obj in rot_list){
  dat = as.tibble(obj[["lost polygons matrix"]])
  cluster = as.matrix(dat[,1])
  dat = cbind(dat,orebro_districts$NAMN_KOM[cluster])
  colnames(dat)[4] = "muncip"
  mode_dat = getmode(dat[,4])
  res =dat %>% 
    group_by(muncip == mode_dat) %>% 
    summarise( mean = mean(spatial))
  res = res[,2]
  result_lost[counter,1:nrow(res)] = as.matrix(res)
  
  counter = counter +1
}

colnames(result_lost) = c("Main municipality", "Other municipalities")
xtable(t(result_lost)
)
View(rot_list)









