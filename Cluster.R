## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------------------
data <- read.csv("data.csv", header = TRUE)
# scale the data so that we can do clustering under better distence
scaled_data <- scale(data)
library("factoextra")


## ----------------------------------------------------------------------------------------------------------------
dist_euclidean <- dist(scaled_data, method = "euclidean")
dist_manhattan <- dist(scaled_data, method = "manhattan")


## ----------------------------------------------------------------------------------------------------------------
# Complete linkage
hc_complete_e <- hclust(dist_euclidean, method = "complete")
# Average linkage
hc_average_e <- hclust(dist_euclidean, method = "average")
# Single linkage
hc_single_e <- hclust(dist_euclidean, method = "single")
# Ward's method (using ward.D2 for better behavior)
hc_ward_e <- hclust(dist_euclidean, method = "ward.D2")


## ----------------------------------------------------------------------------------------------------------------
plot(hc_complete_e, main = "Dendrogram - Complete Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_average_e, main = "Dendrogram - Average Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_single_e, main = "Dendrogram - Single Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_ward_e, main = "Dendrogram - Ward's Method", xlab = "", sub = "", cex = 0.6)


## ----------------------------------------------------------------------------------------------------------------
# Complete linkage
hc_complete_man <- hclust(dist_manhattan, method = "complete")
# Average linkage
hc_average_man <- hclust(dist_manhattan, method = "average")
# Single linkage
hc_single_man <- hclust(dist_manhattan, method = "single")
# Ward's method (using ward.D2 for better behavior)
hc_ward_man <- hclust(dist_manhattan, method = "ward.D2")


## ----------------------------------------------------------------------------------------------------------------
plot(hc_complete_man, main = "Dendrogram - Complete Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_average_man, main = "Dendrogram - Average Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_single_man, main = "Dendrogram - Single Linkage", xlab = "", sub = "", cex = 0.6)
plot(hc_ward_man, main = "Dendrogram - Ward's Method", xlab = "", sub = "", cex = 0.6)


## ----------------------------------------------------------------------------------------------------------------
clustering_models <- list(
  "Complete-Euclidean" = hc_complete_e,
  "Average-Euclidean"  = hc_average_e,
  "Single-Euclidean"   = hc_single_e,
  "Ward-Euclidean"     = hc_ward_e,
  "Complete-Manhattan" = hc_complete_man,
  "Average-Manhattan"  = hc_average_man,
  "Single-Manhattan"   = hc_single_man,
  "Ward-Manhattan"     = hc_ward_man
)


optimal_k <- 3


## ----------------------------------------------------------------------------------------------------------------
for(model_name in names(clustering_models)){
  hc_model <- clustering_models[[model_name]]
  cat("Model now is:", model_name, "\n")
   plot(hc_model, main = paste("Dendrogram -", model_name), xlab = "", sub = "")
   
  wss_values <- sapply(2:10, function(k) compute_wss(k, hc_model, scaled_data))
  plot(2:10, log(wss_values), type = "b", 
       xlab = "number of cluster", 
       ylab = "Log(WSS)", 
       main = paste("Log-Elbow -", model_name))
  abline(v = optimal_k, col = "red", lty = 2)
}


## ----------------------------------------------------------------------------------------------------------------
for(model_name in names(clustering_models)){
  hc_model <- clustering_models[[model_name]]
  cat("Model now is:", model_name, "\n")
  clusters <- cutree(hc_model, k = optimal_k)
  p <- fviz_cluster(list(data = scaled_data, cluster = clusters), 
                    geom = "point", 
                    ellipse.type = "convex", 
                    main = paste("Cluster Visualization -", model_name))
  print(p)
}


## ----------------------------------------------------------------------------------------------------------------
cluster_centers_list <- list()
for(model_name in names(clustering_models)){
  hc_model <- clustering_models[[model_name]]
  clusters <- cutree(hc_model, k = optimal_k)
  centers <- aggregate(data, by = list(Cluster = clusters), FUN = mean)
  cluster_centers_list[[model_name]] <- centers
  cat("The center of Model", model_name, "\n")
  print(centers)
}


## ----------------------------------------------------------------------------------------------------------------
optimal_k <- 3

model_names <- names(clustering_models)
n_models <- length(model_names)

for(i in 1:(n_models - 1)){
  for(j in (i + 1):n_models){
    model1_name <- model_names[i]
    model2_name <- model_names[j]
    
    clusters1 <- cutree(clustering_models[[model1_name]], k = optimal_k)
    clusters2 <- cutree(clustering_models[[model2_name]], k = optimal_k)
    
    contingency_table <- table(Cluster_Model1 = clusters1, Cluster_Model2 = clusters2)
    
    cat("-----\n")
    cat("Compare:", model1_name, "and", model2_name, "\n")
    print(contingency_table)
    cat("\n")
  }
}

