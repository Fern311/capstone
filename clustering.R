install.packages("factoextra")
install.packages("NbClust")
merge_pop1 <- read.csv("cluster.csv")
library(data.table)
setDT(merge_pop1)
merge_pop1 <- merge_pop1[, "X":= NULL]
dim(merge_pop1)
# k-means clustering:
library(factoextra)
library(NbClust)
set.seed(123)
km.res <- kmeans(merge_pop1, 3, nstart = 25)
km.res$cluster
fviz_cluster(km.res, data = merge_pop1 , geom = "point",
             stand = FALSE, frame.type = "norm")

# elbow method to determine k
set.seed(123)
k.max <- 15 # Maximal number of clusters
data <- merge_pop1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 3, lty =2)

# silhouette to determine k:
install.packages("cluster")
library(cluster)
k.max <- 15
data <- merge_pop1
sil <- rep(0, k.max)
# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}
# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)