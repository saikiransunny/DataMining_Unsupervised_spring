rm(list = ls(all = T))
setwd("D:/Spring 2018/STA/Homeworks/hw/HW2/code")


data("USArrests")
dataset = USArrests; rm(USArrests)
head(dataset)
dataset = data.frame(dataset)
sum(is.na(dataset))

# a
set.seed(123)
hc.complete = hclust(dist(dataset), method = "complete")
plot(hc.complete)

#b
clusters = cutree(hc.complete, 3)
cities = rownames(dataset)
cluster_df = data.frame(cities, clusters)
#belonging to cluster1
cluster_df[which(cluster_df$clusters == 1),]$cities
#belonging to cluster2
cluster_df[which(cluster_df$clusters == 2),]$cities
#belonging to cluster 3
cluster_df[which(cluster_df$clusters == 3),]$cities

#c
sd.data = scale(USArrests)
hc.complete.sd = hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)


#d
newclusters = cutree(hc.complete.sd, 3)
new_cluster_df = data.frame(cities, newclusters)
#belonging to cluster1
new_cluster_df[which(new_cluster_df$newclusters == 1),]$cities
#belonging to cluster2
new_cluster_df[which(new_cluster_df$newclusters == 2),]$cities
#belonging to cluster 3
new_cluster_df[which(new_cluster_df$newclusters == 3),]$cities
table(clusters, newclusters)
rm(cluster_df, dataset, hc.complete, hc.complete.sd, new_cluster_df, sd.data, cities, clusters, newclusters)







#problem - 10
#a
set.seed(1234)
set1 = data.frame(matrix(rnorm(20  * 50, mean = 0, sd = .001), ncol = 50))
set1$target = 1
set2 = data.frame(matrix(rnorm(20  * 50, mean = 0.001, sd = .001), ncol = 50))
set2$target = 2
set3 = data.frame(matrix(rnorm(20  * 50, mean = 0.002, sd = .001), ncol = 50))
set3$target = 3
data = rbind(set3, rbind(set1, set2))

#b
pca = prcomp(data[,-c(51)])
library(ggplot2)
plot_data = data.frame(pca$x[,c(1,2)])
plot_data$target = data$target
p = ggplot(plot_data, aes(PC1, PC2))
p + geom_point(aes(colour = factor(target)))

#c
#performing kmeans clustering on the dataset. k - 3
kmeans_obj = kmeans(data[,-c(51)], 3, nstart = 20)
table(data$target, kmeans_obj$cluster)

#d
#performing kmeans clustering on the dataset. k - 2
kmeans_obj = kmeans(data[,-c(51)], 2, nstart = 20)
table(data$target, kmeans_obj$cluster)
           
#e
#performing kmeans clustering on dataset. k = 4
kmeans_obj = kmeans(data[,-c(51)], 4, nstart = 20)
table(data$target, kmeans_obj$cluster)

#f
#kmeans on the pca data with k = 3
kmeans_obj = kmeans(pca$x[, 1:2], 3, nstart = 20)
table(data$target, kmeans_obj$cluster)

#g
#kmeans on the original data with k = 3 with scaling. 
kmeans_obj = kmeans(scale(data[,-c(51)]), 3, nstart = 20)
table(data$target, kmeans_obj$cluster)






#prob - 11
genes = read.csv("../data/Ch10Ex11.csv", header = FALSE)
hc.complete = hclust(as.dist(1 - cor(genes)), method = "complete")
plot(hc.complete)


hc.single = hclust(as.dist(1 - cor(genes)), method = "single")
plot(hc.single)

hc.average = hclust(as.dist(1 - cor(genes)), method = "average")
plot(hc.average)

# taking a transpose of genes since we are interested in genes as features.
t_genes = t(genes)
pr.out = prcomp(t_genes)
head(pr.out$rotation)

total.load = apply(pr.out$rotation, 1, sum)
index = order(abs(total.load), decreasing = TRUE)
index[1:10]