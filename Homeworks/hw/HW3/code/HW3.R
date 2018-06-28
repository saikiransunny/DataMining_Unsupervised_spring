rm(list = ls(all = T))
workingdir = "D:/Spring 2018/STA/Homeworks/hw/HW3/code"
setwd(workingdir)


# problem - 1
#####
load("../SwissBankNotes1.rdata")
dataset = SwissBankNotes; rm(SwissBankNotes)
dir.create(file.path(workingdir, "../plots"), showWarnings = FALSE)
dir.create(file.path(workingdir, "../plots/prob1"), showWarnings = FALSE)


# 0 is genuine and 1 is counterfiet. 
dataset$target = 0
dataset$target[101:200] = 1
table(dataset$target)


#perform PCA. 
pca_object = prcomp(dataset[,-c(7)])
pca_importance = data.frame(t(data.frame(unclass(summary(pca_object))$importance)[2:3,]))
pca_importance$Principal_Comps = row.names(pca_importance)
names(pca_importance) = c("Variance", "Cumulative_Variance", "Component")
library(ggplot2)
p = ggplot(pca_importance, aes(x = Component, y = Cumulative_Variance, group = 1))+ 
  geom_line()
p = p + geom_point() + ggtitle("Principal Component Variation with complete data") + theme(plot.title = element_text(hjust = 0.5))
p
ggsave(filename = "../plots/prob1/PCA_data.pdf", plot = p)


#to what feature is it more correlated to?
cat("Feature", "\t", "Correlation")
feature1 = pca_object$x[,1]
for(i in 1:ncol(dataset)-1){
  cat(names(dataset)[i], ": ", cor(feature1, dataset[,i]), "\n")
}


# pca on only class - 0
pca_object_0 = prcomp(dataset[1:100,-c(7)])
pca_importance_0 = data.frame(t(data.frame(unclass(summary(pca_object_0))$importance)[2:3,]))
pca_importance_0$Principal_Comps = row.names(pca_importance_0)
names(pca_importance_0) = c("Variance", "Cumulative_Variance", "Component")
library(ggplot2)
p = ggplot(pca_importance_0, aes(x = Component, y = Cumulative_Variance, group = 1))+ 
  geom_line()
p = p + geom_point() + ggtitle("Principal Component Variation within class-0") + theme(plot.title = element_text(hjust = 0.5))
p
ggsave(filename = "../plots/prob1/PCA_class0.pdf", plot = p)
#showing the feature correspondence. 
library(ggfortify)
autoplot(prcomp(dataset[1:100,-c(7)]), data = pca_object_0, 
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



# pca on only class - 1
pca_object_1 = prcomp(dataset[101:200,-c(7)])
pca_importance_1 = data.frame(t(data.frame(unclass(summary(pca_object_1))$importance)[2:3,]))
pca_importance_1$Principal_Comps = row.names(pca_importance_1)
names(pca_importance_1) = c("Variance", "Cumulative_Variance", "Component")
library(ggplot2)
p = ggplot(pca_importance_1, aes(x = Component, y = Cumulative_Variance, group = 1))+ 
  geom_line()
p = p + geom_point() + ggtitle("Principal Component Variation within class-1") + theme(plot.title = element_text(hjust = 0.5))
p
ggsave(filename = "../plots/prob1/PCA_class1.pdf", plot = p)
#showing the feature correspondence. 
autoplot(prcomp(dataset[101:200,-c(7)]), data = pca_object_1,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


# plotting 2 components of all data, class - 0 and class -1
plot_data = data.frame(pca_object$x)
plot_data$target = dataset$target
plot_data$target = as.factor(as.character(plot_data$target))
plot = ggplot(plot_data, aes(PC1, PC2,color = target)) + geom_point()
plot
ggsave(filename = "../plots/prob1/compare.pdf", plot = plot)

#showing the feature correspondence. 
autoplot(prcomp(dataset[,-c(7)]), data = plot_data, colour = 'target',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)



for(i in 1:6){
  for(j in 1:6){
    if(i == j){
      next
    }
    filename = paste("PCA_", i,"_",j, sep = "")
    plot_data = data.frame(pca_object$x[,c(i,j)])
    names(plot_data) = c("PC_X", "PC_Y")
    
    plot_data$target = 0
    plot_data$target[101:200] = 1 
    plot_data$target = as.factor(as.character(plot_data$target))
    
    plot = ggplot(plot_data, aes(PC_X, PC_Y,color = target)) + geom_point() + 
      ggtitle(paste("Class-0 vs Class-1. ","PC-",i, " vs ","PC-",j))
    print(plot)
    ggsave(filename = paste("../plots/prob1/", filename,".pdf", sep = ""), plot = plot)
  }
}
######



#problem - 2
#####
rm(list = ls(all = T))
load("../primate.scpulae.rdata")

dataset = primate.scapulae
rm(primate.scapulae)
sum(is.na(dataset))
dataset$gamma[which(is.na(dataset$gamma))] = median(dataset$gamma, na.rm = T)

#hierarchial clustering.
library(Metrics)
cluster_data = scale(dataset[,1:9])

#complete linkage
hc.complete = hclust(dist(cluster_data), method = "complete")
plot(hc.complete)
unique(dataset$classdigit)
clusters_complete = cutree(hc.complete, 5)
table(dataset$classdigit, clusters_complete)
#interchanging the cluster assignment of 4 and 5. 
clusters_complete[which(clusters_complete == 5)] = 999
clusters_complete[which(clusters_complete == 4)] = 5
clusters_complete[which(clusters_complete == 999)] = 4
table(dataset$classdigit, clusters_complete)
# misclassification rate of complete linkage.
(1 - accuracy(actual = dataset$classdigit, clusters_complete)) * 100
#17.14

#single linkage
hc.single = hclust(dist(cluster_data), method = "single")
plot(hc.single)
clusters_single = cutree(hc.single, 5)
table(dataset$classdigit, clusters_single)
# interchanging the cluster assignmnet of 3,4
clusters_single[which(clusters_single == 3)] = 999
clusters_single[which(clusters_single == 4)] = 3
clusters_single[which(clusters_single == 999)] = 4
table(dataset$classdigit, clusters_single)
# misclassification rate of single linkage.
(1 - accuracy(actual = dataset$classdigit, clusters_single)) * 100
#14.28


#average linkage
hc.average = hclust(dist(cluster_data), method = "average")
plot(hc.average)
clusters_average = cutree(hc.average, 5)
table(dataset$classdigit, clusters_average)
#interchanging the cluster assignment of 4 and 5. 
clusters_average[which(clusters_average == 5)] = 999
clusters_average[which(clusters_average == 4)] = 5
clusters_average[which(clusters_average == 999)] = 4
table(dataset$classdigit, clusters_average)
# misclassification rate of average linkage.
(1 - accuracy(actual = dataset$classdigit, clusters_average)) * 100
#17.14


#determining if the data has any outliers. 
workingdir = "D:/Spring 2018/STA/Homeworks/hw/HW3/code"
dir.create(file.path(workingdir, "../plots/prob2"), showWarnings = FALSE)
pca_obj = prcomp(dataset[,-c(10,11)])
for(i in 1:9){
  for(j in 1:9){
    if(i == j){
      next
    }
    filename = paste("PCA_", i,"_",j, sep = "")
    plot_data = data.frame(pca_obj$x[,i])
    plot_data = data.frame(cbind(plot_data, pca_obj$x[,j]))
    names(plot_data) = c("PC_X", "PC_Y")

    
    plot = ggplot(plot_data, aes(PC_X, PC_Y)) + geom_point() + 
      ggtitle(paste("Class-0 vs Class-1. ","PC-",i, " vs ","PC-",j))
    print(plot)
    ggsave(filename = paste("../plots/prob2/", filename,".pdf", sep = ""), plot = plot)
  }
}
#there are outliers in the data. Hence opting k-mediods since it is more robust. 
library(kmed)
distance_computation = distNumeric(as.matrix(cluster_data),as.matrix(cluster_data), method = "mrw")
result = fastkmed(distance_computation, ncluster = 5, iterate = 10)
cluster_result = result$cluster
table(dataset$classdigit, cluster_result)
#there are a bunch of class rearrangements to be done. 
#interchange 3,5
cluster_result[which(cluster_result == 5)] = 999
cluster_result[which(cluster_result == 3)] = 5
cluster_result[which(cluster_result == 999)] = 3
table(dataset$classdigit, cluster_result)

#interchange 2,3
cluster_result[which(cluster_result == 2)] = 999
cluster_result[which(cluster_result == 3)] = 2
cluster_result[which(cluster_result == 999)] = 3
table(dataset$classdigit, cluster_result)

#interchange 1,3
cluster_result[which(cluster_result == 3)] = 999
cluster_result[which(cluster_result == 1)] = 3
cluster_result[which(cluster_result == 999)] = 1
table(dataset$classdigit, cluster_result)
(1 - accuracy(actual = dataset$classdigit, cluster_result)) * 100
#47.61
#####


#problem - 3
#####
#implementing the kmeans on 2,5,10,20
rm(list = ls(all = T))
workingdir = "D:/Spring 2018/STA/Homeworks/hw/HW3/code"
setwd(workingdir)

dir.create(file.path(workingdir, "../plots/prob3"), showWarnings = FALSE)
library(kohonen)
library(ElemStatLearn)
data(nci)
dataset = data.frame(nci); rm(nci)
cluster_data = scale(dataset)
set.seed(1234)
for(i in c(2,5,10,20)){
  cat(paste("k: ", i, "\n",sep = ""))
  kmeans_clusterobject = kmeans(cluster_data, i)
  print("KMeans:")
  print(sort(table(kmeans_clusterobject$cluster)))

  #for different radius.
  
  for(j in c(0.1,1,10,50,100,500,1000)){
  som_object = som(as.matrix(cluster_data), grid = somgrid(i,1,"hexagonal"), keep.data = T)
  cat("Self Organising Map.", "Radius: ", j)
  print(sort(table(som_object$unit.classif)))
  }
  cat("\n\n\n\n")
}
