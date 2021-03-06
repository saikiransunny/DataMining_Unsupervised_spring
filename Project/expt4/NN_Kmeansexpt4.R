rm(list = ls(all = T))
library(vegan)
library(ggfortify)
library(tools)
library(cluster)
library(h2o)

setwd("/home/saikiran/supervised_clustering/code/newcode/expt4")
dataset = read.csv("voice.csv")

names(dataset)
str(dataset)
dataset$label = sapply(dataset$label, function(x) ifelse(x == "male", 1, 2))

dataset[,1:20] = decostand(dataset[,1:20], method = "range")
table(dataset$label)

set.seed(1234)
misplaced_label_data = dataset
row.names(misplaced_label_data) = 1:nrow(misplaced_label_data)
misplaced_label_data_original = misplaced_label_data
index = sample(1:nrow(misplaced_label_data), nrow(misplaced_label_data)/2)
misplaced_label_data[index,]$label = sapply(misplaced_label_data[index,]$label, 
                                            function(x) ifelse(x == 1, 2, 1))
table(misplaced_label_data$label)

#with label changed
misplaced_label_data$label = as.factor(as.character(misplaced_label_data$label))
plt = autoplot(prcomp(misplaced_label_data[,-c(21)]), data = misplaced_label_data, colour = 'label', shape = F, 
               frame = T, frame.type = 'norm',label.size = 1, label = T)
plt
ggsave(filename = paste("results/plots/", "NNPCA_", "withlabelchanged", 
                        ".pdf",sep = ""), plot = plt)


#without labels changed 
misplaced_label_data_original$label = as.factor(as.character(misplaced_label_data_original$label))
plt = autoplot(prcomp(misplaced_label_data_original[,-c(21)]), data = misplaced_label_data_original, colour = 'label', shape = F, 
               frame = T, frame.type = 'norm',label.size = 1, label = T, title = "without outliers")
plt
ggsave(filename = paste("results/plots/", "NNPCA_", "originaldata", 
                        ".pdf",sep = ""), plot = plt)



ClusterPurity = function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

purity_c1 = c()
purity_c2 = c()
purity = c()

type = c()
features = misplaced_label_data


update_lists = function(idx, tab, lab){
  #if the table is as expected. 
  #dim of table is 2*2
  p = max(ClusterPurity(kmeans_obj$cluster, misplaced_label_data$label),
          ClusterPurity(sapply(kmeans_obj$cluster, function(x) ifelse(x == 2, 1, 2)), misplaced_label_data$label))
  p_c1 = max(ClusterPurity(kmeans_obj$cluster[idx], misplaced_label_data$label[idx]),
             ClusterPurity(sapply(kmeans_obj$cluster[idx], function(x) ifelse(x == 2, 1, 2)), misplaced_label_data$label[idx]))
  p_c2 = max(ClusterPurity(kmeans_obj$cluster[-idx], misplaced_label_data$label[-idx]),
             ClusterPurity(sapply(kmeans_obj$cluster[-idx], function(x) ifelse(x == 2, 1, 2)), misplaced_label_data$label[-idx]))
  
  result = c(p, p_c1, p_c2)
  return(result)
}


#normal clustering. 
kmeans_obj = kmeans(misplaced_label_data[,-c(21)], 2)
type = append(type, "NormalClustering")
idx = which(kmeans_obj$cluster == 1)
tab = table(kmeans_obj$cluster, misplaced_label_data$label)
res = update_lists(idx, tab, 1)

purity = append(purity, res[1])
purity_c1 = append(purity_c1, res[2])
purity_c2 = append(purity_c2, res[3])


#Generate NN features and do clustering. 
localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)
data.hex = as.h2o(misplaced_label_data)

feature_extraction = function(layer1, layer2){
  if(layer1 == 0){
    aec = h2o.deeplearning(x = setdiff(colnames(data.hex), "label"), 
                           y = "label", training_frame = data.hex,
                           activation = "Tanh",
                           hidden = c(layer2), epochs = 15, seed = 1234, rate = 0.001)
    features = as.data.frame(h2o.deepfeatures(aec, data.hex[,-c(21)], layer = 1))
    
  }else{
    aec = h2o.deeplearning(x = setdiff(colnames(data.hex), "label"), 
                           y = "label", training_frame = data.hex,
                           activation = "Tanh",
                           hidden = c(layer1, layer2), epochs = 15, seed = 1234, rate = 0.001)
    
    features = as.data.frame(h2o.deepfeatures(aec, data.hex[,-c(21)], layer = 1))
    features = cbind(features, as.data.frame(h2o.deepfeatures(aec, data.hex[,-c(21)], layer = 2)))
  }
  features$label = misplaced_label_data$label
  return(features)
}

count = 0
total_iters = length(seq(0,10)) * length(seq(1,10))
for(layer1 in seq(0,10)){
  for(layer2 in seq(1,10)){
    count = count + 1
    set.seed(1234)
    
    features = feature_extraction(layer1, layer2)
    #write data
    network_type = paste("NN_", layer1, "_", layer2, sep = "")
    write.csv(features, paste("NNdata/", network_type, ".csv", sep = ""), row.names = F)
    
    
    #kmeans clustering.
    kmeans_obj = kmeans(features[,-c(ncol(features))], 2)
    network_type = paste("NN_", layer1, "_", layer2, "_KMeansClustering")
    type = append(type, network_type)
    idx = which(kmeans_obj$cluster == 1)
    tab = table(kmeans_obj$cluster, features$label)
    res = update_lists(idx, tab, 1)
    
    purity = append(purity, res[1])
    purity_c1 = append(purity_c1, res[2])
    purity_c2 = append(purity_c2, res[3])
    print(paste(count, "of", total_iters))
  }
}

results = data.frame(type, purity, purity_c1, purity_c2)
results
improvement = c(0)
improvement_c1 = c(0)
improvement_c2 = c(0)
for(i in 2:nrow(results)){
  current_row = results[i,]
  improvement = append(improvement, ((current_row$purity/results[1,]$purity)-1) * 100)
  improvement_c1 = append(improvement_c1, ((current_row$purity_c1/results[1,]$purity_c1)-1) * 100)
  improvement_c2 = append(improvement_c2, ((current_row$purity_c2/results[1,]$purity_c2)-1) * 100)
}
results = data.frame(results, improvement, improvement_c1, improvement_c2)
write.csv(results, "results/tanh_voice_results.csv", row.names = F)




#plottings
all_files = list.files("/home/saikiran/supervised_clustering/code/newcode/expt4/NNdata",full.names = T, pattern = "NN")
for(i in 2:length(all_files))
{
  current_data = read.csv(all_files[i])
  current_data$label = as.factor(as.character(current_data$label))
  
  if(ncol(current_data) >= 3){
    plt = autoplot(prcomp(current_data[,-c(length(current_data))]), data = current_data,colour = 'label', shape = F, 
                   frame = T, frame.type = 'norm',label.size = 1, label = T)
    plt
    
    ggsave(filename = paste("results/plots/", "NNPCA_", file_path_sans_ext(basename(all_files[i])), 
                            ".pdf",sep = ""), plot = plt)
  }
  
}
