setwd("D:/Spring 2018/STA/Project/newcode/expt4/")
data = read.csv("results/tanh_voice_results.csv")

getname = function(x){
  split = unlist(strsplit(as.character(x), "_"))
  string = paste(trimws(split[1], "both"),
                 trimws(split[2], "both"), trimws(split[3], "both"), sep = "")
  return(string)
}
data$type = sapply(data$type, function(x) ifelse(x == "NormalClustering", "NC", getname(x)))



library(ggplot2)
ggplot(data, aes(x=type, y=improvement, group=1)) +
  geom_line()+
  geom_point() + 
  geom_hline(yintercept = max(data$improvement), color = "green", linetype = "dashed", size = 1.2) +
  annotate("text", label = paste("Max Improvement-", data$type[grep(max(data$improvement), data$improvement)[1]], " :", 
                                 round(max(data$improvement),2), sep = ""), 
           x = "NN11", 
           y = 2.1, color = "black")+
  geom_hline(yintercept = min(data$improvement[1:nrow(data)]), color = "green", linetype = "dashed", size = 1.2) +
  annotate("text", label =  paste("Min Improvement-", data$type[grep(min(data$improvement), data$improvement)[1]], " :", 
                                  round(min(data$improvement),2), sep = ""),
           x = "NN11", 
           y = -1.1) + 
  ggtitle("NN Improvement summarization on 50% mislabels") + theme(plot.title = element_text(hjust = 0.5))+
  annotate("rect", xmin = "NN09", xmax = "NN39", ymin = -6, ymax = -3.8,
           alpha = .2) + annotate("text", label = paste("Mean Improvement: ", round(mean(data$improvement[2:nrow(data)]), 2), sep = ""),
                                  x = "NN21", y = -5)


purity = c(data$purity[1], max(data$purity))
Type = c("OriginalFeatures", "NN-01Features")
plot_data = data.frame(Type, purity)
ggplot(plot_data, aes(x = Type, y = purity, group = 1)) + 
  geom_histogram(stat = "identity")+
  ggtitle("Purity comparison on 50% mislabels")+ theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label =  paste("0.52"),
           x = "NN-01Features", 
           y = 0.55, color = "black")+
  annotate("text", label =  paste("0.51"),
           x = "OriginalFeatures", 
           y = 0.55, color = "black")

