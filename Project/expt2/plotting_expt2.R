setwd("D:/Spring 2018/STA/Project/newcode/expt2/")
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
           x = data$type[grep(max(data$improvement), data$improvement)[1]], 
           y = max(data$improvement)+2, color = "black")+
  geom_hline(yintercept = min(data$improvement[1:nrow(data)]), color = "green", linetype = "dashed", size = 1.2) +
  annotate("text", label =  paste("Min Improvement-", data$type[grep(min(data$improvement), data$improvement)[1]], " :", 
                                  round(min(data$improvement),2), sep = ""),
           x = data$type[grep(min(data$improvement[1:nrow(data)]), data$improvement)[1]], 
           y = min(data$improvement[1:nrow(data)])-2, color = "black") + 
  ggtitle("NN Improvement summarization on 10% outliers on all features") + theme(plot.title = element_text(hjust = 0.5))+
  annotate("rect", xmin = "NN48", xmax = "NN81", ymin = 17, ymax = 24,
           alpha = .2) + annotate("text", label = paste("Mean Improvement: ", round(mean(data$improvement[2:nrow(data)]), 2), sep = ""),
                                  x = "NN65", y = 20)



purity = c(data$purity[1], max(data$purity))
Type = c("OriginalFeatures", "NN-21Features")
plot_data = data.frame(Type, purity)
ggplot(plot_data, aes(x = Type, y = purity, group = 1)) + 
  geom_histogram(stat = "identity")+
  ggtitle("Purity comparison. Outlier - 10% on all features")+ theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label =  paste("0.8"),
           x = "NN-21Features", 
           y = 0.81, color = "black")+
  annotate("text", label =  paste("0.55"),
           x = "OriginalFeatures", 
           y = 0.58, color = "black")

