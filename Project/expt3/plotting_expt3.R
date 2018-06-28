setwd("D:/Spring 2018/STA/Project/newcode/expt3/")
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
  ggtitle("NN Improvement summarization on 10% of mislabels") + theme(plot.title = element_text(hjust = 0.5))+
  annotate("rect", xmin = "NN09", xmax = "NN39", ymin = -13, ymax = -8,
           alpha = .2) + annotate("text", label = paste("Mean Improvement: ", round(mean(data$improvement[2:nrow(data)]), 2), sep = ""),
                                  x = "NN21", y = -10)


purity = c(data$purity[1], max(data$purity))
Type = c("OriginalFeatures", "NN-12Features")
plot_data = data.frame(Type, purity)
ggplot(plot_data, aes(x = Type, y = purity, group = 1)) + 
  geom_histogram(stat = "identity")+
  ggtitle("Purity comparison - 10% mislabels")+ theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label =  paste("0.87"),
           x = "NN-12Features", 
           y = 0.89, color = "black")+
  annotate("text", label =  paste("0.63"),
           x = "OriginalFeatures", 
           y = 0.65, color = "black")

