setwd("D:/Spring 2018/STA/Project/newcode")
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
  annotate("text", label = paste("Max Improvement-", data$type[grep(max(data$improvement), data$improvement)], " :", 
                                 round(max(data$improvement),2), sep = ""), 
           x = data$type[grep(max(data$improvement), data$improvement)], 
           y = max(data$improvement)+2, color = "black")+
  geom_hline(yintercept = min(data$improvement[2:nrow(data)]), color = "green", linetype = "dashed", size = 1.2) +
  annotate("text", label =  paste("Min Improvement-", data$type[grep(min(data$improvement[2:nrow(data)]), data$improvement)], " :", 
                                  round(min(data$improvement[2:nrow(data)]),2), sep = ""),
           x = data$type[grep(min(data$improvement[2:nrow(data)]), data$improvement)], 
           y = min(data$improvement[2:nrow(data)])-2, color = "black") + 
  ggtitle("Neural Network Improvement summarization") + theme(plot.title = element_text(hjust = 0.5))+
  annotate("text", label = paste("Mean Improvement: ", round(mean(data$improvement[2:nrow(data)]), 2), sep = ""),
           x = "NN55", y = 20) +
  annotate("rect", xmin = "NN44", xmax = "NN66", ymin = 17, ymax = 24,
             alpha = .2)


purity = c(min(data$purity), max(data$purity))
Type = c("OriginalFeatures", "NN-62Features")
plot_data = data.frame(Type, purity)
ggplot(plot_data, aes(x = Type, y = purity, group = 1)) + 
  geom_histogram(stat = "identity")+
  ggtitle("purity using original features vs NN features")+ theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", label =  paste("0.65"),
           x = "OriginalFeatures", 
           y = 0.7, color = "black")+
  annotate("text", label =  paste("0.97"),
           x = "NN-62Features", 
           y = 1, color = "black")

