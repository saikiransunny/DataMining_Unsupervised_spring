rm(list = ls(all = T))


# problem - 1
#####
#PART A 
A = matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3), nrow=3, ncol=8, byrow=TRUE)
A[A>0] = 1
library(philentropy)
distance(A, method="jaccard")
distance(A, method="cosine")

#PART B
A=matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3), nrow=3, ncol=8, byrow=TRUE)
A[A<3] = 0
A[A>0] = 1
distance(A, method="jaccard")
distance(A, method="cosine")

#PART C
A=matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3), nrow=3, ncol=8, byrow=TRUE)
m=3
n=8
means = rowMeans(A)
for (i in 1:3)
  for (j in 1:8)
    if(A[i,j] > 0)
      A[i,j] = A[i,j] - means[i]
distance(A, method="cosine")
#####


# problem - 2
#####
rm(list = ls(all = T))
library(MASS)
data("Boston")
head(Boston)
str(Boston)

library(ggplot2)
for(i in 1:ncol(Boston))
{
  if(typeof(Boston[,i]) != "integer"){
    print(ggplot(data = Boston, aes(Boston[,i])) + geom_histogram() + xlab(names(Boston)[i]))
  }
}

#inspecting the graphs and summary following changes can be made.
#changing the dataframe into a binary dataframe like in the question. 
summary(Boston)
table(sapply(Boston$crim, function(x) ifelse(x < 0.08, 1, 0)))
# ideally because of skewness we can consider 0 as one class and others as another.
#But taking the first quartile as the decision case. we get the following.
# low is 1, high is 0
Boston$new_crim = sapply(Boston$crim, function(x) ifelse(x < 0.08, 1, 0))
Boston$new_indus = sapply(Boston$indus, function(x) ifelse(x > 10, 0, 1))
Boston$new_nox = sapply(Boston$nox, function(x) ifelse(x >= 0.7, 0, 1))
Boston$new_age = sapply(Boston$age, function(x) ifelse(x > 75, 0, 1))
Boston$new_dis = sapply(Boston$dis, function(x) ifelse(x <= 3.7, 1, 0))
Boston$new_tax = sapply(Boston$tax, function(x) ifelse(x >= 500, 0, 1))
Boston$new_ptratio = sapply(Boston$ptratio, function(x) ifelse(x >= 19, 0, 1))
Boston$new_black = sapply(Boston$black, function(x) ifelse(x >= 396, 0, 1))
Boston$new_medv = sapply(Boston$medv, function(x) ifelse(x>= 30, 0, 1))

subset = Boston[,c(15:23)]
subset = data.frame(lapply(subset, as.factor))
trans_subset = as(subset, "transactions")
itemFrequencyPlot(trans_subset)
library(arules)
object = apriori(trans_subset, parameter = list(support = 0.05))
summary(object)

rules_lowcrime = subset(object, subset = rhs %in% "new_crim=1")
#inspect(rules_lowcrime)
rules_lowdistance = subset(object, subset = rhs %in% "new_dis=1")
#inspect(rules_lowdistance)


inspect(head(sort(rules_lowcrime, by = "lift")))
inspect(head(sort(rules_lowdistance, by = "lift")))


rules_lowpupilratio = subset(object, subset = rhs %in% "new_ptratio=1")
inspect(head(sort(rules_lowpupilratio, by = "lift")))
#regression model. 
subset = data.frame(lapply(subset, function(x) as.numeric(as.character(x))))
model = lm(new_ptratio~., subset)
summary(model)
#####



#problem - 3
#####
rm(list = ls(all = T))
# constructing a table from text book data. 
library(ElemStatLearn)
data("marketing")
names(marketing)
str(marketing)
summary(marketing)
sum(is.na(marketing))
#there are na in the data. Replacing with the median value of each feature. 
marketing$Edu[is.na(marketing$Edu)] = median(marketing$Edu)
marketing$Occupation[is.na(marketing$Occupation)] = median(marketing$Occupation)
marketing$Lived[is.na(marketing$Lived)] = median(marketing$Lived)
marketing$Dual_Income[is.na(marketing$Dual_Income)] = median(marketing$Dual_Income)
marketing$Household[is.na(marketing$Household)] = median(marketing$Household)
marketing$Status[is.na(marketing$Status)] = median(marketing$Status)
marketing$Ethnic[is.na(marketing$Ethnic)] = median(marketing$Ethnic)
marketing$Language[is.na(marketing$Language)] = median(marketing$Language)
marketing$Marital[is.na(marketing$Marital)] = median(marketing$Marital)
marketing$Home_Type[is.na(marketing$Home_Type)] = median(marketing$Home_Type)
sum(is.na(marketing))
marketing$target = 1
training_sample = marketing
reference_sample = marketing
rm(marketing)
for(i in 1:ncol(reference_sample)){
  reference_sample[,i] = sample(reference_sample[,i])
}
reference_sample$target = 0

combined_data = rbind(reference_sample, training_sample)
library(rpart)
library(rattle)
combined_data$target = as.factor(as.character(combined_data$target))
model = rpart(target~., combined_data)
plot(model)
summary(model)
pred = predict(model, combined_data[,-c(15)])
pred
#####
# no predictive power in the model