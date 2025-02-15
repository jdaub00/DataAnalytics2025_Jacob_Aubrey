###################
##### Abalone #####
###################

library(class)
library(ggplot2)

# read dataset
abalone <- read.csv("C:/Users/jdaub/OneDrive/Desktop/Documents/PhD/Spring 2025/Data Analytics/Labs/Lab 3/abalone_dataset.csv")

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))


## alternative way of setting age.group
dataset$age.group[dataset$rings<=8] <- "young"
dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

k.list <- c(59, 61, 63, 65, 67, 69, 71)

# empty list for accuracy
accuracy.list <- c()

# loop: train and predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k = k)
  
  contingency.table <- table(knn.predicted, dataset$age.group, dnn = list('predicted', 'actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$age.group)
  
  accuracy.list <- c(accuracy.list, accuracy)
  
}

plot(k.list, accuracy.list, type = "b")

knn.predicted <- knn(train = dataset[,2:4], test = dataset[,2:4], cl = dataset$age.group, k = 59)

contingency.table <- table(knn.predicted, dataset$age.group, dnn = list('predicted', 'actual'))

contingency.table

# Calculate classification accuracy
sum(diag(contingency.table))/length(dataset$age.group)

k.list <- c(59, 61, 63, 65, 67, 69, 71)

# empty list for accuracy
accuracy.list <- c()

# loop: train and predict model for each k, compute accuracy and append it to list
for (k in k.list) {
  
  knn.predicted <- knn(train = dataset[,5:8], test = dataset[,5:8], cl = dataset$age.group, k = k)
  
  contingency.table <- table(knn.predicted, dataset$age.group, dnn = list('predicted', 'actual'))
  
  accuracy <- sum(diag(contingency.table))/length(dataset$age.group)
  
  accuracy.list <- c(accuracy.list, accuracy)
  
}

plot(k.list, accuracy.list, type = "b")

knn.predicted <- knn(train = dataset[,5:8], test = dataset[,5:8], cl = dataset$age.group, k = 71)

contingency.table <- table(knn.predicted, dataset$age.group, dnn = list('predicted', 'actual'))

contingency.table

# Calculate classification accuracy
sum(diag(contingency.table))/length(dataset$age.group)

k.list <- c(2, 3, 4, 5)

wcss.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(dataset[,5:8], centers = k)
  wcss <- abalone.km$tot.withinss
  wcss.list <- c(wcss.list, wcss)
  
  assigned.clusters <- as.factor(abalone.km$cluster)
  
  }

plot(k.list, wcss.list, type = "b")

abalone.km <- kmeans(dataset[,5:8], centers = 3)
wcss <- abalone.km$tot.withinss
wcss.list <- c(wcss.list, wcss)

assigned.clusters <- as.factor(abalone.km$cluster)

# Explicitly print the ggplot object
print(
  ggplot(dataset, aes(x = shucked_wieght, y = shell_weight, colour = assigned.clusters)) + 
    geom_point()
)
