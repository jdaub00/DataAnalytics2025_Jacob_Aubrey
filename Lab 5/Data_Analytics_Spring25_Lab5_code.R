library(readr)
library(ggplot2)
library(e1071)
library(caret)

## read data
NY_House_Dataset <- read_csv("Courses/Data Analytics/Fall24/assignments/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

# dataset <- wine
# dataset$Type <- as.factor(dataset$Type)

## column names
names(dataset)

## split train/test
train.indexes <- sample(nrow(dataset),0.75*nrow(dataset))

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

x <- dataset[,2:4] 
y <- as.factor(as.matrix(dataset[,1]))

## feature boxplots
boxplot(X, main="iris features")

## class label distributions
plot(Y)

## feature-class plots
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=X, y=Y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=X, y=Y, plot="density", scales=scales)

ggplot(dataset, aes(x = Alcohol, y = Ash, colour = Type)) +
  geom_point()

svr.mod0 <- svm(PRICE ~ PROPERTYSQFT, train)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

svr.pred <- cbind(test$PRICE,svr.pred)

ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset, kernel="radial")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm")


svr.mod0 <- lm(log10(PRICE) ~ log10(PROPERTYSQFT), dataset)

summary(svr.mod0)

svr.pred <- predict(svr.mod0, dataset)

svr.outs <- data.frame(real=log10(dataset$PRICE), pred=svr.pred)

ggplot(svr.outs, aes(x = real, y = pred)) +
  geom_point() +
  stat_smooth(method = "lm")
