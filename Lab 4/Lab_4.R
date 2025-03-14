library(caret)
library(ggfortify)
library(rgl)
library(randomForest)
library(readr)
library(EnvStats)
library("ggplot2")
library(mgcv)
library(boot)
library(MASS)
library(glmnet)
library(class)

wine

colnames(wine) <- c("Class", "Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids", "Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")



# Select predictors (adjusting to specific columns if needed)
predictors <- wine[, c(2:14)]  # Modify if needed

# Standardize data (mean = 0, variance = 1)
predictors_scaled <- scale(predictors)

# Run PCA
pca_result <- prcomp(predictors_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA: Importance of each principal component
summary(pca_result)

# Scree plot (explained variance)
scree_plot <- data.frame(PC = 1:length(pca_result$sdev),
                         Variance_Explained = (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100)

ggplot(scree_plot, aes(x = PC, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(group = 1), color = "red") +
  geom_point(color = "red") +
  labs(title = "Scree Plot: Variance Explained by Principal Components",
       x = "Principal Component",
       y = "Variance Explained (%)") +
  theme_minimal()

wine$Class <- as.factor(wine$Class)  # Convert to factor before plotting

autoplot(pca_result, data = wine, colour = 'Class',  # Uses distinct colors for classes
         loadings = TRUE, 
         loadings.colour = 'orange',  # Arrows in black
         loadings.label = TRUE, 
         loadings.label.colour = 'black',  # Text in black
         loadings.label.size = 3) +
  scale_color_brewer(palette = "Set1")  # Distinct colors for classes

# Alcalinity of ash, Nonflavanoid phenols, Flavanoids, Proanthocyanins, and Total phenols
# all contribute significantly to PC1
# Rerun PCA using only these predictors

# Select specific predictor columns
predictors_reduced <- wine[, c("Alcalinity of ash", "Nonflavanoid phenols", "Flavanoids", "Proanthocyanins", "Total phenols")]

# Run PCA
pca_result <- prcomp(predictors_reduced, center = TRUE, scale. = TRUE)  # Scaling applied

# Summary of PCA: Importance of each principal component
summary(pca_result)

# Scree plot (explained variance)
scree_plot <- data.frame(PC = 1:length(pca_result$sdev),
                         Variance_Explained = (pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100)

ggplot(scree_plot, aes(x = PC, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(group = 1), color = "red") +
  geom_point(color = "red") +
  labs(title = "Scree Plot: Variance Explained by Principal Components",
       x = "Principal Component",
       y = "Variance Explained (%)") +
  theme_minimal()

wine$Class <- as.factor(wine$Class)  # Convert to factor before plotting

autoplot(pca_result, data = wine, colour = 'Class',  # Uses distinct colors for classes
         loadings = TRUE, 
         loadings.colour = 'orange',  # Arrows in orange
         loadings.label = TRUE, 
         loadings.label.colour = 'black',  # Text in black
         loadings.label.size = 3) +
  scale_color_brewer(palette = "Set1")  # Distinct colors for classes

# kNN classification with all predictors

# Convert Class to a factor
wine$Class <- as.factor(wine$Class)

# Select predictors 
kNN_predictors <- wine[, c(2:14)]  # Replace with actual column names

# Normalize predictors (scaling to [0,1] range)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
kNN_predictors_norm <- as.data.frame(lapply(kNN_predictors, normalize))

# Add Class column back
wine_norm <- cbind(kNN_predictors_norm, Class = wine$Class)

# Split data into training (80%) and testing (20%)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(wine_norm$Class, p = 0.8, list = FALSE)
train_data <- wine_norm[train_index, ]
test_data <- wine_norm[-train_index, ]

# Define predictor and target variables
train_X <- train_data[, -ncol(train_data)]  # Exclude Class
train_Y <- train_data$Class  # Target variable
test_X <- test_data[, -ncol(test_data)]  # Exclude Class
test_Y <- test_data$Class  # Target variable

# Train kNN model (choose k, e.g., k = 5)
k_value <- 5
predictions <- knn(train = train_X, test = test_X, cl = train_Y, k = k_value)

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_Y)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Model Accuracy:", accuracy * 100, "%\n")

# Get performance metrics using confusionMatrix from the caret package
conf_matrix <- caret::confusionMatrix(predictions, test_Y)

# Precision, Recall, and F1 Score for each class
cat("Precision for each class:\n")
print(conf_matrix$byClass[, "Precision"])  # Use matrix indexing

cat("\nRecall for each class:\n")
print(conf_matrix$byClass[, "Recall"])  # Use matrix indexing

cat("\nF1 Score for each class:\n")
print(conf_matrix$byClass[, "F1"])  # Use matrix indexing

# Optionally, calculate the average Precision, Recall, and F1 Score
cat("\nAverage Precision:", mean(conf_matrix$byClass[, "Precision"]), "\n")
cat("Average Recall:", mean(conf_matrix$byClass[, "Recall"]), "\n")
cat("Average F1 Score:", mean(conf_matrix$byClass[, "F1"]), "\n")

# kNN classification with PCs

# Select predictor columns
kNN_predictors <- wine[, c(2:14)]

# Run PCA
pca_result <- prcomp(kNN_predictors, center = TRUE, scale. = TRUE)

# Extract the first 3 PCs
pcs <- pca_result$x[, 1:3]  # The first 3 principal components

# Convert pcs (PCA results) to a data frame
pcs_df <- as.data.frame(pcs)

# Add the Class column for classification
wine_pca <- cbind(pcs_df, Class = wine$Class)

# Split data into training (80%) and testing (20%)
set.seed(123)  # For reproducibility
train_index <- createDataPartition(wine_pca$Class, p = 0.8, list = FALSE)
train_data <- wine_pca[train_index, ]
test_data <- wine_pca[-train_index, ]

# Define predictor and target variables
train_X <- train_data[, -ncol(train_data)]  # Exclude Class
train_Y <- train_data$Class  # Target variable
test_X <- test_data[, -ncol(test_data)]  # Exclude Class
test_Y <- test_data$Class  # Target variable

# Train kNN model (choose k, e.g., k = 5)
k_value <- 5
predictions <- knn(train = train_X, test = test_X, cl = train_Y, k = k_value)

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_Y)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Model Accuracy:", accuracy * 100, "%\n")

# Get performance metrics using confusionMatrix from the caret package
conf_matrix <- caret::confusionMatrix(predictions, test_Y)

# Precision, Recall, and F1 Score for each class
cat("Precision for each class:\n")
print(conf_matrix$byClass[, "Precision"])  # Use matrix indexing

cat("\nRecall for each class:\n")
print(conf_matrix$byClass[, "Recall"])  # Use matrix indexing

cat("\nF1 Score for each class:\n")
print(conf_matrix$byClass[, "F1"])  # Use matrix indexing

# Optionally, calculate the average Precision, Recall, and F1 Score
cat("\nAverage Precision:", mean(conf_matrix$byClass[, "Precision"]), "\n")
cat("Average Recall:", mean(conf_matrix$byClass[, "Recall"]), "\n")
cat("Average F1 Score:", mean(conf_matrix$byClass[, "F1"]), "\n")

