# Lab 2: Applying linear regression models to the NY Housing Dataset

# Reading in the necessary libraries
library("ggplot2")
library("readr")

# Reading in the dataset
NY_House_Dataset <- read_csv("C:/Users/jdaub/OneDrive/Desktop/Documents/PhD/Spring 2025/Data Analytics/Labs/Lab 2/NY-House-Dataset.csv")

# Assign NY_House_Dataset to dataset
dataset <- NY_House_Dataset

# Plot the data for initial visual analysis 
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) + geom_point()

# Create two new columns for log10 of price and property square footage
dataset$log10_PRICE <- log10(dataset$PRICE)
dataset$log10_PROPERTYSQFT <- log10(dataset$PROPERTYSQFT)

# Plot the data as log10 for better visualization 
ggplot(dataset, aes(x = log10_PROPERTYSQFT, y = log10_PRICE)) + geom_point()

# Extract the column names and return them as a vector
names(dataset)

# In analyzing the plots, there seem to be outliers
# But first fit the data to a linear model 
# using property square footage as the predictor of price
lmod <- lm(log10_PRICE ~ log10_PROPERTYSQFT, data = dataset)

# Print the results of lmod
summary(lmod)

# The R2 value is terrible indicating a poor trend, 
# or the model needs to be cleaned of outliers
# Remove the random datapoint at the top right of the log10 plot (> log10(x) = 9)
dataset <- dataset[dataset$PRICE < 1e9,]
dataset <- dataset[dataset$log10_PRICE < 9,]

# Replot the dataset
ggplot(dataset, aes(x = log10_PROPERTYSQFT, y = log10_PRICE)) + geom_point()

# Rerun the linear model and print the results
lmod <- lm(log10_PRICE ~ log10_PROPERTYSQFT, data = dataset)
summary(lmod)

# The R2 did not improve
# remove the strange vertical line of datapoints (homes with identical square footage)
# In assessing the dataset, the outlier points all have the square footage, 2184.207862
dataset <- dataset[dataset$PROPERTYSQFT != 2184.207862,]

# Replot the dataset
ggplot(dataset, aes(x = log10_PROPERTYSQFT, y = log10_PRICE)) + geom_point()

# Rerun the linear model and print the results
lmod <- lm(log10_PRICE ~ log10_PROPERTYSQFT, data = dataset)
summary(lmod)

# R2 improved to 0.5849
# How does the model perform using different inputs? Beds? Baths?

# Plot price vs beds
ggplot(dataset, aes(x = BEDS, y = PRICE)) + geom_point()

# Remove outlier bed 
dataset <- dataset[dataset$PRICE < 1.5e8,]

# Plot price vs beds
ggplot(dataset, aes(x = BEDS, y = PRICE)) + geom_point()

# Plot log10(price) vs beds
ggplot(dataset, aes(x = BEDS, y = log10_PRICE)) + geom_point()

# Create a new column in dataset for log10(BEDS)
dataset$log10_BEDS <- log10(dataset$BEDS)

# Plot log10(price) vs log10(beds)
ggplot(dataset, aes(x = log10_BEDS, y = log10_PRICE)) + geom_point()

# Run a linear model of the plot and print the results
lmod <- lm(log10_PRICE ~ log10_BEDS, data = dataset)
summary(lmod)

# Plot price vs baths
ggplot(dataset, aes(x = BATH, y = PRICE)) + geom_point()

# Remove outlier bed 
# dataset <- dataset[dataset$PRICE < 1.5e8,]

# Plot price vs baths
ggplot(dataset, aes(x = BATH, y = PRICE)) + geom_point()

# Plot log10(price) vs beds
ggplot(dataset, aes(x = BATH, y = log10_PRICE)) + geom_point()

# Create a new column in dataset for log10(BEDS)
dataset$log10_BATH <- log10(dataset$BATH)

# Plot log10(price) vs log10(baths)
ggplot(dataset, aes(x = log10_BATH, y = log10_PRICE)) + geom_point()

# Run a linear model of the plot and print the results
lmod <- lm(log10_PRICE ~ log10_BATH, data = dataset)
summary(lmod)

# Run a linear model using price, beds, and baths as predictors
lmod <- lm(log10_PRICE ~ log10_PROPERTYSQFT + log10_BEDS + log10_BATH, data = dataset)
summary(lmod)
