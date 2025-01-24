# Read in the data from the csv and assign it to the variable, EPI_data
EPI_data <- read.csv("C:/Users/jdaub/OneDrive/Desktop/Documents/PhD/Spring 2025/Labs/Lab 1/epi_2024_results_DAF24.csv")

# Assign the path for the working directory
setwd("C:/Users/jdaub/OneDrive/Desktop/Documents/PhD/Spring 2025/Labs/Lab 1/")

# Re-import the csv file from the working directory 
EPI_data <- read.csv("epi_2024_results_DAF24.csv") 

# Open the data in R for visualization 
View(EPI_data)

# Temporarily attach EPI_data for easier referencing
attach(EPI_data) # sets the ‘default’ object 

# Print the values of EPI_data, column EPI.new
EPI.new # prints out values EPI_data$EPI.new

# Create a logical vector where True represents a missing value
NAs <- is.na(EPI.new) # records True values if the value is NA 

# Sorts through the logical vector just established removing all True (missing) values
# Saves the new array
EPI.new.noNAs <- EPI.new[!NAs]

# Provides a summary of the EPI.new array
summary(EPI.new)  

# Calculates min, lower quartile, median, upper quartile, and max for EPI.new
# NA values ignored in calculation of min, lower quartile, median, upper quartile, and max
fivenum(EPI.new,na.rm=TRUE)

# Generates a stem and leaf plot of EPI.new
stem(EPI.new)

# Generates a histogram of EPI.new 
hist(EPI.new) 

# Adjustment of EPI.new histogram parameters
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

# Adds a curve to the EPI.new histogram
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw="SJ" 

# Adds a rug to the EPI.new histogram
rug(EPI.new)

# Creates a box plot comparing EPI.new and APO.new (two columns in EPI_data)
boxplot(EPI.new, APO.new)

# Create a sequence of numbers from 20 to 80 with a step-size of 1
x<-seq(20,80,1) 

# Calculates the probability density function (PDF) of a normal distribution with a
# mean of 42 and std. dev. of 5 at each x value.
q<- dnorm(x,mean=42, sd=5,log=FALSE) 

# Plot the normal distribution
lines(x,q)

# Plot another curve with scaling
lines(x,.4*q) 

# Recompute and plot the PDF
q<-dnorm(x,mean=65, sd=5,log=FALSE) 
lines(x,.12*q) 

# Plot EPI.new empirical cumulative distribution
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

# Generate a Q-Q plot to compare EPI.new quantiles to the standard normal distribution quantiles
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

# Generate a new Q-Q plot comparing EPI.new against 250 random samples 
# within a t distribution of five degrees of freedom
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

