# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("readr", "ggplot2"),  pkgTest)

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Check working directory
getwd()

# Agenda
# (a.) Correlation
# (b.) Bivariate regression 

# -------------------------------#
# a. Correlation
# -------------------------------#

df <- read.csv("../../datasets/fictional_data.csv")
View(df)

# Create a scatter plot 

# Calculate correlation

# Add correlation to scatter plot

# Improve visualization with detailed axis and main labels, save as PDF

# Run a t-test for the correlation coefficient

# Check p-value

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# -------------------------------#
# b. Bivariate regression
# -------------------------------#

# Question: Is there a relationship between education and income?
# Model: income = b0 + b1 * education
# Save model as object

# Output interpretation:
#  - Intercept (b0)
#  - Slope (b1)

# Conduct t-test for the slope of a regression line

# Check p-value

# Create a residual plot

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Calculate 95% and 99% confidence intervals around your estimated coefficients

# Create a scatter plot w/ educate and income, add regression line


# What is the prediction equation?

# Make predictions for first observation in df

# Make predictions for a range of x values (specifically min to max of education)

# Add standard errors for your predicted values

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
# Rembmer: more variability in individual responses --> wider intervals

# Make predictions for x values not in data
# (1) Mean education
# (2) Only unique values of x

# Now, plot those predictions in base R

# Plot, regression line with confidence intervals
# First, save confidence intervals
# Second, create scatter plot
# Then, add regression line, lower bound, and upper bound

# And now try with ggplot2