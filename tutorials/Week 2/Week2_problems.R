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
lapply(c("readr", "ggplot2", "dplyr", "viridis"),  pkgTest)

# Get working directory
getwd()

# Set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -------------------------------#
# 2. Load & Inspect Data
# -------------------------------#

df <- read_csv("../../datasets/fictional_data.csv")

# Quick overview
head(df)
str(df)
summary(df)

# Variables:
# - income: Monthly net income (numeric)
# - edu: University-level education in years (numeric)
# - cap: Binary variable (1 = lives in capital, 0 = otherwise)

# -------------------------------#
# 3. Descriptive Statistics
# -------------------------------#

### Measures of central tendency (mean, median) and variability (variance, sd) ###

# Income
mean_income <- mean(df$income)
median_income <- median(df$income)
var_income <- var(df$income)
sd_income <- sd(df$income)


# Education
mean_edu <- mean(df$edu)
median_edu <- median(df$edu)
var_edu <- var(df$edu)
sd_edu <- sd(df$edu)

mean_edu
median_edu
var_edu
sd_edu

# Comment:
# - Mean: average value, sensitive to outliers
# - Median: midpoint of the distribution, robust to outliers
# - Variance/SD: how spread out the values are
# - Standard Error (SE): how uncertain our estimate of the mean is


# Standard Error (SE) of the mean:
# SE = sd / sqrt(n)
# - Standard deviation (sd) measures how spread out the individual data points are.
# - Standard error (se) measures how much the sample mean itself would vary
#   if we repeatedly took new samples from the population.
#   → Larger n → smaller SE (mean estimate is more precise).
#   → Larger sd → larger SE (mean estimate is less precise).
# We use SE to build confidence intervals and perform hypothesis tests.
se_income <- sd_income / sqrt(length(df$income))
se_edu <- sd_edu / sqrt(length(df$edu))

se_income
se_edu

# -------------------------------#
# 4. Visualization with ggplot2
# -------------------------------#

# Create histogram of income with mean line

# Create histogram of education with mean line

# Create scatter plot: Income vs Education, colored by Capital
ggplot(df, aes(x = income, y = edu, color = factor(cap))) +
  geom_point() +
  scale_color_manual(values = c("black", "red"), labels = c("Non-capital", "Capital")) +
  labs(title = "Relationship between Education and Income",
       x = "Monthly Net Income (Euro)",
       y = "Years of University Education",
       color = "Residence") +
  theme_minimal()

# -------------------------------#
# 5. Confidence Intervals
# -------------------------------#

# Calculate 95% CI for income mean

# Calculate 99% CI for income mean

# --------------------------------------#
# 6. Central Limit Theorem (CLT) Example
# --------------------------------------#

# The CLT tells us: the sampling distribution of the mean approaches normality
# as the NUMBER of sample size increases, regardless of the original population distribution.
# though the original pop. distribution isn't a crazy distribution

set.seed(123)  # reproducibility
sample_means_n50 <- data.frame("mean"=replicate(50, mean(sample(df$income, size = 500, replace = TRUE))), "n"=100)
sample_means_n1000 <- data.frame("mean"=replicate(1000, mean(sample(df$income, size = 500, replace = TRUE))), "n"=1000)
sample_means_n10000 <- data.frame("mean"=replicate(10000, mean(sample(df$income, size = 500, replace = TRUE))), "n"=10000)

# Combine sampling distributions
df_clt <- rbind(sample_means_n50, sample_means_n1000, sample_means_n10000)

ggplot(df_clt, aes(x = mean, fill = n)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ n, scales = "free") +
  lims(x=c(1700, 2000)) + 
  geom_vline(xintercept = mean_income, color = "red", linetype = "dashed") +
  labs(title = "Central Limit Theorem: Sampling Distributions of the Mean",
       x = "Sample Mean of Income", y = "Count") +
  theme_minimal() + guides(fill="none")

# Interpretation: What do you notice about the differences in the sampling distributions?

# Income, for example, is usually not really normally distributed.
# First, we generate some hypothetical income data.

income <- rgamma(1000, shape = 1.1, scale = 2000) 
summary(income)
var(income)

# Let's have a look. Plot income variable you just created (using a histogram and density plot)
# What shape is the population distribution?


# Now we want to get our sampling distribution of the mean again.
# Create a sampling distribution of 50 draws from the population distribution
# And plot the sampling distribution (again using a histogram and density plot)
# What do you notice now?

# Do the same thing, but with 500 draws from the population distribution?
# Anything change?