#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
#lapply(c("ggplot2"),  pkgTest)

#set wd to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd() #checking path

#####################
# Problem 1
#####################

#Q1(a): Calculating the chi-squared test stat by hand in R
#This test asks whether two categorical variables are related (vars: driver
#class and police officer behavior)
#H0: Driver class and officer behavior are statistically independent.
#H1: Driver class and officer behavior are not statistically independent.

#STEP 1: put the observed numbers into a matrix
obs_table <- matrix(c(14, 6, 7, 7, 7, 1),
                nrow = 2, byrow = TRUE)
                # byrow = TRUE fills the matrix across rows (left to right) 
rownames(obs_table) <- c("Upper class", "Lower class")
colnames(obs_table) <- c("Not stopped", "Bribe requested", 
                                         "Stopped/warning")
obs_table

#STEP 2: calculate the marginal totals
#To calculate expected frequencies, I need to know the overall distribution of
#each variable on its own (its "marginal" distribution).
row_totals <- rowSums(obs_table) # total encounters per driver class
col_totals <- colSums(obs_table) # total times each officer behavior happened
n_total <- sum(obs_table) # total number of encounters
row_totals #27,15
col_totals #21,13,8
n_total #42

# Double check totals using R's function for marginal vals
addmargins(obs_table)

#STEP 3: Calculate expected frequencies (count if H0 were true)
#formula: F(e) = (row total * column total) / n_total
#row_totals[1] * col_totals multiplies the upper class total by each column
#total (27*21, 27*13, 27*8); same for row 2 with the lower class total

exp_table <- matrix(c(row_totals[1] * col_totals,
                      row_totals[2] * col_totals) / n_total,
                    nrow = 2, byrow = TRUE)
rownames(exp_table) <- rownames(obs_table)
colnames(exp_table) <- colnames(obs_table)
exp_table

#STEP 4: Calculate the chi-squared test statistic
#formula: chi-squared = sum of (observed - expected)^2 / expected for every cell
#(observed - expected) = how far the data are from what H0 predicts
#squared = so positive and negative diff don't cancel out
#divided by expected = standardizes each diff relative to the size of the cell
#larger chi-squared = more evidence against independence (H0)

#Each cell's contribution (R applies the formula to each cell)
c_contributions <- (obs_table - exp_table)^2 / exp_table
c_contributions

#Sum all the contributions together to get the test statistic
chi_sq <- sum(c_contributions)
chi_sq # 3.79

#Double check the answer test statistic using R's function
#(R warns "Chi-squared approximation may be incorrect" because of the small
#expected frequencies, not because of an error in the code)
chisq.test(obs_table)

#Q1(b): Calculating the p-value from the test statistic
#The p-val is the probability of getting a chi-squared statistic at least as
#big as 3.79 IF H0 were true (if class and officer behavior were independent).
#A small p-value means obs data would be unlikely under H0.

#STEP 1: Calculate the degrees of freedom (matters for chi-sq distribution)
#formula: df = (number of rows - 1) * (number of columns - 1)
df <- (nrow(obs_table) - 1) * (ncol(obs_table) - 1)
df # 2

#STEP 2: Get the p-val from the chi-squared distribution
#lower.tail = FALSE because I want the area to the right of test stat.
p_value <- pchisq(chi_sq, df, lower.tail = FALSE)
p_value # 0.15

#STEP 3: Compare the p-value to alpha of 0.1
p_value <= 0.1 # FALSE, so we fail to reject H0

# Double check p-val using R's function
chisq.test(obs_table)$p.value # 0.15

#Q1(c): Calculate the standardized residuals for each cell
#Standardized residual measures how far each cell's observed num is from its
#expected count (under H0), in standardized units. Unlike the chi-squared stat, 
#residuals look cell-by-cell and directional (positive = more than expected,
#negative = fewer than expected). formula: (observed - expected) / sqrt
#(expected * (1 - row proportion) * (1 - column proportion))

#STEP 1: Calculate row and column proportions
row_props <- row_totals / n_total
col_props <- col_totals / n_total
row_props #0.64, 0.36
col_props #0.50, 0.31, 0.19

#STEP 2: Calculate the (1 - row prop) * (1 - col prop) part for every cell
#same approach as the expected table: row 1's value * each column, then row 2's
adjusted_table <- matrix(c((1 - row_props[1]) * (1 - col_props),
                           (1 - row_props[2]) * (1 - col_props)),
                         nrow = 2, byrow = TRUE)
adjusted_table

#STEP 3: Calculate the standardized residuals for each cell
std_res <- (obs_table - exp_table) / sqrt(exp_table * adjusted_table)
std_res
round(std_res, 2) #round to second dec 

# Double check using R's built-in function
round(chisq.test(obs_table)$stdres, 2)

#####################
# Problem 2
#####################

#Q2(a): Does the reservation policy (reserving GP seats for women) affect the
#number of new/repaired water facilities in villages?

#STEP 1: Load the data
women_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#STEP 2: Look at the data
head(women_data) #first 6 rows 
str(women_data) #num of observations and the type of each variable
summary(women_data) #min/max/mean of each variable to spot anything weird

#Check how many villages were reserved (should be apprx 1/3) (1) v not (0)
table(women_data$reserved)

#STEP 3: State the null and alternative (two-tailed) hypotheses
#Regression model: water = b0 + b1 * reserved + e
#b1 is the diff in the average number of new/repaired drinking-water facilities
#between reserved and unreserved villages.
#H0: b1 = 0 (the reservation policy has no effect on the number of new or
#repaired drinking-water facilities)
#H1: b1 != 0 (the reservation policy has an effect on the number of new or
#repaired drinking-water facilities, in either direction)

#Q2(b): Run a bivariate regression to test the hypothesis
#Bivariate reg looks at the outcome (water) as a linear function of var (reserved)
#Because reserved is binary (0 or 1), the model predicts two mean values
#So b1 turns out to be the diff in average water facilities between the groups,
#and testing H0:b1 = 0 tests if that difference is zero.

#STEP 1: Run the regression + save it as an object
water_model <- lm(water ~ reserved, women_data)
summary(water_model)
#B0 intercept: 14.74
#B1 Reserved: 9.25  (std. error 3.95), t = 2.34, p = 0.02
#R-squared = 0.017

#STEP 2: Compare the p-val to alpha (using the conventional 0.05)
#p = 0.02 < 0.05, so we can reject H0

#STEP 3: Check with confidence intervals - 95% confidence interval for b1
#If it doesn't include 0, that matches rejecting H0 at alpha = 0.05
confint(water_model, level = 0.95)
#reserved: [1.49, 17.02], doesn't include 0
