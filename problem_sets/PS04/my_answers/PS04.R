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
lapply(c("car", "texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load in the data
# (done above) install.packages("car")
library(car) #load in the library
data(Prestige) #Prestige came with the car library, loaded above
help(Prestige) #opens the help page with more information

# View the data structure and top of data
str(Prestige)
head(Prestige)

# Get an understanding of occupation types and how they're split  
# occupation type:  blue collar, professional, white collar
table(Prestige$type)

# 1(a) New variable professional - using if else

# professional = 1 if type is "prof", = 0 for "bc" or "wc" 
# Nested ifelse: the outer ifelse checks for "prof", and if that is False
# the inner ifelse checks for "bc" or "wc"
# Some may be NA, so they should stay that way (final ifelse) 
# as.factor() makes it a categorical (dummy) variable, not a continuous one
Prestige$professional <- as.factor(ifelse(Prestige$type=="prof", 1,
                                          ifelse(Prestige$type=="bc" | 
                                                 Prestige$type=="wc", 0, NA)))

# Check if the new variable is working well with a cross-tab of old and new var
# Prof should only appear under 1, bc and wc should only appear under 0
table(Prestige$type, Prestige$professional, useNA = "ifany")

# Check total counts in each category
table(Prestige$professional, useNA = "ifany")

# 1(b) Linear model with prestige as outcome and income, professional, and the 
      # interaction of the two as predictors

# Model with the two main effects and their interaction (income:professional)
# The : term is the interaction, so both main effects are written out too
# Continuous x dummy interaction: the slope of income can be different for 
# professionals and non-professionals
model1 <- lm(prestige ~ income + professional + income:professional, data=Prestige)

# View results
# Expect the summary to say "4 observations deleted due to missingness"
summary(model1)

# LaTeX table for the tex file
texreg(list(model1), digits=5,
       custom.coef.names=c("(Intercept)", "Income", 
                           "Professional", "Income x Professional"))

# 1(c) Prediction equation
# Using coefficients from the model for the equation
round(coef(model1), 5)

# 1(d) Interpret the coefficient for income. - written

# 1(e) Interpret the coefficient for professional
# Check the range for observed income to see if $0 is inside the data
range(Prestige$income)

# 1(f) Effect of $1,000 increase in income when professional = 1
# Directly use coefficients from the model
b <- coef(model1)

# Income + income:professional1 gives the marginal effect of income for prof
# The interaction term adds to the income slope when professional = 1
(b["income"] + b["income:professional1"]) * 1000

# 1(g) Effect of professional vs non-professional when income = 6000
# Gap at a given income = professional1 + income:professional1 * income
b["professional1"] + b["income:professional1"] * 6000

 





#####################
# Problem 2
#####################

# 2(a) Hypothesis test for the lawn sign coefficient from linear reg
# H0: Signs have no effect on vote share. Coefficient = 0
# H1: Coefficient is not 0 (two-sided test, alpha = 0.05)

# Set the estimate and the standard error from the regression table
estimate_sign <- 0.042
se_sign <- 0.016

# Degrees of freedom = N - number of estimated parameters
# 131 precincts - 3 (two variables + the constant) = 128
df <- 128

# t-value = estimate / standard error
t_val_sign <- estimate_sign / se_sign
t_val_sign

# Two-sided p-value
2*pt(t_val_sign, df = df, lower.tail = F)

# Get the critical value at alpha = 0.05 to compare with the t-value
qt(0.975, df = df)

# 2(b) Hypothesis test for being next to precincts with yard signs
# H0: Being next to a sign precinct has no effect (coefficient is 0)
# H1: the coefficient is not 0 (two-sided test, alpha = 0.05)

# Ge the estimate and standard error from the regression table
est_prec <- 0.042
se_prec <- 0.013

# t-value = estimate / standard error
t_val_prec <- est_prec / se_prec
t_val_prec

# Two-sided p-value (same degrees of freedom)
2*pt(t_val_prec, df = df, lower.tail = F)

# Get the critical value at alpha = 0.05 to compare with the t-value
qt(0.975, df = df)
