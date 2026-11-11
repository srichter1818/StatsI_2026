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
lapply(c("texreg"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#####################
# Question 1
#####################

# Look at the data first
# voteshare is the incumbent's vote share (proportion between 0 and 1)
# difflog is the logged difference in spending between incumbent and challenger
str(inc.sub)
summary(inc.sub$voteshare)
summary(inc.sub$difflog)

# 1(a). Run a regression for voteshare (outcome) on difflog (predictor)
# lm() uses OLS and explains y through x(s)
model1 <- lm(voteshare ~ difflog, data = inc.sub)

# texreg() prints the reg table as LaTeX code to paste into the write-up
texreg(model1, digits = 3, caption = "Regression of voteshare on difflog")

# 1(b). Make scatterplot of the two variables with reg line
pdf("scatter1.pdf")
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "Logged spending difference between incumbent and challenger",
     ylab = "Incumbent vote share",
     main = "Spending difference and incumbent vote share",
     pch = 18, col = "grey40")   # smaller dots are easier to read  
# abline() draws the fitted line from the regression model above
abline(model1, col = "red", lwd = 2)
dev.off()

# 1(c). Save the residuals of the model in a sep object
# Residuals are the observed voteshare - the model's predicted value (the part 
# of the voteshare that difflog doesn't explain)
model1_resid <- residuals(model1)

# 1(d). Write the prediction equation
# extract the intercept and slope to write the equation. Round to 3 dec points.
round(coef(model1), 3)

#####################
# Question 2
#####################

# 2(a). Regression where outcome is presvote and explanatory is difflog.
# presvote = voteshare of the pres candidate from the incumbent's party (0-1)
summary(inc.sub$presvote)

# same predictor as Q1, but now the outcome is presidential voteshare
model2 <- lm(presvote ~ difflog, data = inc.sub)

# texreg() prints the regression table as LaTeX code
texreg(model2, digits = 3, caption = "Regression of presvote on difflog")

# 2(b). Scatterplot with the fitted regression line
pdf("scatter2.pdf")
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "Logged spending difference between incumbent and challenger",
     ylab = "Presidential candidate vote share (incumbent party)",
     main = "Spending difference and presidential vote share",
     pch = 18, col = "grey40")
# add the fitted line from model2
abline(model2, col = "green", lwd = 2)
dev.off()

# 2(c). Save the residuals of the model in a sep object
# Residuals are the observed presvote - the model's predicted value (the part 
# of the presidential vote that difflog doesn't explain)
model2_resid <- residuals(model2)

# 2(d). Write the prediction equation
# extract the intercept and slope to write the equation. Round to 3 dec points.
round(coef(model2), 3)

#####################
# Question 3
#####################

# 3(a). Regression voteshare (outcome) on presvote (predictor)
# outcome = incumbent's vote share again (like in Q1),
# now the predictor is the presidential candidate's vote share
model3 <- lm(voteshare ~ presvote, data = inc.sub)

# texreg() prints the regression table as LaTeX code
texreg(model3, digits = 3, caption = "Regression of voteshare on presvote")

# 3(b). Scatterplot with the fitted regression line
pdf("scatter3.pdf")
plot(inc.sub$presvote, inc.sub$voteshare,
     xlab = "Presidential candidate vote share (incumbent party)",
     ylab = "Incumbent vote share",
     main = "Presidential vote share and incumbent vote share",
     pch = 18, col = "grey40")
# add the fitted line from model3
abline(model3, col = "red", lwd = 2)
dev.off()

# 3(c). Write the prediction equation
# extract the intercept and slope to write the equation. Round to 3 dec points.
round(coef(model3), 3)

#####################
# Question 4
#####################

# 4(a). Regression where outcome is residuals from Q1, and explanatory is
# residuals from Q2.

# model1_resid = part of the voteshare not explained by difflog (Q1)
# model2_resid = part of the presvote not explained by difflog (Q2)
# Essentially, both models "removed" the influence of spending

# no data = argument needed because they're not columns of inc.sub
model4 <- lm(model1_resid ~ model2_resid)

# texreg() prints the regression table as LaTeX code
texreg(model4, digits = 3, caption = "Regression of Q1 residuals on Q2 residuals")

# 4(b). Scatterplot of the two residual sets with the fitted reg line
# Added variable plot:
pdf("scatter4.pdf")
plot(model2_resid, model1_resid,
     xlab = "Q2 Residuals (presvote on difflog)",
     ylab = "Q1 Residuals (voteshare on difflog)",
     main = "Added variable plot for presvote",
     pch = 18, col = "grey40")
# add the fitted line from model4
abline(model4, col = "purple", lwd = 2)
dev.off()

# 4(c). Prediction equation
# The intercept and slope, rounded to 3 decimals
round(coef(model4), 3)


#####################
# Question 5
#####################

# 5(a). Multiple regression for voteshare on difflog and presvote
# using "+" adds a second predictor variable, so each coefficient is the
# association with voteshare, and holding the other predictor constant
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# texreg() prints the regression table as LaTeX code
texreg(model5, digits = 3, caption = "Regression of voteshare on difflog and presvote")

# 5(b). Write the prediction equation
# using intercept and both slopes, rounded to 3 decimal points
round(coef(model5), 3)

# 5(c). What is identical to the output in Q4?

# Comparing standard errors: column 2 of the coefficient table
# rounded to 3 decimals (same as the residual check), these should match
se_model5 <- round(summary(model5)$coefficients["presvote", 2], 3)
se_model4 <- round(summary(model4)$coefficients["model2_resid", 2], 3)
c(se_model5, se_model4)
se_model5 == se_model4 # TRUE = they match

# Looking at residuals from both models (should be identical)
res_model5 <- round(residuals(model5), 3) #round to avoid computational decimal issues
res_model4 <- round(residuals(model4), 3)
all(res_model5 == res_model4) # True means every residual matches

# Main comparison between presvote slope in Q5 vs residual slope in Q4
# [3] is the third coefficient in q5 (intercept, difflog, presvote)
# [2] is the second coefficient in q4 (intercept, model2_resid)
slope_model5 <- coef(model5)[3]
slope_model4 <- coef(model4)[2]
slope_model5
slope_model4
round(slope_model5, 3) == round(slope_model4, 3) # True means they match

