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
lapply(c("stargazer", "coefplot","arm","emmeans","ggplot2", "interflex"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Agenda
# (1) Regression analysis
# (2) F test for some coefficient
# (3) Interactions
# (4) Marginal effects

# Research questions: 
# What is the relationship between education and Euroscepticism?

# Load data
df <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/refs/heads/main/datasets/ess_euroscepticism.csv")
View(df)

# Convert into factor variable
df$edu_cat <- factor(df$edu_cat)
is.factor(df$edu_cat)

# (1) Regression analysis ----------

# Complete case analysis
df_na <- df[complete.cases(df), ] 

# Categorical independent variable (manually)

# Create dummy variables 
df_na$JuniorCycle <- ifelse(df_na$edu_cat == "Junior Cycle", 1, 0)
df_na$LeavingCertificate <- ifelse(df_na$edu_cat == "Leaving Certificate", 1, 0)
df_na$AdvancedCertificate <- ifelse(df_na$edu_cat == "Advanced Certificate", 1, 0)
df_na$Bachelor <- ifelse(df_na$edu_cat == "Bachelor Degree", 1, 0)
df_na$Postgraduate <- ifelse(df_na$edu_cat == "Postgraduate Degree", 1, 0)

# Fit model (ref=JuniorCycle)
model <- lm(euftf_re~LeavingCertificate+AdvancedCertificate+Bachelor+Postgraduate,data=df_na)
summary(model)

# Change reference category to leaving certificate 
model <- lm(euftf_re~JuniorCycle+AdvancedCertificate+Bachelor+Postgraduate,data=df_na)
summary(model)

# Education--Categorical independent variable
is.factor(df_na$edu_cat)
model1 <- lm(euftf_re~edu_cat,data=df_na)
summary(model1)

# Change reference category to leaving certificate 
levels(df_na$edu_cat) # First level, "Advanced Certificate"
df_na$edu_cat <- relevel(df_na$edu_cat, ref = 4)
levels(df_na$edu_cat) # First level, "Leaving Certificate"

# Refit model
model1 <- lm(euftf_re~edu_cat,data=df_na)
summary(model1)

# Education--Continuous independent variable
model1 <- lm(euftf_re~eduyrs,data=df_na)
summary(model1)

# Add economic dimension
model_eco <- lm(euftf_re~eduyrs + hinctnta,data=df_na)
summary(model_eco)

# Add political dimension
model_pol <- lm(euftf_re~eduyrs + hinctnta + trstplt, data=df_na)
summary(model_pol)

# Add cultural dimension
model_cul <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt, data=df_na)
summary(model_cul)

# Add socio-economic variables 
model_final <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt + gndr + agea + brncntr, data=df_na)
summary(model_final)

# Get Latex table
stargazer(model1,model_eco,model_pol,model_cul,model_final)

# How to visualize results?
coefplot::multiplot(
  list("Model 1: Education Only" = model1,
       "Model 5: Final Model" = model_final),
  intercept = FALSE
)

# F test for some coefficients -------

# Does adding economic dimension improve fit?
anova(model1, model_eco, test='F')
summary(model_eco)

# In which cases is partial F test not equal to t-test?
# Answer: A partial F-test is not equal to a t-test when it tests more than one coefficient at once

# Does adding political dimension improve fit?
anova(model1, model_pol, test='F')
summary(model_pol)

# What about political dimension alone?
model3 <- lm(euftf_re~eduyrs+trstplt,data=df_na) 
anova(model1, model3, test='F')
summary(model3)

# Does adding the education dummy set improve fit?
model5 <- lm(euftf_re~hinctnta + trstplt + imwbcnt, data=df_na)
model6 <- lm(euftf_re~hinctnta + trstplt + imwbcnt + edu_cat, data=df_na)
anova(model5, model6, test='F')
summary(model6)

# In conclusion: When to use partial F test?
# Answer: Use a partial F-test when you want to test whether a group of two or more coefficients is jointly significant, 
# or whether adding a set of predictors significantly improves a regression model. 
# It compares a full model with a simpler, nested reduced model.

# (3) Interactions ------------

# So far, visualizing regression models with categorical independent variables 

# Fit model
model1 <- lm(euftf_re~edu_cat+imwbcnt,data=df_na)

# Plot
plot(df_na$imwbcnt, df_na$euftf_re, ylab = 'Eurosceptism', xlab = 'Cultural Dimension')
abline(model1$coefficients[1], model1$coefficients[6],col="black") # edu_cat=0, "Leaving Certificate"
abline(model1$coefficients[1]+model1$coefficients[2], model1$coefficients[6],col="blue") # edu_cat=1, "Advanced Certificate"
abline(model1$coefficients[1]+model1$coefficients[3], model1$coefficients[6],col="green") # edu_cat=2, "Bachelor"
abline(model1$coefficients[1]+model1$coefficients[4], model1$coefficients[6],col="gray") # edu_cat=3, "Junior Cycle"
abline(model1$coefficients[1]+model1$coefficients[5], model1$coefficients[6],col="orange") # edu_cat=4, "Postgraduate"
legend("topright",
       legend=c("Leaving Certificate","Advanced Certificate","Bachelor","Junior Cycle","Postgraduate"),
       col=c("black","blue","green","gray","orange"),
       pch=1) 

# In ggplot
ggplot(df_na, aes(imwbcnt, euftf_re, group = edu_cat)) +
  geom_point(aes(colour = edu_cat)) + # Add points
  geom_line(data = df_na, aes(y = model1$fitted.values, colour = edu_cat)) + # Add regression lines
  labs(y= 'Eurosceptism', x = 'Cultural Dimension', colour="Education")

# A. Add interaction term between, attitudes towards immigration and gender
# (categorical x continuous)

# Recode dummy variables as 0,1; needed for interactions
df_na$gndr <- ifelse(df_na$gndr == 2, 1, 0)

# And convert into factor
df_na$gndr <- factor(df_na$gndr, labels = c("Male", "Female"))

# Fit model
model_int <- lm(euftf_re~eduyrs + 
                         imwbcnt + 
                         gndr + 
                         imwbcnt*gndr, data=df_na)
summary(model_int)

# What is the prediction equation?
# Answer: Y_hat_i =
#   beta_0_hat
#   + beta_1_hat * Education_i
#   + beta_2_hat * Immigration_Attitudes_i
#   + beta_3_hat * Gender_i
#   + beta_4_hat * (Gender_i * Immigration_Attitudes_i)

# How to interpret the intercept?
# Answer: Predicted outcome for men whose education and immigration-attitudes scores are both zero

# How to interpret the coefficient for education?
# Answer: Conditional effect of education for men with an immigration-attitudes score of 0

# How to interpret the coefficient for attitudes towards immigration?
# Answer: Conditional effect of immigration attitudes for men with an education score of 0

# How to interpret the coefficient for gender?
# Answer: Difference in predicted value of Y between women and men when Education = 0 and Immigration attitudes = 0

# How to interpret the interaction term?
# Answer: Tells us how the effect of gender on Y changes as immigration attitudes change

# Get slopes for levels of categorical variables
summary(model_int)$coefficients[3]
summary(model_int)$coefficients[3]+summary(model_int)$coefficients[5]

# Or use emmeans package
emtrends(model_int, ~ gndr, var="imwbcnt")

# Visualize estimated marginal means (EMMs)
emmip(model_int, 
      gndr ~ imwbcnt,
      at=list(imwbcnt=seq(0,10,by=1), gndr=c("Male","Female"), eduyrs=mean(df_na$eduyrs)),
      plotit = TRUE, 
      CIs = TRUE)

# What does this function do?
emmip(model_int, 
      gndr ~ imwbcnt,
      at=list(imwbcnt=seq(0,10,by=1), gndr=c("Male","Female"), eduyrs=mean(df_na$eduyrs)),
      plotit = FALSE, 
      CIs = TRUE)

# We can also use predict
predict(model_int, 
        newdata=data.frame(imwbcnt=0,gndr="Male",eduyrs=mean(df_na$eduyrs)))

# Prediction equation from model with interaction term
#  7.144123+0.008914*Education-0.467063*Attitudes-1.111747*Gender+0.169874*Attitudes*Gender

# Calculate the marginal mean, for men, with very negative attitudes towards immigration, and mean education level.
# Answer: 
# - Gender = 0 for men
# - Gender = 1 for women
# - Education is held at its sample mean.
# - Attitudes is held at the value representing very negative
#   attitudes towards immigration.
#
# Because we are calculating the marginal mean for men, we set:
#
# Gender = 0
#
# Substituting Gender = 0 into the prediction equation gives:
#
# Y_hat_men =
#   7.144123
#   + 0.008914 * Mean_Education
#   - 0.467063 * Very_Negative_Attitudes
#   - 1.111747 * 0
#   + 0.169874 * (Very_Negative_Attitudes * 0)
#
# Therefore:
#
# Y_hat_men =
#   7.144123
#   + 0.008914 * Mean_Education
#   - 0.467063 * Very_Negative_Attitudes
#
# The gender coefficient and the interaction term disappear because Gender = 0 for men.

# What is the marginal effect of changing gender from male to female, in this scenario? 
# Answer: Marginal effect of changing gender from male to female is: -1.111747 + 0.169874 * Very_Negative_Attitudes

# B. Add interaction term between, education and whether the person was born in country
# (categorical x categorical)

# Recode dummy variables as 0,1; needed for interactions
df_na$brncntr <- ifelse(df_na$brncntr == 2, 1, 0)

# And convert into factor
df_na$brncntr <- factor(df_na$brncntr, labels = c("Born in country", "Not born in country"))

# Fit model
model_int2 <- lm(euftf_re~edu_cat + 
                          imwbcnt + 
                          brncntr +
                          edu_cat*brncntr, data=df_na)
summary(model_int2)

# Visualize estimated marginal means (EMMs)
emmip(model_int2, 
      brncntr ~ edu_cat,
      at=list(edu_cat=c("Leaving Certificate","Advanced Certificate","Bachelor Degree","Junior Cycle","Postgraduate Degree"), 
              brncntr=c("Born in country", "Not born in country"), imwbcnt=mean(df_na$imwbcnt)),
      plotit = TRUE, 
      CIs = TRUE)

# Make your own visualization in ggplot
emm <- emmip(model_int2, 
             brncntr ~ edu_cat,
             at=list(edu_cat=c("Leaving Certificate","Advanced Certificate","Bachelor Degree","Junior Cycle","Postgraduate Degree"), 
                     brncntr=c("Born in country", "Not born in country"), imwbcnt=mean(df_na$imwbcnt)),
             plotit = FALSE, 
             CIs = TRUE)

# Build plot
plot <- ggplot(data=emm, aes(x=edu_cat, y=yvar, fill=brncntr)) + 
  geom_bar(stat="identity",position="dodge") +
  geom_errorbar(position=position_dodge(.9),width=.25, aes(ymax=UCL, ymin=LCL),alpha=0.3) + # Add error bars
  ylab("Predicted level of Euroscepticism") +
  xlab("Education levels") +
  labs(fill = "Regime type")
plot  

# Partial F test for interactions
# Does the interaction effect improve fit?
model_no_int <- lm(euftf_re~edu_cat + imwbcnt + brncntr, data=df_na)
anova(model_no_int, model_int2)

# Interflex package

# Use interflex with the euroscepticism data 
# Let's examine the interaction between education (eduyrs) and income (hinctnta) 
# on Euroscepticism (euftf)

# Fit an interflex model: D = eduyrs (treatment), X = hinctnta (moderator), Y = euftf (outcome)
# Using linear estimator to match previous interaction model
interflex_eurosc_linear <- interflex(
  estimator = "linear",
  Y = "euftf_re",
  D = "eduyrs",
  X = "hinctnta",
  data = df_na
)

# Visualize marginal effects of education at different levels of income
plot.interflex(interflex_eurosc_linear,
               main = "Marginal Effect of Education on Euroscepticism\nby Income Level",
               Xlabel = "Household Income", 
               Ylabel = "Marginal Effect of Education (years)", 
               cex.main = 1)

# You can also use "binning" estimator for comparison:
interflex_eurosc_binning <- interflex(
  estimator = "binning",
  Y = "euftf_re",
  D = "eduyrs",
  X = "hinctnta",
  data = df_na
)

plot.interflex(interflex_eurosc_binning,
               main = "Marginal Effect of Education (Binning)\non Euroscepticism by Income",
               Xlabel = "Household Income", 
               Ylabel = "Marginal Effect of Education (years)",
               cex.main = 1)