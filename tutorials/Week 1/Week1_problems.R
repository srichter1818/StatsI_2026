# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

#############
# Basic stats
#############

# create vector y
y <- c(0, 4, 4, 5, 7, 10)

# (1) find sum of y using the built-in R function
sumy <- sum(y)
sumy

# (2) find mean of y using your "own" function
sum(y)/length(y)

# now do the same thing, but faster using the built-in R function
mean(y)

# (3) find sum of demeaned values
deviations <- y - mean(y)
square_dev <- deviations^2
square_dev
n <- length(y)
sum(square_dev) / (n-1)

#or
sd(y)

# (4) calculate sum of squared error

###########
# Quantiles
###########

# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)

# (1) calculate median 

# (2) calculate quantiles

# (3) make a histogram of state median income
state.x77[,2]

# remember to save your plot as a pdf