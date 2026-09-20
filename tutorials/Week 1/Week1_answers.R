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

# create vector y
y <- c(0, 4, 4, 5, 7, 10)
# (1) find sum of y
sum(y)

# (2) find mean of y
sum(y)/length(y)
# same thing, but faster
mean(y)

# (3) find sum of demeaned values
# substract mean from each y_i
# first, I'll create a vector to fill
# there is nothing in there when I create an object
# like x <- NULL
demeanedSum <- NULL
# remember that [i] is indexing each element of y
# I'm basically saying, for each element in all of the elements in y
# take that y_i and subtract the mean of y
# and store that value in my vector of demeaned values for each y_i
for(i in 1:length(y)){
  demeanedSum[i] <- y[i] - mean(y) 
}
# the simple way (R figures it out)
demeanedSumSimple <- y - mean(y)
# now we take the sum of all those demeaned values
sum(demeanedSumSimple)

# (4) sum of squared error
squaredError <- demeanedSum^2
sum(squaredError)

# quantiles
# create vector
quantilesVec <- c(55, 84, 65, 54, 61, 67, 80, 59, 81, 82)
# put the values in order to see which is median
sort(quantilesVec)

median(quantilesVec)
# different quantiles formulas
summary(quantilesVec)
# type 1
quantile(quantilesVec, c(0.25, 0.5, 0.75), type = 1)

# reminder of how to:
# making a histogram
# find state median income
state.x77[,2]
# create histogram
pdf("medianIncomeHist.pdf")
hist(state.x77[,2], main="Distribution of state median income", xlab="Median income")
dev.off()
