# Question - 1

# Importing the boot library
library(boot)

# Reading the data
gpaData = read.csv("E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/4/gpa.csv")

# Seperating the data
gpaVals = as.numeric(gpaData$gpa)
actVals = as.numeric(gpaData$act)

# Plotting the data on a scatter-plot
plot(gpaVals, actVals, main = "Scatterplot of GPA and ACT Scores",
     xlab = "GPA", ylab = "ACT")
abline(lm(actVals ~ gpaVals))

# Calculating correlation
corr = cor(gpaVals, actVals)

# Creating a statistic function for correlation
covarience.npar <- function(gpa, indexes) {
  xgpa <- gpa$gpa[indexes]
  xact <- gpa$act[indexes]
  result <- cor(xgpa, xact)
  return(result)
}

# Executing the statistical function
covarience.npar.boot <- boot(gpaData, covarience.npar, R = 999,
                            sim = "ordinary", stype = "i")

# Point Estimation of the bootstap value
pEst <- mean(covarience.npar.boot$t)

# Getting the confidence interval
ci <- boot.ci(covarience.npar.boot)

# Verifying confidence interval by calculating quantiles
sort(covarience.npar.boot$t)[c(25, 975)]
covarience.npar.boot