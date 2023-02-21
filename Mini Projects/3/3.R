# Question 1 - a,b
#  Returns MLE and MOM for a sample
getMLEandMOM <- function(n, theta) {
  sample = runif(n, min = 0, max = theta)
  mom = 2 * mean(sample)
  mle = max(sample)
  return(c(mle, mom))
}

# Returns Mean squared Error of MLE and MOM for 1000 replications
getMSE <- function(n, theta) {
  values = replicate(1000, getMLEandMOM(n, theta))
  values = (values - theta)^2
  values.mom = values[c(TRUE, FALSE)]
  values.mle = values[c(FALSE, TRUE)]
  
  return(c(mean(values.mle), mean(values.mom)))
} 

mse_1_1 = getMSE(1,1)

# Question 1 - c

n = c(1,2,3,5,10,30)
theta = c(1,5,50,100)

mse <- list() 
for(x in n) {
  for(y in theta){
    mse = append(mse, list(getMSE(x,y)))
  }
}

# Drawing graphs for fixed 'n' and varying 'theta'
par(mfrow = c(3,2))
t = 1

for(x in n) {
  msel1 = list()
  msel2 = list()
  for(y in 0:3){
    msel1 = append(msel1, unlist(mse[t + y])[1])
    msel2 = append(msel2, unlist(mse[t + y])[2])
  }
  
  plot(theta, c(unlist(msel1)), type="b", xlab="theta", ylab="MSE", 
       col="red", main= paste("n=", x))
  lines(theta, c(unlist(msel2)), type="b", col="blue")
  legend("topleft", legend = c("MLE", "MOM"), col = c("red", "blue"), 
         text.col = c("black", "black"), lty = 1, pch = 1, 
         inset = 0.01, ncol = 1, cex = 0.6, bty = 'n')
  t = t + 4
}

# Drawing graphs for fixed 'theta' and varying 'n'
par(mfrow = c(2,2))
t = 1

for(x in theta) {
  msel1 = list()
  msel2 = list()
  for(y in 0:5){
    msel1 = append(msel1, unlist(mse[t+4*y])[1])
    msel2 = append(msel2, unlist(mse[t+4*y])[2])
  }
  
  plot(n, c(unlist(msel1)), type="b", xlab="n", ylab="MSE", 
       col="red", main= paste("theta=", x))
  lines(n, c(unlist(msel2)), type="b", col="blue")
  legend("topleft", legend = c("MLE", "MOM"), col = c("red", "blue"), 
         text.col = c("black", "black"), lty = 1, pch = 1, 
         inset = 0.01, ncol = 1, cex = 0.6, bty = 'n')
  t = t + 1
}

# Question-2 c

# Returns neg value of the derived function
negativeMLE <- function(par, dat) {
  print(par,dat)
  result = length(dat) * log(par) - (par+1) * sum(log(dat))
  return(-result)
}

x <- c(21.42, 14,65, 50.42, 28.78, 11.23)

mle <- optim(par = 2, fn = negativeMLE, method = "L-BFGS-B", 
             hessian = TRUE, lower = 0.01, dat = x)

# d

# Finding Standard Error
se <- (1/mle$hessian)^0.5

# Finding Confidence Interval
conf = mle$par + c(-1,1)*se*qnorm(0.975)

se
conf

 