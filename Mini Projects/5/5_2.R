checkzci <- function(n, lambda) {
  U <- rexp(n,lambda)
  lb <- mean(U) - qnorm(0.975) * sd(U) / sqrt(n)
  ub <- mean(U) + qnorm(0.975) * sd(U) / sqrt(n)
  tm = 1/lambda
  if(ub > tm & lb < tm) {
    return (1)
  }
  else {
    return (0)
  }
} 

zproportion <- function(n, lambda) {
  values <- replicate(5000, checkzci(n, lambda))
  ones <- values[which (values == 1)]
  return (length(ones)/5000)
}

mean.star <- function(n,lambda) {
  u.star <- rexp(n, lambda)
  return (mean(u.star))
} 

checkbci <- function(n, lambda) {
  U <- rexp(n,lambda)
  tm <- 1/lambda
  lambda1 = 1/mean(U)
  V <- replicate(1000, mean.star(n,lambda1))
  bound <- sort(V)[c(25, 975)]
  if(bound[2] > tm & bound[1] < tm) {
    return (1)
  }
  else {
    return (0)
  }
}

bproportion <- function(n, lambda) {
  values <- replicate(5000, checkbci(n, lambda))
  ones <- values[which (values == 1)]
  return (length(ones)/5000)
} 

# Calculating z-interval and bootstrap interval for n = 5 and lambda = 0.01
zproportion(5, 0.01)
bproportion(5, 0.01)

# Repeating same process for all n, Lambda values
nVals = c(5, 10, 30, 100)
lambdaVals = c(0.01, 0.1, 1, 10)
zprops = c()
bprops = c()

for(l in lambdaVals) {
  for(n in nVals)  {
    zprops <- c(zprops, zproportion(n, l))
    bprops <- c(bprops, bproportion(n, l))
  }
}

zciMatrix = matrix(zprops, nrow = 4, ncol = 4)
bciMatrix = matrix(bprops, nrow = 4, ncol = 4)

  
par(mar=c(2,2,2,2), mfrow = c(2,2))
for(x in 1:4) {
  plot(nVals, zciMatrix[,x], main = paste("L = ", lambdaVals[x]), 
       xlab = 'n', ylab ='Proportions', col = 'red', type = 'b', 
       xlim = c(1,100), ylim = c(0.8,1)
  )
  lines(nVals, bciMatrix[,x], col = 'blue', type = 'b')
}
 
par(mar=c(2,2,2,2), mfrow = c(2,2))
for(x in 1:4) {
  plot(lambdaVals, zciMatrix[x,], main = paste("N = ", nVals[x]), 
       xlab = 'Lambda', ylab ='Proportions', col = 'red', type = 'b', 
       xlim = c(0.01,10), ylim = c(0.8,1)
  )
  lines(lambdaVals, bciMatrix[x,], col = 'blue', type = 'b')  
}


