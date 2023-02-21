max(rexp(1,0.1), rexp(1,0.1))
#let draw be 10000 replications
draw = 100000
#to repeat the step 10000 times we can use replicate function.
#here we are storing the 10000 draws in the variable T
T = replicate(draw,max(rexp(1,0.1),rexp(1,0.1)))
# Here we used hist() and curve() functions to represent the data graphically of draws T
hist(T, probability = TRUE)
curve(0.2*exp(-0.1*x)-0.2*exp(-0.2*x), col = "darkBlue",  add = TRUE)
meanOfT <- mean(T)
P <- length(T[which(T>15)])/length(T)