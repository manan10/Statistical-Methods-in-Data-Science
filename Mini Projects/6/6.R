# Reading the file
cancerData = read.csv("E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/6/prostate_cancer.csv") 

# Getting names of all columns
names = colnames(cancerData)

names
summary(cancerData)
cor(cancerData)

# Using log to scale PSA column 
logPSA = log(cancerData$psa)

# Visualization of the data
boxplot(cancerData$psa)

# Checking the distributions of age and psa
plot(cancerData$psa,cancerData$age)

# finding relationship between the predictor 'subject' and PSA
plot(cancerData$subject, logPSA)
fitSubject <- lm(logPSA ~ cancerData$subject, data = cancerData)
abline(fitSubject)
summary(fitSubject)

# finding relationship between the predictor 'cancervol' and PSA
plot(cancerData$cancervol, logPSA)
fitCancervol <- lm(logPSA ~ cancerData$cancervol, data = cancerData)
abline(fitCancervol)
summary(fitCancervol)

# finding relationship between the predictor 'weight' and PSA
plot(cancerData$weight, logPSA)
fitWeight <- lm(logPSA ~ cancerData$weight, data = cancerData)
abline(fitWeight)
summary(fitWeight)

# finding relationship between the predictor 'age' and PSA
plot(cancerData$age, logPSA)
fitAge <- lm(logPSA ~ cancerData$age, data = cancerData)
abline(fitAge)
summary(fitAge)

# finding relationship between the predictor 'benpros' and PSA
plot(cancerData$benpros, logPSA)
fitBenpros <- lm(logPSA ~ cancerData$benpros, data = cancerData)
abline(fitBenpros)
summary(fitBenpros)

# finding relationship between the predictor 'capspen' and PSA
plot(cancerData$capspen, logPSA)
fitCapspen <- lm(logPSA ~ cancerData$capspen, data = cancerData)
abline(fitCapspen)
summary(fitCapspen)

# finding relationship between the predictor 'gleason' and PSA
plot(cancerData$gleason, logPSA)
fitGleason <- lm(logPSA ~ cancerData$gleason, data = cancerData)
abline(fitGleason)
summary(fitGleason)

# finding relationship between the predictor 'vesinv' and PSA
vesinv = factor(cancerData$vesinv)
plot(vesinv, logPSA)
fitVesinv <- lm(logPSA ~ vesinv, data = cancerData)
abline(fitVesinv)
summary(fitVesinv)

# Creating various models by combining multiple significant predictors
fit1 = lm(logPSA ~ cancerData$cancervol + cancerData$gleason + factor(cancerData$vesinv) 
          + cancerData$capspen , data = cancerData)
summary(fit1)

# Removing capspen 
fit2 = lm(logPSA ~ cancerData$cancervol + cancerData$gleason + factor(cancerData$vesinv), 
          data = cancerData) 
summary(fit2)

# Comparing the two models
anova(fit2,fit1)

# Creating model with all predictors
fit3 = lm(logPSA ~ cancerData$cancervol + factor(cancerData$vesinv) + cancerData$benpros + 
            cancerData$gleason, data = cancerData )
summary(fit3)

# Creating residual plot for fit3
plot(fitted(fit3), resid(fit3))
abline(h = 0)

# Creating QQ plot for fit3
qqnorm(resid(fit3))
qqline(resid(fit3))

# Using fit3 to predict desired output.
# Computing means of all quantitative predictors
meanCV = mean(cancerData$cancervol)
meanGL = mean(cancerData$gleason)
meanBP = mean(cancerData$benpros)

# Computing frequency count of all qualitative predictors
mfVesinv = names(which.max(table(factor(cancerData$vesinv))))

# Computing the PSA value
beta0 = -0.65013
beta1 = 0.06488
beta2 = 0.68421
beta3 = 0.09136
beta4 = 0.33376 

predAns = exp(beta0 + beta1*meanCV + beta2*0 + beta3*meanBP + beta4*meanGL)


