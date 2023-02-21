# Reading data
heartRateData = read.csv("E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/5/bodytemp-heartrate.csv") 

# Separting the database based on gender
maleData = subset(heartRateData, heartRateData$gender == 1)
femaleData = subset(heartRateData, heartRateData$gender == 2)

#Section - a
# Drawing boxplots to depict Body Temperatures
boxplot(maleData$body_temperature, femaleData$body_temperature, 
  main = "Boxplots of Body Temperatures", 
  names = c('Males', 'Females'), ylab = "Temperatures"
) 

# Drawing QQPlots for Body Temperature
par(mfrow=c(1,2)) 
qqnorm(maleData$body_temperature, main = 'Q-Q Plot for Males') 
qqline(maleData$body_temperature)
qqnorm(femaleData$body_temperature, main = 'Q-Q Plot for Females')
qqline(femaleData$body_temperature)

# Calculating CI using t-test function for the body temperature values
t.test(maleData$body_temperature, femaleData$body_temperature, 
      alternative = 'two.sided', var.equal = F) 

#Section - b
# Drawing boxplots to depict Heart Rates
boxplot(maleData$heart_rate, femaleData$heart_rate, 
        main = "Boxplots of Heart Rates", 
        names = c('Males', 'Females'), ylab = "Heart Rates"
)

# Drawing QQPlots for Heart Rates
par(mfrow=c(1,2)) 
qqnorm(maleData$heart_rate, main = 'Q-Q Plot for Males') 
qqline(maleData$heart_rate)
qqnorm(femaleData$heart_rate, main = 'Q-Q Plot for Females')
qqline(femaleData$heart_rate)

# Calculating CI using t-test function for the Heart Rates values
t.test(maleData$heart_rate, femaleData$heart_rate, 
       alternative = 'two.sided', var.equal = F)

#Section - c
# Finding the correlation values between body temperatures and heart rates 
cor(maleData$body_temperature, maleData$heart_rate)
cor(femaleData$body_temperature, femaleData$heart_rate)

#drawing the scatter plots for the body temperature and heart rate values for males and females
par(mfrow=c(1,2))
plot(maleData$heart_rate, maleData$body_temperature, pch=1, 
     main='Scatter Plot (Males)', 
     xlab = "Heart Rates(Males)", ylab="Body Temperature(Male)")
abline(lm(maleData$body_temperature~maleData$heart_rate))
plot(femaleData$heart_rate, femaleData$body_temperature, pch=1, 
     main='Scatter Plot (Females)',
     xlab = "Heart Rates(Females)", ylab="Body Temperature(Female)")
abline(lm(femaleData$body_temperature~femaleData$heart_rate))

