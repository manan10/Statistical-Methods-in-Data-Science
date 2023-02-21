# Reading the data from the CSV file
vapor <- read.csv("E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/4/VAPOR.csv")

# Drawing qqplots
par(mfrow = c(1,2))
qqnorm(vapor$theoretical, main = "Theoretical")
qqline(vapor$theoretical)
qqnorm(vapor$experimental, main = "Experimental")
qqline(vapor$experimental)

# Drawing the boxplot
par(mfrow = c(1,1))
boxplot(vapor$theoretical, vapor$experimental, 
        names = c("Theoretical", "Experimental"),
        main = "Boxplot of Theoretical/Experimental Readings")

# Calculatating summaries of dataset
summary(vapor$theoretical)
summary(vapor$experimental)

# Calculating Mean, Standard deviation, 
# t(n-1) val, and confidence interval
vapor.diff = vapor$theoretical - vapor$experimental

mean(vapor.diff)
sd(vapor.diff)
qt(0.975, 15)
mean(vapor.diff) + c(-1,1) * qt(0.975, 15) * sd(vapor.diff)/4

#Confidence interval using t test
t.test(vapor$theoretical, vapor$experimental, 
       alternative= "two.sided", paired = TRUE, 
       var.equal = FALSE, conf.level = 0.95)

