# Reading the data from the CSV file
voltage <- read.csv("E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/4/VOLTAGE.csv")

# Separating datasets by location
voltage.remote = voltage$voltage[which(voltage$location == 0)]
voltage.local = voltage$voltage[which(voltage$location == 1)]

# Drawing boxplot
boxplot(voltage.local, voltage.remote, 
        names = c("Local Location", "Remote Location"), 
        main = "Boxplot of voltage at Local/Remote Locations", 
        range = 1.5)

# Drawing qqplots 
par(mfrow = c(1,2))
qqnorm(voltage.local, main = "Local")
qqline(voltage.local)
qqnorm(voltage.remote, main = "Remote")
qqline(voltage.remote)

# Calculating summaries
summary(voltage.local)
summary(voltage.remote)

# Calculate mean, variance, standard error and confidence interval
meanLocal <- mean(voltage.local)
meanRemote <- mean(voltage.remote)

varLocal <- var(voltage.local)
varRemote <- var(voltage.remote)

se <- sqrt((varLocal + varRemote)/30)
diff <- meanRemote - meanLocal

ci <- diff + c(-1,1) * qnorm(0.975) * se

# Calculate confidence interval using t test
t.test(voltage.remote, voltage.local, 
       alternative = "two.sided", paired = FALSE, 
       var.equal = FALSE, conf.level = 0.95)


