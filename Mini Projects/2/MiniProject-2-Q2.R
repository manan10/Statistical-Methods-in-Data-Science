# Reading data
motorcycleData = read.csv(
  'E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/2/motorcycle.csv'
)

# Retriving all fatal accidents
fatalAccidents = motorcycleData$Fatal.Motorcycle.Accidents

# Creating boxplot for fatal accidents
boxplot(fatalAccidents,
        xlab="Fatal Motorcycle Accidents",
        ylab="Number of Accidents"
)

# Calculating the lower bound of Fatal Accidents
lowerBound = max(
  quantile(fatalAccidents, prob=0.25) - 1.5*IQR(fatalAccidents),
  min(fatalAccidents)
)

# Calculating the upper bound of Fatal Accidents
upperBound = min(
  quantile(fatalAccidents, prob=0.75) + 1.5*IQR(fatalAccidents),
  max(fatalAccidents)
)

# Retriving countys that may be outliers
outlierCounties = motorcycleData$County[which(
  motorcycleData$Fatal.Motorcycle.Accidents < lowerBound |
  motorcycleData$Fatal.Motorcycle.Accidents > upperBound
)]

outlierCounties

# Generating relevant statistics

summary(fatalAccidents)
IQR(ageFemales)
range(ageFemales)
sd(ageFemales)

