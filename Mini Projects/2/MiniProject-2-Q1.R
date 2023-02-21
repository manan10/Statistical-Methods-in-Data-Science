
## Question-1
# Part - a

runnerData = read.csv(
  'E:/MS-CS/Spring 22/CS6313 - SMDS/Mini Projects/2/roadrace.csv'
)

runnersFromMaine = runnerData$Maine ==  'Maine'
runnersFromAway = runnerData$Maine == 'Away'

barplot(c(sum(runnersFromMaine), sum(runnersFromAway)), 
        names.arg = c('Maine', 'Away'), 
        space = 0.25, 
        ylab = "Number of Runners")

print(sum(runnersFromMaine))
print(sum(runnersFromAway))


# Part-b

runnerTimeMaine = runnerData$Time..minutes.[which(runnersFromMaine)]
runnerTimeAway = runnerData$Time..minutes.[which(runnersFromAway)]

hist(runnerTimeMaine, 
     xlim = range(0, 200), 
     ylim = range(0,2000), 
     main = "Maine Runners Time(Minutes)", 
     xlab="Time(Minutes)")

hist(runnerTimeAway, 
     xlim = range(0, 200), 
     ylim = range(0,2000), 
     main = "Away Runners Time(Minutes)", 
     xlab="Time(Minutes)")

summary(runnerTimeMaine)
IQR(runnerTimeMaine)
range(runnerTimeMaine)
sd(runnerTimeMaine)

# Part - c

boxplot(runnerTimeMaine, 
        runnerTimeAway, 
        names = c('Maine', 'Away')
)

# Part - D

ageMales = strtoi(runnerData$Age[which(runnerData$Sex == 'M')])
ageFemales = strtoi(runnerData$Age[which(runnerData$Sex == 'F')])

boxplot(ageMales, 
        ageFemales, 
        names = c('Male Runners Age','Female Runners Age'),
        ylim = range(0,100)
)

summary(ageMales)
IQR(ageMales)
range(ageMales)
sd(ageMales)

summary(ageFemales)
IQR(ageFemales)
range(ageFemales)
sd(ageFemales)

