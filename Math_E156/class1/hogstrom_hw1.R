# Larson Hogstrom - HW1
# MATH_E156 - 1/30/2014


### Tutorial 1
fd <- read.csv("/Users/hogstrom/Documents/code/ModernKicks/Math_E156/extra_datasets/Data/FlightDelays.csv")
names(fd)
head(fd)
#The columns are the variables. There are two types of variables: 
#numeric, for example, FlightLength and Delay and factor 
#(also called categorical) (for example Carrier and DepartTime). The rows are called 
#observations or cases.
dim(fd)

#summary of variable 'Carrier'
table(fd$Carrier)
table(fd$FlightLength)
barplot(table(fd$Carrier))

#compare two categorical variables
table(fd$Carrier, fd$Delayed30)
hist(fd$Delay)

# numeric summaries
delay <- fd$Delay
mean(delay)
median(delay)
mean(delay,trim=.25)

# check that sd = sqrt(var)
sd(delay) == sqrt(var(delay))

#If you need the population variance 
#(that is, denominator of 1/n instead of 1/(n-1))
n <- length(delay)
(n-1)/n*var(delay)


tapply(delay,fd$Carrier,mean)
tapply(delay,fd$DepartTime,median)
boxplot(Delay ~ Day, data=fd)
tapply(delay, fd$DepartTime, summary)
boxplot(Delay ~ DepartTime, data = fd)
seq(0,3,by=.2)

x <- c(2, 0, -4) # combine ints into a sequence
w <- 6:10

which(x < 4)
index <- which(x < 4)
x[index]


delay <- subset(FlightDelays, select = Delay, drop = TRUE)
delay2 <- subset(fd,select=Delay,subset=Day != "Mon",drop=TRUE)

#obtain random sample without replacement:
sample(10,4)
sample(10,4,replace=TRUE)


### PART 1 ###

# Determine the median flight delay for each day of the week.
delay <- fd$Delay
tapply(delay,fd$Day,median)
# Determine the mean flight delay for flights on AA on Wednesdays.
dfSubset <- subset(fd, subset = (Carrier == "AA" & Day == "Wed"), drop = TRUE)
median(dfSubset$Delay)
# Make a histogram of all the flight lengths (in minutes).
hist(fd$FlightLength)
# Make a histogram of all the flight lengths (in minutes) for flights to DEN.
dfDen <- subset(fd, subset = (Destination == "DEN"), drop = TRUE)
hist(dfDen$FlightLength)

### PART 3 ### 

# When you roll three fair dice, 
# what is the probability P 1 that they all show the same number, 
# the probability P 2 that they show two different numbers, and the probability P3 
# that they show three different numbers? Of course, P1+P2+P3 = 1. You are 
# welcome to check your answer by approaching this as a counting problem, 
# but the challenge is to do it by having R count rows for each event.

df <- read.csv('/Users/hogstrom/Documents/code/ModernKicks/Math_E156/class1/Dice3.csv')

# probability P 1 that they all show the same number, 
df3Match <- df[(df$Red3 == df$Green3) & (df$Red3 == df$White3),] #df where all dice match
P1 <- nrow(df3Match)/ nrow(df) # number of outcomes where all match divided by all possible outcomes

# probability P 2 that they show two different numbers
# (or probability that two of the dice match)
No3Match <- df[-df3Match$X,] # exclude instances where all 3 match
df2Match <- No3Match[(No3Match$Red3 == No3Match$Green3) | (No3Match$Red3 == No3Match$White3),] #df where two dice match
P2 <- nrow(df2Match)/ nrow(df) # number of outcomes where all match divided by all possible outcomes

# probability P3 that they show three different numbers
NoMatch <- df[(df$Red3 != df$Green3) & (df$Red3 != df$White3) & (df$Green3 != df$White3),] #df where all dice match
P3 <- nrow(NoMatch)/ nrow(df) # number of outcomes where all match divided by all possible outcomes




