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
df2Match <- No3Match[(No3Match$Red3 == No3Match$Green3) | (No3Match$Red3 == No3Match$White3) | (No3Match$Green3 == No3Match$White3),] #df where two dice match
P2 <- nrow(df2Match)/ nrow(df) # number of outcomes where all match divided by all possible outcomes

# probability P3 that they show three different numbers
NoMatch <- df[(df$Red3 != df$Green3) & (df$Red3 != df$White3) & (df$Green3 != df$White3),] #df where all dice match
P3 <- nrow(NoMatch)/ nrow(df) # number of outcomes where all match divided by all possible outcomes

#check if the probabilties sum to 1
if (P1 + P2 + P3 == 1) {
    print("P1, P2, and P3 sum to 1")
} else {
    print("P1, P2, and P3 do not sum to 1")
}

### PART 4 ###

# two quizes, each with a single multiple choice questions
# four answers to each question
# You can do nothing but guess, so your probability of getting a score 
# of 100 on a quiz is 1/4, while your probability of getting 0 is 3/4.

# Make a data frame with columns Q1 and Q2 for your two quiz scores. By using 16 rows, all 
# assumed equally likely, you can make the probabilities come out correct. Do 
# this in three ways:

# QEdit is done using the R Data Editor (see Funny Dice).(your script will not show theresult of your editing)
TestResults = data.frame(Q1=numeric(),Q2=numeric()) #empty data frame
TestResults = edit(TestResults) #bring up the editor

# QRep is done using the rep() function (see Funny Dice).
# 1 represents the correct answer, 0 represents the wrong answer 
Q1<-c(rep(0,12),rep(1,4)); Q1
Q2<-rep(c(0,0,0,1),4); Q2
#Now combine the columns into a data frame
TestResults<-data.frame(Q1,Q2) 
# check that the probability of getting a score of 100 on each quiz is 1/4
pTest1 <- sum(TestResults$Q1)/length(Q1); pTest1
pTest2 <- sum(TestResults$Q2)/length(Q2); pTest2

# QGrid is done using the expand.grid() function (see CardPairs).
TestResults<-expand.grid(c(0,0,0,1),c(0,0,0,1), stringsAsFactors = FALSE)  #a giant Cartesian product!
names(TestResults) <- c("Q1", "Q2") #specify the column names

# Using QRep or QGrid, which should be identical, create two random variables:
# X is your average quiz score, 100, 50, or 0.
X <- (TestResults$Q1+TestResults$Q2)/2
TestResults<-data.frame(TestResults,X)

# Y is your improvement rating. This is 0 if you score worse on the second quiz, 
# 1 if you score the same on both quizzes, 2 if you score better on the second quiz.
calc_improvement <- function(x) {
    if (x[1] > x[2]){
        0
    } else if (x[1] == x[2]) {
        1
    } else if (x[1] < x[2]){
        2
    }
}
# apply function which assigns the improvement score
# based on the Q1 and Q2
Y <- apply(TestResults, 1,calc_improvement) # calculate improvements by row
TestResults<-data.frame(TestResults,Y); TestResults

# Calculate E[XY ] − E[X]E[Y ]. Since the random variables are uncor- related, this should equal zero.
mean(TestResults$X*TestResults$Y) - mean(TestResults$X)*mean(TestResults$Y)

# Calculate E[X2Y 2] − E[X^2]E[Y^2]. If X and Y were independent this would also equal zero.
product <- TestResults$X*TestResults$Y
mean(product^2) - mean(TestResults$X^2)*mean(TestResults$Y^2)

# Invent an event A involving X and and an event B involving Y such that P (A ∩ B) ̸= P (A)P (B), 
# and do the calculation of these probabilities in R by counting rows.
 
#### Part 5 ####
# Make a data frame by loading the file cereals.csv from your Data subfolder 
# and write a script that does the following. The details are up to you.
cerials <- read.csv('/Users/hogstrom/Documents/code/ModernKicks/Math_E156/class1/Cereals.csv')

# Make a barplot.
barplot(table(cerials$Age))

# Make a histogram.
hist(cerials$Sodiumgram)

# Make a contingency table using two factors.
table(cerials$Age,cerials$Shelf)

# Calculate a mean broken down by factor.
SodimMean <- tapply(cerials$Sodiumgram, cerials$Age ,mean); SodimMean

# Extract a subset of one numeric variable for one factor.
bottom <- cerials[cerials$Shelf == 'bottom',]; bottom




