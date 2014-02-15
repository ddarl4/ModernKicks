#Math E-156 Script 3B-PermSkewed.R
#Based on Section 3.3, pages 44-51

#Topic 1 - dealing with skewed data and unequal sample size
#This is example 3.3, which starts on page 44.
#The technique is exactly the same as before.
Verizon <- read.csv("Verizon.csv"); head(Verizon) #repair data
Time<-Verizon$Time   #a vector containing all the time data
TimeSplit<-split(Verizon$Time,Verizon$Group) #a list of split time vectors
mean(TimeSplit$ILEC)   #Verizon customers
mean(TimeSplit$CLEC)   #non-Verizon customers
lapply(TimeSplit, length) #notice the wildly unequal group sizes
#We save the observed difference so that we can compare it with differences from random samples
observed <- mean(TimeSplit$CLEC) - mean(TimeSplit$ILEC); observed #is this difference significant?
hist(Time, breaks = "FD")   #notice the highly skewed data
#Although the skewness is clearly large, let's compute it for practice
x.bar = mean(Time)     #sample mean
S = sd(Time)           #sample standard deviation
mean((Time-x.bar)^3)/S^3  #it's meaningful to compare this skewness to 1

#Now do a permutation test 
#Just use a random subset of 23 customers in place of the non-Verizon customers.
#To choose a subset we sample without replacement.
#The book draws a sample that includes all but 23 customers, which does the same split
index <- sample(1:length(Time), size = length(TimeSplit$CLEC)); index #23 random rows
mean(Time[index]) - mean(Time[-index])        #difference in mean repair times between the groups
N = 10^4 -1 ; result <- numeric(N)      #vector to hold results from 9999 random subsets
for (i in 1:N) {
  index <- sample(1:length(Time), size = length(TimeSplit$CLEC))
  result[i]<- mean(Time[index]) - mean(Time[-index])
}
hist(result,breaks = "FD")   #like Figure 3.5 but the sign is reversed
#Skewness persists, because sometimes one of the 23 has a very long repair time
abline(v = observed, col = "red") #observed difference is unusual
pVal <- (sum(result >= observed) +1)/(N+1); pVal    #the P-value shows how unusual

#Topic 2 - Using other statistics 
#Anything reasonable will do, like the median
#The occasional very long repair time will not affect the median very much
observed <- median(TimeSplit$CLEC) - median(TimeSplit$ILEC); observed 
#Surprise - the difference in medians is greater than the difference in means was
N = 10^4 -1 ; result <- numeric(N)
for (i in 1:N) {
  index <- sample(1:length(Time), size = length(TimeSplit$CLEC))
  result[i]<- median(Time[index]) - median(Time[-index])
}
hist(result, breaks = "FD")    #much less skewed than when we used the mean -- less effect from long repair times
abline(v = observed, col = "red") #observed difference was MOST unusual
pVal<-(sum ( result >= observed) +1)/(N+1); pVal    #looks very bad for Verizon!

#Or we could use the ratio of means
observed <- mean(TimeSplit$CLEC)/ mean(TimeSplit$ILEC); observed #twice as long for CLEC customers
N = 10^4 -1 ; result <- numeric(N)
for (i in 1:N) {
  index <- sample(1:length(Time), size = length(TimeSplit$CLEC))
  result[i]<- mean(Time[index])/ mean(Time[-index]) #computing a different statistic
}
hist(result, breaks = "FD")  
abline(v = observed, col = "red") #observed difference is unusual
pVal <-(sum ( result >= observed) +1)/(N+1); pVal #P-value is close to what we got with means

#Or we could use the difference in proportion of repair times greater than 10 hours
#Just copy the code and change the two lines that compute the chosen statistic
mean(TimeSplit$CLEC >10); mean(TimeSplit$ILEC>10)   #CLEC wait more than 10 hours much more often
#If the CLEC sample were large this difference would clearly be significant.
#A permutation test will show that it cannot be blamed on the small CLEC sample.

mean(Time >10)   #these long delays happen about 23% of the time
#In a group of 23 customers we expect to see about 5 or 6 long delays
sum(TimeSplit$CLEC >10) #the non-Verizon customers had 13 of them!

observed <- mean(TimeSplit$CLEC >10) - mean(TimeSplit$ILEC>10); observed #save it for comparison
N = 10^4 -1 ; result <- numeric(N)
for (i in 1:N) {
  index <- sample(1:length(Time), size = length(TimeSplit$CLEC))
  result[i]<- mean(Time[index] > 10) - mean(Time[-index] > 10) #just change the statistic
}
table(result)  #there are between 0 and 14 long delays in our random samples
hist(result, breaks = "FD")   
abline(v = observed, col = "red") #observed difference is unusual
pVal <-(sum ( result >= observed) +1)/(N+1); pVal    #those 13 long delays were highly unlikely



