#Math E-156 Script 11B-ConfidenceOneSided.R

#Topic 1 -- computing a one-sided confidence interval from sample mean and sd
#Example 7.14 from the textbook
#15 samples of lake water have average lead level 7 with sample sd 2
mu <- 7; SE <- 2/sqrt(15)   #need standard error for Student denominator

#Assume these data have a t distribution with 14 degrees of freedom
q95<-qt(.95, 14); q95 #quantile of the t distribution of (Xbar - mu)/SE

L <- mu-q95*SE; L #lower limit of confidence interval
#The probability that L is less than the true mean is 0.95

#Topic 2 -- a simulation for lead levels in lake water
#To do a simulation we have to assume that we know the parameters for a population
#For Student t theory to be valid, we need a normal distribution.
#So we assume that the distribution of lead in samples is N(7,2)

#First we test our methodology with a single sample.
x<-rnorm(15,7,2); x               #Take one sample of 15
xbar<-mean(x); S <-sd(x); xbar; S #random variables derived from the sample
SE <- S/sqrt(15)         #standard error is the Student denominator
q95<-qt(.95, 14);q95 #quantile of the t distribution of (Xbar - mu)/SE
xbar-q95*SE #lower limit of confidence interval - a random variable
#95% of the time, this will be less than the true mean of 7
#Here is the assumed t distribution with our sample mean
density <- function(x) {dt(x-7, df = 14)}
curve(density, from = 2, to = 12); abline(v=xbar, col = "red")

#Run some simulations
N<- 1000; counter <- 0
plot(x =c(4,10), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:N) {
  x<-rnorm(15,7,2)         #Take one sample of 15
  xbar<-mean(x); S <-sd(x) #random variables derived from the sample
  SE <- S/sqrt(15)         #standard error is the Student denominator
  q95<-qt(.95, 14)         #quantile of the t distribution of (Xbar - mu)/SE
  L <- xbar-q95*SE         #lower limit of confidence interval - a random variable
  if (L < 7) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100){
    segments(L, i, 10, i) #the confidence interval extends from L to infinity
    points(xbar, i, col = "red", pch = ".", cex = 3) #the sample mean from the data
  }
}
abline (v = 7, col = "blue") #vertical line at population mean
counter/N #what fraction of the time did our confidence interval include the true mean?

#Topic 2 - simulation, using the mean mu as a scale parameter
#Example 7.16 --same data, but use xbar/mu as the statistic
#It has mean 1 and standard deviation 2/7
x<-rnorm(15,7,2)        #Take a sample of 15
xbar<-mean(x)/7; S <-sd(x)/7; xbar; S #rescaled sample mean and SD
SE <- S/sqrt(15)    #standard error of rescaled sample
#Rescaling by mu leads to the same t distribution
q95<-qt(.95, 14); q95 #quantile of the t distribution of ((Xbar/mu)-1)/(S/mu)
U <- 1 + q95*SE; U     #Rescaled sample mean will be less than U 95% of the time
mean(x)/U   #lower limit of confidence interval - a random variable
#95% of the time, this will be less than the true mean of 7

#Run some simulations
N<- 1000; counter <- 0
plot(x =c(4,12), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:N) {
  x<-rnorm(15,7,2)        
  xbar<-mean(x)/7; S <-sd(x)/7; xbar; S #rescaled sample mean and SD
  SE <- S/sqrt(15)    #standard error of rescaled sample
  q95<-qt(.95, 14); q95 #quantile of the t distribution of ((Xbar/mu)-1)/(S/mu)
  U <- 1 + q95*SE; U     #Rescaled sample mean will be less than U 95% of the time
  L <- mean(x)/U   #lower limit of confidence interval - a random variable
  if (L < 7) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100){
    segments(L, i, 12, i) #the confidence interval
    points(mean(x), i, col = "red", pch = ".", cex = 3) #the sample mean from the data
    #Also display the subtractive confidence limit
    points(mean(x)-q95*(sd(x)/sqrt(15)), i, col = "green", pch = "|", cex = 0.5)
  }
}
abline (v = 7, col = "blue") #vertical line at population mean
counter/N #what fraction of the time did our confidence interval include the true mean?



#Topic 3 -- computing a one-sided confidence interval from real data
#Read the data for birth weights of North Carolina babies.
NCB<-read.csv("NCBirths2004.csv"); head(NCB)
weight<-NCB$Weight; n<-length(weight);n # lots of babies

#In this case there are so many degrees of freedom that the t distribution is almost normal
q95<-qt(.95, n); n95<-qnorm(.95); q95; n95 #not much need to use t test

#Subtract the 95th quantile times the standard error to get a lower bound
L1 <- mean(weight)-q95*(sd(weight)/sqrt(n)); L1
#Assuming a normal distribution gives almost the same number.
L2 <- mean(weight)-n95*(sd(weight)/sqrt(n)); L2

#This test has also been automated
t.test(weight, alt = "greater")

