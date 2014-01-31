#1-Moments
#Sections A.7 and 2.7 of the textbook
#Moments - a concept from probability
#For random variable X, the nth moment is the expectation of X^n
#The nth central moment is the expectation of (X-mu)^n
#For a finite population, all equally likely, these are easy to calculate in R
Dice = read.csv("Dice3.csv")
X <- Dice$Red3 + Dice$Green3 + Dice$White3
mu <- mean(X); mu   #first moment
M2 <- mean(X^2); M2     #second moment
MC2 <- mean((X-mu)^2); MC2  #the variance
M3 <- mean(X^3); M3     #third moment
MC3 <- mean((X-mu)^3); MC3  #zero ,by symmetry
M4 <- mean(X^4); M4     #fourth moment
MC4 <- mean((X-mu)^4); MC4
#It is conventional to make the higher moments invariant to scale change
sigma<- sqrt(MC2); sigma  #the standard deviation
skewness <- MC3/sigma^3; skewness  #zero in this case
kurtosis <- MC4/sigma^4-3; kurtosis #zero for a normal dstribution

#We can also do the calculation starting with a discrete density function
#Try a Poisson distribution with lambda = 5
plot(0:20, dpois(0:20,5), "h")
mu<-sum(0:30*dpois(0:30,5)); mu  #remaining terms in infinite sum ignored
MC2<-sum((0:30-mu)^2*dpois(0:30,5));MC2   #easily proved in general
sigma = sqrt(MC2)
MC3<-sum((0:30-mu)^3*dpois(0:30,5));MC3
MC4<-sum((0:30-mu)^4*dpois(0:30,5));MC4
MC3/sigma^3    #positive value from long tail to the right
kurtosis <- MC4/sigma^4-3; kurtosis #zero for a normal dstribution

#With a continuous distribution we need to do integrals
#Start with the normal distribution: expectation 3, variance 4
curve(dnorm(x,3,2), from = -4, to = 10) 
integ<-integrate(function(x) x*dnorm(x,3,2), -Inf, Inf); integ
mu<-integ$value; mu     #this can be used as a number
MC2<-integrate(function(x) (x-mu)^2*dnorm(x,3,2), -Inf, Inf); MC2
sigma<- sqrt(MC2$value); sigma
MC3<-integrate(function(x) (x-mu)^3*dnorm(x,3,2), -Inf, Inf); MC3 #no skewness
MC4<-integrate(function(x) (x-mu)^4*dnorm(x,3,2), -Inf, Inf); MC4
kurtosis <- MC4$value/sigma^4-3; kurtosis #zero for a normal dstribution

#Example 2.13 - exponential with lambda = 1
#In the textbook the integrals are evaluated by knowing antiderivatives.
#In R we can do them numerically.
curve(dexp(x,1), from = 0, to = 6) 
integ<-integrate(function(x) x*dexp(x,1), 0, Inf); integ
mu<-integ$value; mu     #this can be used as a number
MC2<-integrate(function(x) (x-mu)^2*dexp(x,1), 0, Inf); MC2
sigma<- sqrt(MC2$value); sigma
MC3<-integrate(function(x) (x-mu)^3*dexp(x,1), 0, Inf); MC3 #skewed to the right
MC3$value/sigma^3     #the skewness
MC4<-integrate(function(x) (x-mu)^4*dexp(x,1), 0, Inf); MC4
kurtosis <- MC4$value/sigma^4-3; kurtosis #zero for a normal dstribution

#An alternative approach is to draw a lot of samples from the distribution and compute sample moments
x = rexp(10^4, 1);head(x)
hist(x, breaks = 200, col= "red", add = TRUE, freq = FALSE)
x.bar <- mean(x); x.bar    #should be close to mu = 1
MC2.bar <- mean((x-x.bar)^2)
S<- sqrt(MC2.bar); S    #should be close to 1
MC3.bar <- mean((x-x.bar)^3)
MC3.bar/S^3       #not a great estimate of the skewness - 2 is correct
MC4.bar <- mean((x-x.bar)^4)
kurtosis <- MC4.bar/sigma^4-3; kurtosis   #Correct value is 6
#Repeat with 100,00 samples and you will get a better result

#The problem was with the long tail - try example 2.14
curve(dunif(x, 0, 1), from = 0, to = 1)
x = runif(10^4, 0, 1);head(x)
hist(x, breaks = 100, col= "red", add = TRUE, freq = FALSE)
x.bar <- mean(x)  ; x.bar    #should be close to mu = 0.5
MC2.bar <- mean((x-x.bar)^2)
S<- sqrt(MC2.bar); 12*S^2    #should be close to 1
MC3.bar <- mean((x-x.bar)^3)
MC3.bar/S^3       #should be close to zero
MC4.bar <- mean((x-x.bar)^4)
kurtosis <- MC4.bar/S^4-3; kurtosis   #Correct value is -1.2



