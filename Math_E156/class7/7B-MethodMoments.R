#Math E-156 Script 7B-EstimationMoments.R

#Topic 1 - choosing one parameter to make the theoretical mean match the sample mean
#Based on section 6.2, pages 146-148 of the textbook
#This technique requires knowing the theoretical moments of varous distributions, tabulated on pp. 396-399
#The number of sample moments must equal the number of parameters

#Example 6.8 uses a uniform distribution
#Generate 20 samples from a uniform distribution on [0, 8]
x <- runif(20, 0, 8)
#For the uniform distribution on [0, beta] the mean should be beta/2
#So to make the theoretical and observed moments agree, we double the sample mean to estmate beta
beta.hat <-2*mean(x); beta.hat
sum(x > beta.hat)  #if positive, indicates an impossible estimate
#Regenerate x a few times and you will get an impossible result
#We will find a "more efficient" estimator of the mean than this one.

#Example 6.9 uses an exponential distribution
#Generate 20 samples from an exponential distribution with lambda = 2.
#The theoretical mean is 1/lambda = 0.5
x <- rexp(20, 2)
lambda.hat <-1/mean(x); lambda.hat   #estimate of lambda for which the mean matches the sample mean
#Try this 1000 times to see how it does
N <- 1000; lambdas <- numeric(N); means <- numeric(N)
for (i in 1:N) {
  x <- rexp(20, 2)
  M1 <- mean(x)
  means[i] <- M1     #record the sample mean
  lambdas[i] <- 1/M1  #record the corresponding estmate of lambda
}
mean(means)   #theoretical value is 0.5
median(means) #for an exponential distribution the median is less than the mean -- persists in the sampling distribution
#In general, E[1/X] does not equal 1/E[X], and that is the case here.
mean(lambdas)  #theoretical value is not 2, alas
hist(lambdas)
abline(v = mean(lambdas), col = "red")
mean(lambdas > 2)  #comes out too large more than half the time

#We can only use one moment to estimate one parameter, but why not the second moment?
#The theoretical second moment is 2/lambda^2 = 0.5
x <- rexp(20, 2)
lambda.hat <- 2/sqrt(mean(x^2)); lambda.hat  #this value of lambda would match the observed second moment
#Try this 1000 times to see how it does
N <- 1000; lambdas <- numeric(N); moments2<-numeric(N)
for (i in 1:N) {
  x <- rexp(20, 2)
  M2 <- mean(x^2) 
  moments2[i] <- M2        #second moment of the sample
  lambdas[i] <- sqrt(2/M2)  #corresponding parameter estimate
}
mean(moments2)   #theoretical value is 0.5
mean(lambdas)  #because of exponential skewness, the theoretical value is not 2
hist(lambdas)
abline(v = mean(lambdas), col = "red")
mean(lambdas > 2)  #comes out too large more than half the time - problem is even worse than when we used the mean

#Topic 2 - using two sample moments to estmate two parameters
#With two parameters we can use two moments
#Example 6.11 -- Exponential distribution with lower limit delta
#This could happen if you start observing radioactive decays at time delta
#Choose delta = 4, lambda = 2, and generate 50 samples
x <- rexp(50,2) + 4
hist(x)
#Calculating the first two moments from the sample is easy
M1 = mean(x); M1
M2 = mean(x^2); M2

#The messy algebra on page 148 is not really necessary.
#For an exponential distribution the variance is 1/lambda^2 (page 397)
variance <- M2 - M1^2  #this sample variance is independent of delta
lambda.bar = 1/sqrt(variance); lambda.bar  #estimate of lambda based just on the sample variance
#If delta = 0, then the mean and standard deviation are equal, so
delta.bar = M1 - sqrt(variance); delta.bar #estimate of delta based on both sample moments

#If delta.bar > 4 then some of the samples might again be impossible
#Regenerate the sample a couple of times and you will probably observe this phenomenon.
sum(x < delta.bar)

