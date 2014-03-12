#Math E-156 Script 7A-MLE estimates.R

#Topic 1 - Maximum likelihood estimate of a parameter

#Example 6.7 from the textbook
#The Cauchy distribution has a single parameter theta, which is its median.
#Suppose that independent samples of 1, 2, 2, 3, and 4 have been drawn from this distribution.
#The likelihood of this event is a function of theta.
#It is proportional to the probability density for the observed event.
L<-function(theta) 1/((1+(1-theta)^2)*(1+(2-theta)^2)^2*(1+(3-theta)^2)*(1+(4-theta)^2))
#Plot the function to see what is going on
curve(L(x), from = 1, to = 3)
#Now find the maximum - leads to a ninth-degree equation on page 143. 
#Fortunately R can easily find a local maximum or minimum
optimize(L, c(2,2.5), maximum = TRUE) #agrees with the text, which finds the minimum of g = 1/L

#The same approach works for Example 6.1, which can also be solved exactly
#The distribution is now discrete: a Poisson distrbution with unknown parameter lambda
#we have observed independent samples 3, 4, 3, and 7.
#Multiplying the Poisson mass functions gives the probability of this event
L<- function(lambda) lambda^17*exp(-4*lambda)  #product of four Poisson mass functions
curve(L(x), from = 1, to = 5)
optimize(L, c(3,5), maximum = TRUE)   #MLE estimate of lambda is the sample mean

#Example 6.4 - a uniform distribution on [0, beta]
#we have observed samples 1.2, 3.3, 4.5, and 5.0
#If beta < 5.0, the likelihood of observing these samples is zero
#Otherwise the density function is 1/beta and
L<- function(beta) 1/beta^4
curve(L(x), from = 5, to = 7)
optimize(L, c(5,7), maximum = TRUE) 
#So the MLE estimator of beta is just the largest sample
#There is clearly a problem with this estimator.
beta<-5; N <- 1000; beta.hats <-numeric(N)
for (i in 1:N) {
  x <- runif(4,max = beta)
  beta.hats[i] <- max(x)
}
hist(beta.hats,breaks = "FD")
mean(beta.hats)   #should be very close to 4.
#We can "unbias" this estimator by multiplying it by 5/4


#Topic 2 - using the mle function to estimate one or more parameters
#If there is more than one parameter to estimate, we need to load a library
library(stats4)
#Now we have access to the function mle() and its documentation
#The first argument must be minus the log-likelihood function.
#Redo example 6.7 using -log(L) instead of L
observed = c(1,2,2,3,4)  #the observed sample
#It is easier to add log-likelihoods than to multiply likelihoods, and we can use the built-in Cauchy density function.
MLL <-function(x) -sum(dcauchy(observed, x, log = TRUE))
#For some reason this function has to be vectorized before it can be plotted
curve(Vectorize(MLL)(x),from =1, to =3)
#Now call the built-in Maximum Likelihood Estimation function with a decent initial estimate
mle(MLL,start = list(x = 2)) #same answer

#Redo example 6.1 using the mle() function
observed = c(3,4,3,7)
MLL <-function(x) -sum(dpois(observed, x, log = TRUE))
curve(Vectorize(MLL)(x), from =1, to = 5) 
mle(MLL,start = list(x = 4)) #same answer as before

#This function works fine for more than one parameter
#Start with some observations and find parameters mu and sigma for a normal distribution
observed=c(1,2,3,4,5); mean(observed); sd(observed)  #sd estimates the population standard deviation
MLL <-function(mu, sigma) -sum(dnorm(observed, mu, sigma, log = TRUE)) #again, negative log likelihood
#Now we have a function of two variables, which is not easy to plot
#We have to provide a list of initial estimates, one per parameter
mle(MLL,start = list(mu = 2, sigma = 3)) 
#This is correct -- the MLE estimate of sigma is the actual sd of the observations
sqrt(mean((observed-mean(observed))^2))  #same as the MLE estimate

#We already know that this is a "biased" estimator of sigma.
#The quantity whose expectation equals the population standard deviation is
sd(observed)
#In this case we can "unbias" the MLE estimate by multiplying by a  factor based on the sample size
sqrt(5/4)*sqrt(mean((observed-mean(observed))^2))

#The example on pp. 144-145 uses the two-parameter Weibull distribution
#Remember to set the start folder for the data
Turbine<-read.csv("Turbine.csv"); head(Turbine)
wind<-Turbine$AveSpeed; mean(wind); length(wind)
#We have 168 observations from which to estimate the two paramters of the Weibull distribution.
MLL <-function(lambda, k) -sum(dweibull(wind, k, lambda, log = TRUE))
mle(MLL,start = list(lambda = 6, k=2)) 
#We got the book's answer in four lines!

#Now that we have the MLE values of the parameters, we can compare the data with the Weibull density function
hist(wind, breaks= "FD", probability = TRUE)
curve(dweibull(x, 3.17, 7.661), col = "red", add=TRUE)

#Here is the authors' code to compare cumulative probability
plot.ecdf(wind,main = "ECDF of wind data")
curve(pweibull(x,3.169,7.661), add=TRUE, col="blue",lwd=2)

#We can also do a chi-square goodness of fit test
#Get the deciles for the Weibull distribution with the MLE parameters
q <- qweibull(seq(.1, .9, by = .1), 3.169, 7.661); q   #a vector of 9 deciles

#range of wind
range(wind)

#encompass range of wind
q <- c(0, q, 14);q   #now we have 11 vaues that define 10 sub-intervals

hist(wind, breaks=q)   #each bar corresponds to a sub-interval, with the area proportional to the count
# Get the counts in each sub-interval. The plot=F command suppresses the plot

count <- hist(wind,breaks=q,plot=F)$counts; count
#Of course, we expect to see 10% of the counts in each decile
expected <- length(wind)*.1; expected  #R will repeat this number 10 times to subtract it from the longer count vector

# compute chi-square test statistic
ChiSq <- sum((count-expected)^2/expected); ChiSq
#We had 10 intervals but we estimated two parameters, leaving just 7 degrees of freedom
pchisq(ChiSq, df =7, lower.tail = FALSE) #agrees with the P-value on page 146

#Conclusion: the Weibull distribution, with our MLE parameters, could easily have generated the observed wind speeds



