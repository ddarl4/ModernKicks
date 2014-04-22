#Math E-156 Script 10B-BayesBeta.R

#Topic 1 - looking at some beta distributions
#Beta distributions fit together nicely with binomial distributions
#With two parameters, we can get a wide variety of behaviors on [0,1].
curve(dbeta(theta, 12, 12), from = 0, to = 1, xname = "theta", ylab = "Beta density")
curve(dbeta(x, .5, .5), col = "red", add = TRUE) #called the arcsine density function
curve(dbeta(x, 1, 1), col = "green", add = TRUE) #uniform
curve(dbeta(x, 1, 3), col = "blue", add = TRUE) #mean is 1/4, variance is large
curve(dbeta(x, 2, 6), col = "blue", lty = 2, add = TRUE) #mean is 1/4
curve(dbeta(x, 4, 12), col = "blue", lty = 3, add = TRUE) #mean is 1/4, but variance is smaller
curve(dbeta(x, 0.01, 0.01), col = "gray", add = TRUE) #true or false
legend("topright", legend = c("mean 1/2", "arcsine", "uniform", "mean 1/4", "noninformative"), lty = 1, col = c("black", "red", "green", "blue", "gray"))

#Topic 2 - mean and variance of a beta distribution
alpha <- 3; beta <- 7; #change these numbers if you like
curve(dbeta(x, alpha, beta))  #the built in density function

#Use the formula from page 290 of the textbook
mybeta <- function(x) gamma(alpha+beta)*x^(alpha-1)*(1-x)^(beta-1)/(gamma(alpha)*gamma(beta))
curve(mybeta(x), col = "red", add = TRUE) #looks the same

#Check properties of the beta distribution
integrate(function(x) dbeta(x, alpha, beta), 0, 1)  #normalized to 1
#The expectation is alpha/(alpha+beta)
integrate(function(x) x*dbeta(x, alpha, beta), 0, 1); mu <-alpha/(alpha+beta); mu 
#The variance is alpha*beta/(alpha+beta)^2*(alpha+beta+1)
integrate(function(x) (x-mu)^2*dbeta(x, alpha, beta), 0, 1)
alpha*beta/((alpha+beta)^2*(alpha+beta+1))  #the formula is correct!
(mu^2-mu^3)/(alpha+mu)    #another formula for the variance

#Given a desired expectation E and variance V, we can create a beta distribution
E <- 0.7; V <- 0.01 #targets
alpha <- (E^2 - E^3)/V - E
beta <- alpha/E - alpha
mu <-alpha/(alpha+beta); mu   #the mean is correct
alpha*beta/((alpha+beta)^2*(alpha+beta+1)) #so is the variance
curve(dbeta(x, alpha, beta))  #a Bayesian prior with the given mean and variance


#Topic 3 - using a continuous Bayesian prior to estimate a proportion
#Example 10.3 with less data -- 11 of 20 students report that they watch the Daily Show
#Just add these "Yes" and "No" responses to the beta parameters

#Case 1: assume a uniform prior: alpha = beta = 1, and add in 11Y, 9N.
curve(dbeta(theta, 12, 10), from = 0, to = 1, xname = "theta", ylab = "Beta density")
curve(dbeta(x, 1, 1), col = "green", add = TRUE) #the uniform prior

#Now theta is a random variable and we can find a 95% "credible interval" for it
qbeta(c(.025, .975), 12, 10)

#The calculation on page 314 of the textbook
#Suppose that a national poll suggests that 58% watch the show, with standard deviation .03
#We know how to make a beta distribution with a mean of 0.58, variance 0.0009
E <- 0.58; V <- 0.0009 #targets
alpha <- (E^2 - E^3)/V - E
beta <- alpha/E - alpha; alpha; beta #values agree with page 314
curve(dbeta(theta, alpha, beta), from = 0, to = 1, ylim = c(0,17), xname = "theta", ylab = "Beta density")
#The national poll might have received 156 "yes", 113 "no" responses

#Use the book's data: on our campus, 110 of 200 watch the show
curve(dbeta(theta, alpha+110, beta+90), xname = "theta", col = "red", add = TRUE) 
#This posterior distribution combines national and local data

#Get a 95% "credible interval"
qbeta(c(.025, .975), alpha+110, beta+90)

#If we round alpha and beta to the nearest integer we can get the same answer from a binomial distribution.
n <-  round(alpha+beta+200)
qbinom(c(.025,.975),n,(alpha+110)/(alpha+beta+200) )/n

