#Math E-156 Script 5P-Proof 5.R

#Topic 1 - moment generating function for discrete distributions

#Let Y be a vector of values (not necessarily positive or integer).
#Let pr be a vector of probabilities
#The moment generating function for random variable X is the expectation of e^X
mgf <- function(x, Y, pr)  {
  sum(pr*(exp(x*Y)))
}

#Simplest example: Y1 is the constant random variable 3
Y1 <- 3; pr <- 1
mgfP3 <- Vectorize(function(x) mgf(x, Y1, pr))
#"Vectorize" lets the function act on a vector, produce a vector (required by curve())
curve (mgfP3, from = -1, to = 1)
#This is just a graph of the exponential fuction e^(3x)
curve (exp(3*x), col = "red", add = T)

#Next example: Y2 is the constant random variable -1
Y2 <- -1; pr <- 1; mgfM1 <- Vectorize(function(x) mgf(x, Y2, pr))
curve (mgfM1, from = -1, to = 1, col = "green", add = TRUE)

#The random variable Y1+Y2 has the constant value 2.
Y3 <- 2; pr <- 1; mgfP2 <- Vectorize(function(x) mgf(x, Y3, pr))
curve (mgfP2, from = -1, to = 1, col = "blue", lty = 2, add = TRUE)

#Alternatively we can multiply the mgfs
mgfPlot <- function(x) mgfP3(x)*mgfM1(x)
curve (mgfPlot, from = -1, to = 1, col = "blue", lty = 1, add = TRUE)

#Now let's try with random variables that have two possible values
#Simplest example: Y1 is equally likely to be +3 or -3
Y1 <- c(3, -3); pr <- c(0.5, 0.5)
sum(pr*(exp(Y1)))
mgf1 <- Vectorize(function(x) mgf(x, Y1, pr))
curve (mgf1(x), from = -1, to = 1)
#Note: the slope at x = 0 is zero, and so is the expectation

#Y2 is equally likely to be +1 or -1
Y2 <- c(1,-1); pr <- c(0.5,0.5); mgf2 <- Vectorize(function(x) mgf(x, Y2, pr))
curve (mgf2, from = -1, to = 1, col = "red", add = TRUE)
#Note: same expectation, but smaller curvature shows smaller variance

#The random variable Y1+Y2 has four possible values
Y3 <- c(-4, -2, 2, 4); pr <-c(0.25, 0.25, 0.25, 0.25); mgf3 <- Vectorize(function(x) mgf(x, Y3, pr))
curve (mgf3, from = -1, to = 1, col = "blue", lty = 2, add = TRUE)

Alternatively we can multiply the mgfs
mgfPlot <- function(x) mgf1(x)*mgf2(x)
curve (mgfPlot, from = -1, to = 1, col = "blue", lty = 1, add = TRUE)

#Topic 2 -- summing discrete random variables in R

#Here is how to make a table of addition facts
outer(1:5, 1:5, "+")
#Here is how to make a table of multiplication facts
outer(1:5, 1:5, "*")

#To sum two discrete random variables,add the values, multiply the probabilities
Y4 <-outer(c(-3,3),c(-1,1),"+"); Y4
pr4 <- outer(c(0.5,0.5),c(0.5,0.5),"*"); pr4

#The results are displayed as matrices, but in R a matrix is really a vector
mgf4 <- Vectorize(function(x) mgf(x, Y4, pr4))
curve (mgf4, from = -1, to = 1, col = "magenta", lty = 2, add = TRUE)

#Final example: Bernoulli with probability 0.3
Y = c(0,1); pr = c(0.7, 0.3)
mgf1 <- Vectorize(function(x) mgf(x, Y, pr))
curve (mgf1(x), from = -1, to = 1)
#There is a tricky way to evaluate the expectation as the derivative at 0 
x <- 0; numericDeriv(quote(mgf1(x)), "x")
#To get the second moment, approximate the second derivative
h <- 0.001; (mgf1(h)+mgf1(-h) - 2*mgf1(0))/h^2

#Now add two of these Bernoulli random variables together
Y2 <- outer(Y, Y, "+"); pr2 <- outer(pr, pr, "*")
mgf2 <- Vectorize(function(x) mgf(x, Y2, pr2))
curve (mgf2(x), from = -1, to = 1)
x <- 0; M1<-numericDeriv(quote(mgf2(x)), "x"); M1   #double the expectation
h <- 0.001; M2<-(mgf2(h)+mgf2(-h) - 2*mgf2(0))/h^2
M2 - M1^2     #the variance has also doubled

#Alternatively, and more easily, we could have squared the original mgf
curve (mgf1(x)^2, from = -1, to = 1, col = "red")
#It is easy to get the mgf for the sum of 10 such variables
curve (mgf1(x)^10, from = -1, to = 1, col = "red")
#We get the same mgf by knowing that the sum has a binomial distribution
Y10 <- 0:10; pr10 <- dbinom(0:10, 10, 0.3)
mgf10 <- Vectorize(function(x) mgf(x, Y10, pr10))
curve (mgf10(x), from = -1, to = 1, col = "black", add = TRUE)

#Topic 3 - making the mgf for a continuous random variable
#With a continous random variable we have to do an integral
#Defining properly "vectorized" functions is really tricky!
#Try an exponential random variable with lambda = 2
density <- Vectorize(function(x,y) exp(x*y)*dexp(x,2),vectorize.args = 'x')
mgf <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
curve(mgf(x), from = -1, to =1)

#We can find the expectation and variance by taking derivatives
h <- 0.00001; M1<-(mgf(h)-mgf(0))/h; M1   #expectation
h <- 0.0001; M2<-(mgf(h)+mgf(-h) - 2*mgf(0))/h^2; M2   #second moment
M2 - M1^2     #the variance 

#The sum of three independent exponential random variables has a gamma distribution
#We can find its mgf by cubing what we just found
curve(mgf(x)^3, from = -1, to =1, lty = 2)
#Or we could start over from the density function
density <- Vectorize(function(x,y) exp(x*y)*dgamma(x,3,rate = 2),vectorize.args = 'x')
mgfGamma <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
curve(mgfGamma(x), from = -1, to =1, , col = "red", add = TRUE)

#We can find the expectation and variance by taking derivatives
h <- 0.00001; M1<-(mgfGamma(h)-mgfGamma(0))/h; M1   #expectation is three times larger
h <- 0.0001; M2<-(mgfGamma(h)+mgfGamma(-h) - 2*mgfGamma(0))/h^2; M2   #second moment
M2 - M1^2     #the variance is also three times larger















