#Math E-156 Script 8A-GammaChiSquare.R

#Topic 1 - properties of the gamma function
#Its definition as an improper integral
my.gamma<-Vectorize(function(r) integrate(function(x) x^(r-1)*exp(-x), 0, Inf)$val)
#When the argument is an integer, gamma(n+1) = n!
my.gamma(c(2:8))  #(n-1)!

#When the argument is 1/2, the function value is the square root of pi
my.gamma(0.5)^2

#The integral defnes the function for any positive x
curve(my.gamma(x), from = 0.1, to = 4.5)

#It interpolates the factorial function nicely
abline(v = c(1:4), col = "blue")
abline(h = c(1,2,6), col = "blue")

#Integer argument or not, gamma(r+1) = r gamma(r)
my.gamma(5)/my.gamma(4)   #same as for factorials
my.gamma(5.34)/my.gamma(4.34)   #works also for non-integers

#Topic 2 - density function for the gamma distribution

#The gamma fuction is needed to normalize the pdf for the gamma distribution
my.dgamma <- Vectorize(function(x, r, lambda) (lambda^r/my.gamma(r))*x^(r-1)*exp(-lambda*x), vectorize.args = "x")
#r = 1 gives the exponential distribution
curve(my.dgamma(x,1,1), from = 0, to = 8)
curve(dexp(x,1),col = "red", add = T)

#The function we just defined is the same as the R density function dgamma()
integrate(function(x) my.dgamma(x, 3.4, 2), 0, Inf)  #properly normalized
curve(my.dgamma(x,3.4,2), from = 0, to = 8)
curve(dgamma(x,3.4,2), col = "red", add = T) #same as the built-in function

#Topic 3 - moment generating function for the gamma distribution

#Choose a rate parameter lambda and two different scale parameters r1 and r2
lambda <- 2.5; r1 <- 3.2; r2 <- 4.3
#Work out the mgf for r1 by integration
#This is what we must integrate to get the expectation of exp(xY)
density <- Vectorize(function(x,y) exp(x*y)*dgamma(x,r1, lambda),vectorize.args = 'x')
#Integrate over y to get the moment generating function
mgfr1 <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
curve(mgfr1(x), from = -1, to =1, lty = 1)

#Compare with the closed-form expression for the mgf
curve((lambda/(lambda-x))^r1, col = "red", add = T)

#We can find the expectation and variance by taking derivatives
h <- 0.00001; M1<-(mgfr1(h)-mgfr1(0))/h; M1 ; r1/lambda  #expectation
h <- 0.0001; M2<-(mgfr1(h)+mgfr1(-h) - 2*mgfr1(0))/h^2; M2   #second moment
M2 - M1^2 ; r1/lambda^2    #the variance

#Work out the mgf for r2 by integration in exactly the same way
density2 <- Vectorize(function(x,y) exp(x*y)*dgamma(x,r2, lambda),vectorize.args = 'x')
mgfr2 <- function(x) sapply(x, function(y) integrate(density2, lower=0, upper=20, y=y)$val)
curve(mgfr2(x), from = -1, to =1, lty = 1)

#Work out the mgf for parameter r1+r2 by integration
density3 <- Vectorize(function(x,y) exp(x*y)*dgamma(x,r1+r2, lambda),vectorize.args = 'x')
mgfr3 <- function(x) sapply(x, function(y) integrate(density3, lower=0, upper=20, y=y)$val)
curve(mgfr3(x), from = -1, to =1, lty = 2)
h <- 0.00001; M1<-(mgfr3(h)-mgfr3(0))/h; M1 ; (r1+r2)/lambda  #expectation

#Multiply the earlier mgfs to get the mgf for the sum of gamma random variables
curve(mgfr1(x)*mgfr2(x), col = "red", add = T)
curve((lambda/(lambda-x))^(r1+r2), col = "blue", add = T)


#Topic 4 - the connection between the chi square and gamma distribution

#Look at a gamma distribution with rate parameter 1/2, scale parameter k/2
k <- 3   #change this to whatever integer you like
curve(dgamma(x,k/2,1/2), from = 0, to = 20)
#Compare with the chi square density for k degrees of freedom
curve(dchisq(x,k), col = "red", add = TRUE)

#Sum three chi-squared random variables
#Just as when we added gamma variables, the parameters add
m1 <- 2; m2 <- 5; m3 <- 3
curve(dchisq(x, m1), xlim = c(0,15))
curve(dchisq(x, m2), col = "red", add = T)
curve(dchisq(x, m3), col = "green", add = T)

#Take one sample from each and add them together
N <- 10^4; sums <- numeric(N)
for (i in 1:N) {
  sums[i] <- rchisq(1,m1)+ rchisq(1,m2)+ rchisq(1,m3)
}
hist(sums, breaks = "fd", freq = FALSE)
#The distribution matches a chi square distribution where we add up the parameters
curve(dchisq(x, m1+m2+m3), col = "red", add = TRUE)

#Topic 5 - the connection between the chi square and normal distributions

#Sample from a standard normal N(0,1) distribution, but square each sample
N <- 10^4; squares <- numeric(N)
for (i in 1:N) {
  squares[i] <- rnorm(1)^2
}
hist(squares, xlim = c(0,8),breaks = "fd", freq = FALSE)
#We get a chi square distribution with one degree of freedom
curve(dchisq(x,1), col = "red", add = TRUE)

#Now take k samples from a standard normal distribution and sum their squares
N <- 10^4; sumsquares <- numeric(N); k <- 6
for (i in 1:N) {
  x <- rnorm(k)
  sumsquares[i] <- sum(x^2)
}
hist(sumsquares, xlim = c(0,4*k),breaks = "fd", freq = FALSE)
#We get a chi square distribution with k degrees of freedom
curve(dchisq(x,k), col = "red", add = TRUE)

