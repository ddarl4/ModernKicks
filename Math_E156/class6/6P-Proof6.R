#Math E-156 Script 6P-Proof 6.R

#Topic 1 -- density functions for normal distributions

#Some normal distributions with various means
curve(dnorm(x), from = -5, to = 5)  #default: mu = 0, sigma = 1
curve(dnorm(x,2), col = "red", add = T)  #mu = 2, default: sigma = 1
curve(dnorm(x,-1), col = "blue", add = T)  #mu = -1, default: sigma = 1

#Some normal distributions with different variances
curve(dnorm(x), from = -5, to = 5)  #default: mu = 0, sigma = 1
curve(dnorm(x,0, sqrt(2)), col = "red", add = T)  #mu = 0,  sigma^2 = 2
curve(dnorm(x,0, 2), col = "blue", add = T)  #mu =0, sigma^2 = 4

#Topic 2 -- moment generating function for a normal distribution

#Moment generating function for the default N(0,1)
density <- Vectorize(function(x,y) exp(x*y)*dnorm(x),vectorize.args = 'x')
mgfN01 <- function(x) sapply(x, function(y) integrate(density, lower=-20, upper=20, y=y)$val)
curve(mgfN01(x), from = -1, to =1, lty = 2)
#We can check the functional form graphically
curve(exp((x^2)/2), from = -1, to =1, col = "red", add = TRUE)

#Moment generating function for N(1,2)
density <- Vectorize(function(x,y) exp(x*y)*dnorm(x,1, sqrt(2)),vectorize.args = 'x')
mgfN12 <- function(x) sapply(x, function(y) integrate(density, lower=-20, upper=20, y=y)$val)
curve(mgfN12(x), from = -1, to =0.5, lty = 2)
#We can check the functional form graphically
curve(exp(x+x^2), from = -1, to =0.5, col = "red", add = TRUE)

#We can find the expectation and variance by taking derivatives
h <- 0.00001; M1<-(mgfN12(h)-mgfN12(0))/h; M1   #expectation
h <- 0.0001; M2<-(mgfN12(h)+mgfN12(-h) - 2*mgfN12(0))/h^2; M2   #second moment
M2 - M1^2     #the variance

#If we add three independent N(1,2) random variables we get N(3,6)
curve(mgfN12(x)^3, from = -1, to =0.5, lty = 2)
density <- Vectorize(function(x,y) exp(x*y)*dnorm(x,3, sqrt(6)),vectorize.args = 'x')
mgfN36 <- function(x) sapply(x, function(y) integrate(density, lower=-20, upper=20, y=y)$val)
curve(mgfN36(x), from = -1, to =0.5, col = "red", add = TRUE)





