#Math E-156 Scgript 7P-Proof7.R

#Topic 1 - A famous limit that you met in a calculus course
n <- 0:500
plot(n, (1+1/n)^n, type = "l") #the limit is e
#Adding an extra term that goes to zero faster than 1/n makes no difference
points(n, (1+1/n- 1/n^2)^n, col = "red", type = "l") #the limit is still e

#Changing the coefficient of 1/n does make a difference
x<- 2
plot(n, (1+x/n)^n, type = "l") #the limit is e^x
abline(h = exp(2), col = "blue")
#Adding an extra term still makes no difference
points(n, (1+x/n- x^2/n^2)^n, col = "red", type = "l") #the limit is still e^2


#Topic 2 - the mgf for the sum of many discrete random variables
#Make a discrete random variable with expectation 0, variance 1
Y1 = c(-1, 1); pr = c(0.5, 0.5)
#Use our mgf function from Proof 5
mgf <- function(x, Y, pr)  {
  sum(pr*(exp(x*Y)))
}
mgf1 <- Vectorize(function(x) mgf(x, Y1, pr))
#"Vectorize" lets the function act on a vector, produce a vector (required by curve())
curve (mgf1, from = -1, to = 1)
#Now reduce the variance be 1/2 but add two together by squaring the mgf - sum has variance 1
Y2 = Y1/sqrt(2)
mgf2 <- Vectorize(function(x) mgf(x, Y2, pr))
curve (mgf2(x)^2, from = -1, to = 1, col = "red", add = TRUE) #similar
#Now make the variance be 1/10 and add ten together by rasing the mgf to the 10th power
Y10 = Y1/sqrt(10)
mgf10 <- Vectorize(function(x) mgf(x, Y10, pr))
curve (mgf10(x)^10, from = -1, to = 1, col = "red", add = TRUE) #similar
#The limit is the mgf for the N(0,1) distribution
curve (exp(x^2/2), from = -1, to = 1, col = "blue", add = TRUE)

#Symmetry around zero is not essential
Y1 = c(-1/2, 2); pr = c(0.8, 0.2)
sum(Y1*pr); sum(Y1^2*pr)   #expectation 0, variance 1
mgf1 <- Vectorize(function(x) mgf(x, Y1, pr))
curve (mgf1, from = -1, to = 1)
#Now make the variance be 1/10 and add ten together by rasing the mgf to the 10th power
Y10 = Y1/sqrt(10)
mgf10 <- Vectorize(function(x) mgf(x, Y10, pr))
#Now make the variance be 1/100 and add 100 together by rasing the mgf to the 100th power
Y100 = Y1/sqrt(100)
mgf100 <- Vectorize(function(x) mgf(x, Y100, pr))

curve (mgf100(x)^100, from = -1, to = 1, col = "magenta", add = TRUE) #similar
#The limit is the mgf for the N(0,1) distribution
curve (exp(x^2/2), from = -1, to = 1, col = "blue", add = TRUE)

#Topic 2 - the mgf for the sum of many continuous random variables
#Use a uniform distribution with expectation 0, variance 1
density <- Vectorize(function(x,y) exp(x*y)*dunif(x, min=-sqrt(3), max=sqrt(3)), vectorize.args = 'x')
mgf <- function(x) sapply(x, function(y) integrate(density, lower=-2, upper=2, y=y)$val)
curve(mgf(x), from = -1, to =1)

#We can find the expectation and variance by taking derivatives
h <- 0.00001; M1<-(mgf(h)-mgf(0))/h; M1   #expectation
h <- 0.0001; M2<-(mgf(h)+mgf(-h) - 2*mgf(0))/h^2; M2   #second moment
M2 - M1^2     #the variance 

#Again, divide by 10 (to decrease the variance by 100) and raise the mgf to the 100th power
density <- Vectorize(function(x,y) exp(x*y)*dunif(x, min=-sqrt(3)/10, max=sqrt(3)/10), vectorize.args = 'x')
mgf <- function(x) sapply(x, function(y) integrate(density, lower=-1, upper=1, y=y)$val)
curve(mgf(x)^100, from = -1, to =1, col = "red", add = TRUE)

#The limit is the mgf for the N(0,1) distribution
curve (exp(x^2/2), from = -1, to = 1, col = "blue", add = TRUE)












