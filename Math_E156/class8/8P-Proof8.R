#Proof 8
#Define the gamma function as an integral
my.gamma<-Vectorize(function(r) integrate(function(x) x^(r-1)*exp(-x), 0, Inf)$val)
my.gamma(c(2:8))  #(r-1)!
my.gamma(0.5)^2
curve(my.gamma(x), from = 0.1, to = 4.5)
abline(v = c(1:4), col = "blue")
abline(h = c(1,2,6), col = "blue")
my.gamma(5)/my.gamma(4)   #same as for factorials
my.gamma(5.34)/my.gamma(4.34)   #works also for non-integers

#The gamma fuction is used to normalize the pdf for the gamma distribution
my.dgamma <- Vectorize(function(x, r, lambda) (lambda^r/my.gamma(r))*x^(r-1)*exp(-lambda*x), vectorize.args = "x")
#r = 1 gives the exponential distribution
curve(my.dgamma(x,1,1), from = 0, to = 8)
curve(dexp(x,1),col = "red", add = T)
integrate(function(x) my.dgamma(x, 3.4, 2), 0, Inf)  #properly normalized
curve(my.dgamma(x,3.4,2), from = 0, to = 8)
curve(dgamma(x,3.4,2), col = "red", add = T) #same as the built-in function

