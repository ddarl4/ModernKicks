# Larson Hogstrom - HW5
# MATH_E156 - 3/3/2014

#### Part 1 ###
# 1. Page 93, exercise 10.
# in 2000 census 28.6% of adults have high school diploma
# In a random sample of 800 adults, what is the prob that betweeen 220
# and 230 (inclusive) people have a HS diploma.
n<-800
p<-.286
variance <- n*p*(1-p)
expected <- n*p; expected
# use CLT approximation with continuity correction
dist1 <- pnorm(c(230,220), expected, sqrt(variance), lower.tail = FALSE) #CLT approximation
clt <- diff(dist1); clt
# compare to the exact probability (use pbinorm in R)
dist2 <- pbinom(c(220,230),size=800,prob=.286)
exact <- diff(dist2); exact

#### Part 2 ###
# 2. Page 93, exercise 14. This is a twin of exercise 13, one of the section problems.
# X1...X9, i.i.d. from N(7,3^2)
# Y1...Y12, i.i.d. from N(10,5^2)
# let W = X_bar - Y_bar

# a) Give the exact sampling distribution of W
sampling_stdv = sqrt(3^2/9+5^2/12); sampling_stdv
print('W = X_bar + Y_bar ~ N(17,1.75^2)')

# b) Simulate the sampling distribution of W in R and plot your results
# (adapt code from the previous exercise). Check that the simulated mean and the 
# standard error are close to the theoretical mean and standard error
W <- numeric(10000)
for (i in 1:10000) {
    x <- rnorm(9, 7, 3)
    y <- rnorm(12, 10, 5)
    W[i] <- mean(x) + mean(y)
}
hist(W, freq = FALSE)
# theoretical mean and standard error
curve(dnorm(x, mean=17, sd=1.75),from =0, to = 30, col = "red", add=TRUE)
simMean <- mean(W); simMean
simSE <- sqrt(var(W)); simSE
print('simulated mean and SE are close to the theoretical')

# c) Use your simulation to find P(W< -1.5). Calculate the exact answer and
# compare
p.simulated <- sum(W < -1.5) / length(W); p.simulated
p.theoretical <- pnorm(-1.5,mean=17, sd=1.75); p.theoretical

#### Part 3 ###
# 3. Page 94, exercise 16.
# For random variable X from a Uniform distribution:
# E[X] = (a+b)/2
# VAR[X] = (b-a)^2/12

# a)
# X ~ Unif[40,60]
# Y ~ Unif[45,80]
# Compute the expected value and varaince of X + Y 
# let W = X + Y
w.mean <- (40+60)/2 + (45+80)/2; w.mean
w.var <- (60-40)^2/12 + (80-45)^2/12

# b) compare simmulated distribution of X and Y and compare to the 
# theoretical mean and variance
X <- runif(10000, 40, 60)
Y <- runif(10000, 45, 80)
total <- X + Y
hist(total,freq=FALSE)
curve(dnorm(x, mean=w.mean, sd=sqrt(w.var)),col = "red", add=TRUE)

# simulated mean and variance
total.mean <- mean(total); total.mean
total.SE <- sqrt(var(total)); total.SE
# theoretical mean and variance
w.mean
w.var
print('the way I have calculated it, the theoretical variance seems higher than the observed')

# c) use simulated distribution to find P(X+Y <90)
p.90 <- sum(total < 90)/ length(total); p.90

#### Part 4 ###
# 4. Page 95, exercise 24(b). You will have to work part (a) to do 
# the required comparison. Include a brief explanation of how you came 
# up with the theoretical expectation of Xmin.

# let X1, X2,...,Xn, iid ~ Exp(λ) with pdf f(x) = λe^(λx), λ>0, x>0
# a) find the pdf fmin(x) for the sample minimum Xmin. Recognize this as the pdf
# of a known distribution
# fmin(x) = n(1-F(x))^(n-1)*f(x)
print('for exponential: fmin(x) = nλe^(-nλx)')
print('so the fmin(x) for an exponential function is another exponential with \
    the parameter nλ')

# b) simulate the sampling distribution of Xmin of samples of size n=25
# from the exponential distribution with λ = 7. Compare the theoretical 
# expected value of Xmin to the simulated expected value.
W <- numeric(10000)
for (i in 1:10000) {
    x <- rexp(25, 7)
    W[i] <- min(x)
}
hist(W, freq = FALSE)
curve(dexp(x, rate=7*25),col = "red", add=TRUE)

#### Part 5 ###
# 5. Plot the moment-generating function for two dice, in two different ways.

# (a) Use outer() to get the probability mass function for two dice, 
# then write an R function for the moment generating function of this prob- 
# ability mass function and plot a graph of it. (See script 5P for the 
# “vectorize” trick.)
Y <-outer(c(1,2,3,4,5,6),c(1,2,3,4,5,6),"+"); Y
p <- 1/6
pr <- outer(rep(p,6),rep(p,6),"*"); pr

mgf <- function(x, Y, pr)  {
  sum(pr*(exp(x*Y)))
}
mgf.dice <- Vectorize(function(x) mgf(x, Y, pr))
curve (mgf.dice, from = -1, to = 1, col = "magenta", lty = 2)

# (b) Write an R function for the moment generating function of the prob- 
# ability mass function for a single die and plot a graph of its square.
Y <-c(1,2,3,4,5,6); Y
p <- 1/6
pr <- rep(p,6); pr

mgf_squared <- function(x, Y, pr)  {
  sum(pr*(exp(x*Y)))^2
}
mgfsq.dice <- Vectorize(function(x) mgf_squared(x, Y, pr))
curve (mgfsq.dice, from = -1, to = 1, col = "blue", lty = 2)





