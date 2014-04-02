# Larson Hogstrom - HW7
# MATH_E156 - 3/31/2014

### Part 1 ### 

# Modify Wind Speeds Case Study to see if the times between earthquakes 
# can be modeled with the Weibull distrubution. Include graphs like Fig 6.4

# Hint: when searching for the shape parameter with the uniroot comand, 
# use lower=0.8, upper=1

# This function takes input shape parameter k and
# the data to compute
# (1/k)+ (1/n)*sum (log(xi)) +(1/alpha)sum xi^klog(xi)=0
# where alpha= sum xi^k.
weibull.shape <- function(k, data)
{
  numer <- colSums(outer(data, k, "^") * log(data))
  denom <- colSums(outer(data, k, "^"))
  numer/denom - 1/k - mean(log(data))
}

#-----
# This function takes input shape parameter k
# and data to compute
#  k^{th} root of (1/n) sum xi^k
# n=number of data values
weibull.scale <- function(k, data)
{
  mean(data^k)^(1/k)
}
##-----
# uniroot is a built-in R function which estimates the root
# of a function.
# Provide function, any arguments needed for function,
# and a guess of values two values around root.
# Function values must be opposite signs at lower
# and upper guess.

#Now, we do the data specific commands
Quakes <- read.csv("Quakes.csv"); head(Quakes)
td <- Quakes$TimeDiff
hist(td)

uniroot(weibull.shape, data = td, lower=0.8, upper=1)

# With estimate of shape parameter, now find estimate
# of scale parameters
weibull.scale(0.9172, td)

# Plot histogram with density curve overlap
# The prob=TRUE option scales histogram to area 1.
hist(td, main = "Distribution of times between quakes",
    prob = TRUE)
curve(dweibull(x, 0.9172, 17.34), add = TRUE, col = "blue", lwd = 2)
# ECDF plot
dev.new()
plot.ecdf(td,main = "ECDF of quake data")
curve(pweibull(x, 0.9172, 17.34), add=TRUE, col="blue",lwd=2)

# You can check your answer using the mle() function.
# mle(MLL,start = list(x = 2)) #same answer
library(stats4)
MLL <-function(shape, scale) -sum(dweibull(td, shape, scale, log = TRUE))
mle(MLL,start = list(shape = 1, scale = 15)) 
# Include the graphs that are requested, but do not take the time to 
# replicate the goodness-of-fit analysis, since that shows up in the 
# next problem.

### Part 2 ### 

# Exercise 16 on page 162. So far, all you know about the gamma distribution 
# is that it arises as the sampling distribution for the exponential 
# distribution, in which case the “shape” parameter must be an integer. 
# However, the formulas on page 398 are valid even when r is nor an integer. 
# The goodness- of-fit analysis can be modeled on the wind speed analysis

# model Service.csv wth gamma distribution
Service <- read.csv("Service.csv"); head(Service)
st <- Service$Times # waiting times in minutes

# a) Use the method of moments to estimate the parameters of the 
# gamma distribution

# E[X] = shape*scale
# Var[X] = shape*scale^2
#Calculating the first two moments from the sample is easy
M1 = mean(st); M1
M2 = mean(st^2); M2

#For an exponential distribution the variance is 1/lambda^2 (page 397)
variance <- M2 - M1^2  #this sample variance is independent of delta
# shape.bar <- M1/shape
scale.bar <- M1/shape
shape.bar <- variance/scale^2

lambda.bar = 1/sqrt(variance); lambda.bar  #estimate of lambda based just on the sample variance
#If delta = 0, then the mean and standard deviation are equal, so
delta.bar = M1 - sqrt(variance); delta.bar #estimate of delta based on both sample moments
    


### compare with mle estimate
library(stats4)
MLL <-function(shape, scale) -sum(dgamma(st, shape=shape, scale=scale, log = TRUE))
mle(MLL,start = list(shape = 1, scale = 1)) 
hist(st, prob=TRUE, main = "MLE estimate of gamma distribution")
curve(dgamma(x, 2.81, scale=.247), add=TRUE, col="blue",lwd=2)

# b) Use a goodness-of-fit test to see whether the gamma distribution is an
# adequate model for these data.
# Get the deciles
q <- qgamma(seq(.1, .9, by = .1), 2.81, scale=.247)
#range of wind
range(st)
#encompass range of wind
q <- c(0, q, 2.2)
# Get the counts in each sub-interval. The plot=F command
# repeat above but store output
count <- hist(st,breaks=q,plot=F)$counts
expected <- length(st)*.1
# compute chi-square test statistic
ChiSq <- sum((count-expected)^2/expected)
pchisq(ChiSq, df =7, lower.tail = FALSE) #agrees with the P-value on page 146
print("Conclusion: the gamma distribution, with estimated parameters, could have generated the observed wait times")

# c) Make histogram and ecdf of the data with the gamma dist. superimposed
dev.new()
plot.ecdf(st,main = "ECDF of Service Times")
curve(pgamma(x, 2.81, scale=.247), add=TRUE, col="blue",lwd=2)


### Part 3 ### 
# Karl is applying for membership in the Stats Guild. He is required 
# to submit the results of five different tests, each of which yields a 
# score in the interval [0, 1]. Since he just guesses, his score on each 
# attempt at a test is a random variable with the distribution Unif[0,1].

# Karl may not be very bright, but he is rich, and so he pays to take each 
# test θ times, submitting only the best score for each. Here are his 
# results, pasted in from a simulation that I did in R.
# X1 = 0.855, X2 = 0.891, X3 = 0.913, X4 = 0.989, X5 = 0.943. The pdf for 
# the maximum of θ samples from Unif[0,1] is f(x;θ) = θx^(θ−1), 0 ≤ x ≤ 1,θ > 0

# (a) Find the MLE of θ if it is not required to be an integer.
obs <- c(0.855, 0.891, 0.913, 0.989, 0.943)
# page 140 shows that θ.hat = -n/(sum(ln(Xi)))
n <- length(obs)
theta.hat <- -n/sum(log(obs)); theta.hat

# (b) Find the method of moments estimate of θ.
# mean of obs = integral(x*f(x;θ))
integ<-integrate(function(x) {2/x^3}, 1, Inf); integ

# (c) Find the MLE estimate of θ, requiring it to be an integer. Just 
# have R crank out the likelihood of the given results for θ = 1,2,3,··· ,N 
# choosing N large enough that you are sure that you have found the maximum.
likelihood.theta <- function(theta, data)
{
  n<- length(data)
  theta^n * prod( data^(theta-1) )
}

N <- 20; likelihood.vals <- numeric(N)
for (i in 1:N) {
  likelihood.vals[i] <- likelihood.theta(i,obs)
}
plot(likelihood.vals)
print("likelihood values peak between theta of 11 and 12. This is consistent with MLE estimate")

### part 4 ###

# Carry out a simulation in R to confirm the results of parts (a) and (d) 
# of Exercise 37 on page 164. By doing the sampling N times, you can get 
# an excellent estimate of the expectation of each of the two estimates.

# 37 let X1, x2,...,Xn be independent exponential random variables with
# parameter λ. Let X_bar = (X1 + X2)/2 be an estimator of 1/λ

# a) show that X_bar is an unbiased estimator of 1/λ
n = 10
lambda <- 12
x <- rexp(n, lambda)
lambda.hat <-1/mean(x); lambda.hat   #estimate of lambda for which the mean matches the sample mean
#Try this 1000 times to see how it does
N <- 100000; lambda.bars <- numeric(N); x.bars <- numeric(N)
for (i in 1:N) {
  x <- rexp(n, lambda)
  x.bars[i] <- mean(x[1:2])
  M1 <- mean(x)
  lambda.bars[i] <- 1/M1  #record the corresponding estmate of lambda
}
hist(x.bars)
# plot line for 1/λ
abline(v = 1/lambda, col = "red")
bias <- mean(x.bars) - (1/lambda); bias

# b) show that Var[X_bar] = 1/(2λ^2)
var(x.bars)
1/(2*lambda)^2
bias2 <- var(x.bars) - 1/(2*lambda)^2; bias2
print("Var[X_bar] seems to be a less biased estimator of 1/(2λ^2) at higher values of lambda")

### part 5 ###
# The Pareto distribution with shape 1 and scale s has density function 

# f(x) = s/(x+s)^2 ,x ≥ 0.
# ￼
# It is supported by the “actuar” package, which you probably installed ear-
# lier. If not, remove the # from the first line below. Then
#install.packages("actuar")
library(actuar)
x <- rpareto(10, 1 , 2); x
# will draw a sample of size 10 from a Pareto distribution with shape 1 
# and scale 2.

# (a) Investigate the behavior of sample means for samples of various 
# sizes drawn from this distribution, and do a plot like the one for the 
# Cauchy distribution in script 7D to show that the sample means are not 
# a consistent estimator of anything.
plot(1, xlim= c(200, 20000), ylim = c(0, 100), log = "x", type = "n")
N = 1000;  epsilon = 0.05
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  means = numeric(N);
  for (i in 1:N){
    x <- rpareto(n,1, 2)
    means[i] = mean(x)
  }
  points( rep(n, N), means)
}

# (b) Make a histogram of sample medians for samples of size 10 from the 
# Pareto distribution with shape 1, scale 2.
N = 10000;
  medians = numeric(N);
  for (i in 1:N){
    x <- rpareto(10,1, 2)
    medians[i] = median(x)
  }
hist(medians)

# (c) Invent a consistent estimator of the parameters and show that 
# it works by creating a graphic like figure 6.7 in the textbook, 
# where you display the result of taking the median of larger and 
# larger samples.

# calculate estimator with median
plot(1, xlim= c(200, 20000), ylim = c(0, 5), log = "x", type = "n")
epsilon <- .2
abline(h = c(s.true + epsilon, s.true- epsilon), col = "red")
N = 1000;  
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  medians = numeric(N);
  for (i in 1:N){
    x <- rpareto(n,1, s.true)
    medians[i] <- median(x)
  }
  points( rep(n, N), medians)
}

print("An alternative is to use the given pareto pdf with shape=1 to create 
a MLE estimator of s. To do this I took the derivative of the log-likelihood 
w.r.t s. I set this equal to zero and obtained: 0= n/s - 2*sum(1/(s+Xi)). 
This can be solved numerically to find an consistent estimator of s.")

pareto.s <- function(s, data)
{
  n <- length(data)
  n/s - 2*sum(1/(s+data))
}
s.true <- 3 # set parameter to estimate
# 
plot(1, xlim= c(200, 20000), ylim = c(0, 5), log = "x", type = "n")
epsilon <- .2
abline(h = c(s.true + epsilon, s.true- epsilon), col = "red")
N = 1000;  
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  estimates = numeric(N);
  for (i in 1:N){
    x <- rpareto(n,1, s.true)
    est <- uniroot(pareto.s, data = x, lower = 1,upper = 5)
    estimates[i] <- est$root
  }
  points( rep(n, N), estimates)
}
# Note: to do this problem you are going to have to work out the 
# distribution function for the Pareto distrbution, attempt to 
# calculate its expectation, and figure out its median. An R script 
# is not a good medium for showing this sort of mathematics, but 
# at least state your answers.
