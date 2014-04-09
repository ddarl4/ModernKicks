# Larson Hogstrom - HW8
# MATH_E156 - 4/7/2014


### Part 1 

# a) Invent a way of creating a random variable X that has a chi-square 
# distribution with six degrees of freedom by sampling from an expo- 
# nential distribution. You get to choose the sample size and the value 
# of lambda. Verify your result by making a histogram of the sample means 
# and overlaying a chi-square distribution.

N <- 10^4
y.mean <- numeric(N)
for (i in 1:N) {
    y <- rexp(100,1) # origonal exponential sample
    y.mean[i] <- mean(y)
}
# create z-scores these should have a N(0,1) pattern
y.mean.normed <- (y.mean-mean(y.mean))/sqrt(var(y.mean))
hist(y.mean.normed)

## now create six of these distributions
n <- 6
N <- 10^4
y.norms <- matrix(0,N,n)
for (j in 1:n){
    y.mean <- numeric(N)
    for (i in 1:N) {
        y <- rexp(100,1)
        y.mean[i] <- mean(y)
    }
    y.norms[,j] <- (y.mean-mean(y.mean))/sqrt(var(y.mean))
}
#plot one column
hist(y.norms[,1], breaks= "FD", probability = TRUE) # these are normal
curve(dnorm(x,0,1), col = "red", add = TRUE)
# Z1^2 + Z2^2 + ... Z6^2 ~ chisq(6)
y.squares <- y.norms^2
z.sum <- rowSums(y.squares)
# plot transformed samples and plot with theoretical
hist(z.sum, breaks= "FD", probability = TRUE)
curve(dchisq(x,6), col = "red", add = TRUE)


# yDens <- density(y)
# hist(yDens)
# density <- Vectorize(function(x,y) exp(x*y)*dgamma(x,r1, lambda),vectorize.args = 'x')
# #Integrate over y to get the moment generating function
# mgfr1 <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
# curve(mgfr1(x), from = -1, to =1, lty = 1)






# b) The Student t distribution results when you divide a standard nor- 
# mal random variable by the square root of an independent, suitably rescaled, 
# chi-square random variable. Nowhere does it say the the nu- merator and 
# denomiator have to come from the same source. They only have to be independent.

# T = Z/sqrt(W/k)
# domonstrate this by example
x.sn <- rnorm(10000,0,1)
x.cs <- rchisq(10000,5)
g <- x.sn/sqrt(x.cs/5)
hist(g, breaks= "FD", probability = TRUE)
curve(dt(x,5), col = "red", add = TRUE)

# Create a variable with an approximate Student t distribution with six 
# degrees of freedom by taking the mean of 50 samples from Unif[0,1] and 
# dividing by the chi square variable that you created for part (a). Show 
# that a histogram of values from this distribution matches the built-
# in t density function on R.

N<- 10^4
unif.mean <- numeric(N)
for (i in 1:N){
    unif.mean[i] <- mean(runif(50,0,1))
}
# create N(0,1) from these means
unif.normed <- (unif.mean-mean(unif.mean))/sqrt(var(unif.mean))
hist(unif.normed, breaks= "FD", probability = TRUE) 
curve(dnorm(x,0,1), col = "red", add = TRUE)
# T = Z/sqrt(W/k)
g <- unif.normed/sqrt(z.sum/6) #z.sum is the chisq dist from above
hist(g, breaks= "FD", probability = TRUE)
curve(dt(x,5), col = "red", add = TRUE)

### part 2 

# By evaluating the gamma functions in the formula for the t density on 
# page 389, find explicit formulas for the t density function for 4 and 5 
# degrees of freedom, and show that they agree with the R function dt(x). 
my.gamma<-Vectorize(function(r) integrate(function(x) x^(r-1)*exp(-x), 0, Inf)$val)

### create t-density function using explicit formula
# f(t) = gamma(K+1/2)/ (sqrt(2pi) * gamma(k/2) * (1+(t^2)/k)^((k+1))/2)
my.t <- Vectorize(function(t,k) my.gamma((k+1)/2)/ (sqrt(k*pi) * my.gamma(k/2) * (1+(t^2)/k)^((k+1)/2)) )
# 4 df
curve(my.t(x,4), col = "red",xlim=c(-2,2))
curve(dt(x,4), col = "black",xlim=c(-2,2), lty = 2, add = TRUE)
# 5 df
curve(my.t(x,5), col = "red",xlim=c(-2,2))
curve(dt(x,5), col = "black",xlim=c(-2,2), lty = 2, add = TRUE)

# Using 50000 trials, confirm that the variance is close to k/(k − 2) and 
# attempt to estimate the fourth moment in each case.
k<-4 # define df
# density <- Vectorize(function(x,y) exp(x*y)*my.t(x,r1, lambda),vectorize.args = 'x')
# #Integrate over y to get the moment generating function
# mgf <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
# k<-4
N <- 50000
vars <- numeric(N)
M4s <- numeric(N)
for (i in 1:N) {
    x <- rt(100,k)
    vars[i] <- var(x)
    M4s[i] <- mean(x^4)
}
#plot 4th moments
hist(M4s)
# plot obs and expected variance
hist(vars, breaks="FD",xlim=c(0,10))
abline(v = k/(k-2), col = "red", lty = 2);
var.mean <- mean(vars); var.mean
var.expected <- k/(k-2); var.expected
print("the mean of the observed variances are close to the expected value")


# we can also verify using the difinition of variance for
# continious random variables:
# Var[X] = integral( (x-u)^2 f(x) ) from -inf to inf
integrate(function(x) ((x-0)^2 * my.t(x, k)), -Inf, Inf)  #properly normalized
print("this matches what we found empirically above")



curve(mgfr1(x), from = -1, to =1, lty = 1)


pr <- outer(rep(p,6),rep(p,6),"*"); pr

mgf <- function(x, Y, pr)  {
  sum(pr*(exp(x*Y)))
}
mgf.dice <- Vectorize(function(x) mgf(x, Y, pr))

### Part 3 

# It turns out that the only distribution for which the sample mean and 
# the sample variance are independent is the normal distribution. For 
# samples of size 6 from N(0,1), doing 5000 experiments, create scatter 
# plots of the square of the sample mean against the sample variance, 
# and calculate the correlation. Once you have determined whether the 
# correlation is positive or negative, offer an anecdotal explanation, 
# based on an extreme case, of why this should be so.

# Change N(0,1) to Unif[-1,1]

N<-5000; 
x.mean.square <- numeric(N)
x.var <- numeric(N)
for (i in 1:N) {
  # x <- rnorm(6,0,1)
  x <- runif(6,-1,1)
  x.mean.square[i] <- mean(x)^2
  x.var[i] <- var(x)
}
plot(x.mean.square,x.var)
cor.test(x.mean.square, x.var)
print("there is a negative correlation between the square of the means and the variance ")

# anecdotal explanation
x.extreme <- c(.9, .98, .9, .97, .9, .99)
x.ex.square <- mean(x.extreme)^2; x.ex.square
x.ex.var <- var(x.extreme); x.ex.var
print("if we pick an extreme case where all the sample values are high
  their variance will be low (because they are all close to one). ")

### part 4

NCB <- read.csv('NCBirths2004.csv'); head(NCB)
bw <- NCB$Weight

# If random variable Y is the weight of one of the these babies, 
# it might not be unreasonable to assume that the random variable 
# X = (Y − μ)/σ has a standard normal distribution.
X <- (bw - mean(bw))/sqrt(var(bw))
hist(X)
print("the standard score seems to follow a normal distribution")

# Create samples of size k = 6 that have an approximate standard 
# normal distribution by selecting six weights from the NC baby data, 
# subtracting μ and dividing by σ. Using 1000 such samples (or more if 
# your computer is fast), answer the following questions.
w.sample <- sample(bw,6)
w.standard <- (w.sample - mean(bw))/sqrt(var(bw))


# (a) If you multiply the square of the sample mean by n, does it 
# have a distribution that is approximately chi-square with one 
# degree of free- dom?
N <- 10^4
mean.square.n <- numeric(N)
for (i in 1:N) {
    w.sample <- sample(bw,6)
    mean.square.n[i] <- w.sample^2 * 6
}
hist(mean.square.n, breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

# (b) Does the sum of the squares of the samples have a distribution 
# that is approximately chi-square with k = 6 degrees of freedom?
N <- 10^4
sum.squares <- numeric(N)
for (i in 1:N) {
    w.sample <- sample(bw,6)
    sum.squares[i] <- sum(w.sample^2)
}
hist(sum.squares, breaks= "FD", probability = TRUE)
curve(dchisq(x,6), col = "red", add = TRUE)


# (c) If you multiply the sample variance by k−1, does it have a 
# distribution that is approximately chi-square with k − 1 = 5 degrees 
# of freedom?
N <- 10^4
var.times.k <- numeric(N)
for (i in 1:N) {
    w.sample <- sample(bw,6)
    var.times.k[i] <- var(w.sample) * 5
}
hist(var.times.k, breaks= "FD", probability = TRUE)
curve(dchisq(x,5), col = "red", add = TRUE)


# (d) Is the square of the sample mean uncorrrelated with the sample vari- ance?
N<-5000; 
x.mean.square <- numeric(N)
x.var <- numeric(N)
for (i in 1:N) {
  w.sample <- sample(bw,6)
  x.mean.square[i] <- mean(w.sample)^2
  x.var[i] <- var(w.sample)
}
plot(x.mean.square,x.var)
cor.test(x.mean.square, x.var)
print("")


# (e) If you divide the sample mean by the sample standard deviation,
#  does it have an approximate Student t distribution with k − 1 = 5 
#  degrees of freedom?

# As with section problem 2, you can model your calculations on the 
# ones in script 8B-Student t.


