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
    y <- rexp(100,1)
    y.mean[i] <- mean(y)
}
# create z-scores these should have a N(0,1) pattern
y.mean.normed <- (y.mean-mean(y.mean))/sqrt(var(y.mean))
hist(y.mean.normed)

## now create six of these distributions



y <- rexp(100,1)
hist(y)
yDens <- density(y)
hist(yDens)

density <- Vectorize(function(x,y) exp(x*y)*dgamma(x,r1, lambda),vectorize.args = 'x')
#Integrate over y to get the moment generating function
mgfr1 <- function(x) sapply(x, function(y) integrate(density, lower=0, upper=20, y=y)$val)
curve(mgfr1(x), from = -1, to =1, lty = 1)

# b) The Student t distribution results when you divide a standard nor- 
# mal random variable by the square root of an independent, suitably rescaled, 
# chi-square random variable. Nowhere does it say the the nu- merator and 
# denomiator have to come from the same source. They only have to be independent.

# Create a variable with an approximate Student t distribution with six 
# degrees of freedom by taking the mean of 50 samples from Unif[0,1] and 
# dividing by the chi square variable that you created for part (a). Show 
# that a histogram of values from this distribution matches the built-
# in t density function on R.

# 50 samples from Unif[0,1]
x <- runif(50,0,1)
mean(x)


### part 2 

# By evaluating the gamma functions in the formula for the t density on 
# page 389, find explicit formulas for the t density function for 4 and 5 
# degrees of freedom, and show that they agree with the R function dt(x). 
# Using 50000 trials, confirm that the variance is close to k/(k − 2) and 
# attempt to estimate the fourth moment in each case.


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
  x <- rnorm(6,0,1)
  x.mean.square[i] <- mean(x)^2
  x.var[i] <- var(x)
}
plot(x.mean.square,x.var)
cor.test(x.mean.square, x.var)
print("the first time I ran this i ")

N<-100; 
N.b <- 10000
corrs <- numeric(length(nSamples))
for (j in 1:N.b) {
    x.mean.square <- numeric(N)
    x.var <- numeric(N)
    for (i in 1:N) {
      x <- rnorm(6,0,1)
      x.mean.square[i] <- mean(x)^2
      x.var[i] <- var(x)
    }
    corrs[j] <- cor.test(x.mean.square, x.var)$estimate
}
hist(corrs)

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


