#Math E-156 Script 4P-Proof 4.R

#Topic 1 - Poisson distribution as the limit of the binomial distribution 

#n becomes large, p becomes small, lambda = np stays constant.
#Example with lambda = 5
plot(0, xlim = c(0,15), ylim= c(0, 0.2), type = "n") #empty plot with scales established
#in each case the expectation is 5
points(0:15, dbinom(0:15, 25, 0.2), "l", col = "green")
points(0:15, dbinom(0:15, 50, 0.1), "l", col = "magenta")
points(0:15, dbinom(0:15, 100, 0.05), "l", col = "blue")
points(0:15, dbinom(0:15, 200, 0.025), "l", col = "red")
points(0:15, dpois(0:15, 5), "l", col = "black")
#The mean for each binomial distribution is np = 5 -- same for the Poisson
#The variance for each binomial distribution is np(1-p)
#The variance for the limiting Poisson distribution, as lambda approaches zero, is also np = lambda
#Calculate the expectation and variance of a Poisson distribution
lambda <- 5
M1 <- sum(0:30*dpois(0:30, lambda)); M1   #expectation is lambda
M2 <- sum((0:30)^2*dpois(0:30, lambda));M2   #second moment is lambda^2 + lambda
M2 - M1^2   #variance is also lambda

#Topic 2 -- the sum of two Poisson distributions
#Consider Poisson distributions with lambda = 5 and lambda = 2 respectively
#Calculate the probability of getting sums from 0 to 15 respectively
#The probability of a sum greater the 15 is negligible
sumPois <- numeric(15)
for (i in 1:15) {
  sumPois[i] <- sum(dpois(0:i, 5)*dpois(i:0, 2))
}
plot(0, xlim = c(1,15), ylim= c(0, 0.2), type = "n") #empty plot
points(1:15, sumPois, "l", col = "red")
#Compare with a Poisson distribution with parameter lambda = 7
points(1:15, dpois(1:15, 7), "l") #perfect agreement




