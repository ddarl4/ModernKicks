#Math E-156 Script 3P-Proof 3.R

#Topic 1 - Sample from a Bernoulli distribution
#A Bernoulli random variable for a .300 hitter
#We could use a probability vector with 10 rows, all equally likely
atBats <- c(rep("Hit", 3), rep("Out",7)); atBats
#By sampling with replacement we can simulate a season
x <- sample(atBats, 500, replace = TRUE)
mean(x == "Hit")   #the batting average in season 1
x <- sample(atBats, 500, replace = TRUE)
mean(x == "Hit")   #the batting average in season 2
x <- sample(atBats, 500, replace = TRUE)
mean(x == "Hit")   #the batting average in season 3
#A simpler way is to use the built-in binomial distribution
#A binomial random variable is the sum of n Bernoulli variables
#We can simulate an entire season with a single trial
x <- rbinom(500, 1, 0.3); mean(x) #season 1
x <- rbinom(500, 1, 0.3); mean(x)  #season 2
x <- rbinom(500, 1, 0.3); mean(x)  #season 3
#We can estimate the mean and variance of the sampling distribution
N <- 10^4; hits <- numeric(N)
for (i in 1:N) {
  hits[i] <- sum(rbinom(500, 1, 0.3))
}
hist(hits, breaks = "FD", freq = FALSE) #display probabilities
mean(hits); 150
mean(hits^2) - mean(hits)^2; 150*0.7   #population variance

#Topic 2 - Using the binomial distribution
#The built-in binomial distribution has the exact probability density function
#Use a low-level plot function to overlay it on the histogram
points(0:500, dbinom(0:500, 500, 0.3), col = "red", type = "h")

#We can use it to simulate 10000 full seasons without explicit sampling
hist(rbinom(N, 500, 0.3), breaks = "FD", freq = FALSE)
points(0:500, dbinom(0:500, 500, 0.3), col = "red", type = "h")
#From the sampling we can estimate the expectation and variance
Y <- rbinom(N, 500, 0.3); mean(Y)   #estimate of expectation
var(Y)                              #estimate of population variance


#We can also calculate the population expectation and variance directly
sum(dbinom(0:500, 500, 0.3)* 0:500); 0.3 * 500 #expected hits
sum(dbinom(0:500, 500, 0.3)* (0:500)^2) - sum(dbinom(0:500, 500, 0.3)* 0:500)^2