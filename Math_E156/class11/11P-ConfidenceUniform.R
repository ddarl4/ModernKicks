#Math E-156 Script 11P-ConfidenceUniform.R

#Topic 1 -- a location parameter for a uniform distribution

#We choose theta = 3 as the center for our uniform distribution.
#Draw a single sample X from Unif[theta-1, theta+1]
#Then X - theta has the distribution Unif[-1, 1].
#It is a pivotal statistic and theta is a location parameter.
#Let's construct L and U for a 90% confidence interval
q1 <- qunif(.05, min = -1, max = 1); q1   #didn't need R to get this!
q2 <- qunif(.95, min = -1, max = 1); q2

#Test our methodology on a single sample
theta <- 3
X <- runif(1,theta-1,theta+1); X
#We know that P(X - theta < q2) = .95 so P(X - q2 < theta) = .95
L <- X - q2; L
#We know that P(X - theta > q1) = .95 so P(X - q1 > theta) = .95
U <- X - q1; U

#Now do the simulation
N <- 2000; missLow <-0 ; missHigh <-0; correct <-0
for(i in 1:N) {
  X <- runif(1,theta-1,theta+1);
  L <- X - q2
  U <- X - q1
  if (theta < L) {missLow <- missLow + 1}
  else if (theta > U) {missHigh <- missHigh + 1}
  else correct <- correct + 1 
}
missLow; missHigh; correct   #expect to see 100, 100, 1800



#Topic 2 -- a scale parameter for a uniform distribution
#This is example 7.13, the "German tank problem"
#Collect n samples from a distribution that is uniform in [0, theta]
#We know that ((n+1)/n) * max(x) is a good estimate of theta
#The distribution of max(x)/theta is ny^(n-1)
#This happens to be a beta distribution with alpha = n, beta = 1
#Since the distribution is independent of theta, max(x)/theta is a pivotal statistic
#In this case, theta is a scale parameter.

#Let's do a simulation, using theta = 100 and n = 10
theta <- 100; n <- 10
#Get the appropriate quantiles for a 95\% confidence interval
q1 <- qbeta(.025, n, 1); q2 <- qbeta(.975, n, 1)

#As usual, try everything out for a single sample
X <-max(runif(n, min = 0, max = 100)) #largest of n random samples
        
#Show the distribution, quantiles, and pivotal statistic
curve(dbeta(x, n,  1), from = 0, to = 1) 
abline(v = q1,col = "blue")
abline(v = q2,col = "blue)
abline(v = X/theta,col = "red")

estimate = X*(n+1)/n; estimate #unbiased estimate of theta from this sample
#We know that P(X/theta < q2) = .95 so P(X/q2 < theta) = .95
L <- X/q2; L
#We know that P(X/theta > q1) = .95 so P(X/q1 > theta) = .95
U <- X/q1; U
#Now the interval [L, U] has a 95% chance of including theta
estimate; (L+U)/2 #the estimate is not in the center of the confidence interval

#Now do the simulation with 4000 trials
N<- 4000; missLow <-0 ; missHigh <-0; correct <-0
plot(x = c(50,150), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:N) {
  X <-max(runif(n, min = 0, max = 100))
  estimate = ((n+1)/n) * X #estimate of theta from this sample
  L <- X/q2 #usually less than theta
  U <- X/q1 #usually greater than theta
  if (theta < L) {missLow <- missLow + 1}
  else if (theta > U) {missHigh <- missHigh + 1}
  else correct <- correct + 1 
  if(i <= 100){
    segments(L, i, U, i) #the confidence interval
    points(estimate, i, col = "red", pch = ".", cex = 4) #the estimate of theta from the data
  }
}
abline (v = 100, col = "blue") #vertical line at true theta
missLow; missHigh; correct   #expect to see 100, 100, 3800


