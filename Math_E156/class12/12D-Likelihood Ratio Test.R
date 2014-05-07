#Math E-156 Script 12D-LikelihoodRatioTests.R

#Topic 1 -- using likelihood ratio to design the most powerful test.

#This is example 8.18 from the textbook.
#The test uses 9 samples from an exponential distribution.
#The null hypothesis is that lambda = 8.
#The explicit alternative is that lambda = 10.
#The log of the likelihood ratio involves the sum of the 9 samples.
#This sum, as we know, has a gamma distribution.

#A messy calculation (page 235) determines the critical value for 5% type I errors.
c2 <- qgamma(0.05, 9, 8); c2
#Large values favor the null hypothesis, so the alternative hypothesis wins if sum(x) < c2
#The probability of a Type I error is of course
pgamma(c2, 9, 8)

#What is the probability of rejecting the null hypothesis if the alternative is correct?
pgamma(c2, 9, 10)  #14%, so the probabilty of a type II error is 86%
#Alas, there is no way to do any better

#Do a simulation with samples that satisfy the null hypothesis
N <= 10^4; sums <- numeric(N)
for (i in 1:N ) {
  x <- rexp(9,8)
  sums[i] <- sum(x) 
}
hist(sums)
sum(sums < c2)/N   #fraction of type I errors

#Do a simulation with samples that satisfy the alternative hypothesis
N <= 10^4; sums <- numeric(N)
for (i in 1:N ) {
  x <- rexp(9,10)
  sums[i] <- sum(x) 
}
hist(sums)
sum(sums > c2)/N   #fraction of type II errors

#The problem is that the probabilty distributions overlap. 
curve(dgamma(x,9,rate = 10), from = 0, to = 4 )
curve(dgamma(x,9,rate = 8), col = "red", add = TRUE) 
abline(v = c2, col = "blue")

#The remedy: increase the sample size from 9 to 90
curve(dgamma(x,90,rate = 10), from = 0, to = 30 )
curve(dgamma(x,90,rate = 8), col = "red", add = TRUE) 
c2 <- qgamma(0.05, 90, rate = 8); c2
abline(v = c2, col = "blue")
pgamma(c2, 90, 8)   #Probability of a type I error is again 5%
pgamma(c2, 90, 10)  #55%, so the probability of a type II error is 34%



