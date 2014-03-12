#Math E-156 Script 7C-BiasEfficiency.R
#Based on pages 150-154 in the textbook

#Topic 1 - looking for an efficient estimator

#Example 6.13 from the textbook - estimate beta in Unif[0, beta] from 25 samples
#The sample mean and sample median both have expectation beta/2.
#So twice either of them is an unbiased estimator
#Multiplying the maximum by (n+1)/n also gives an unbiased estimator (observed in script 7A, proved in math notes)
#We can also solve the problem with the maximum by adding the minimum
#Finally, we can set the estimate of the population standard deviation equal to beta/sqrt(12)
#Let's determine which of these estimators of beta has the smallest variance
N<-1000;my.mean <- numeric(N); my.median <- numeric(N);my.max <- numeric(N); my.minmax <- numeric(N); my.sd <- numeric(N)
for (i in 1:N) {
  x <- runif(25, 0, 12)   #the sample
  my.mean[i] <- 2*mean(x)  #unbiased estimator
  my.median[i] <- 2*median(x)  #unbiased estimator
  my.max[i] <- (26/25)*max(x)  #unbiased estimator
  my.minmax[i] <- min(x) + max(x)  #unbiased estimator
  my.sd[i] <- sd(x)*sqrt(12)
}
#Check the means and compare standard deviation
mean(my.mean); sd(my.mean)
mean(my.median); sd(my.median)  #much worse
mean(my.max); sd(my.max)        #much better
mean(my.minmax); sd(my.minmax)  #second best
mean(my.sd); sd(my.sd)          #better than mean or median, but biased
#Looking at histograms makes it clear what is happening
par(mfrow = c(2, 3))
hist(my.mean)
hist(my.median)
hist(my.max)
hist(my.minmax)
hist(my.sd)

par(mfrow = c(1, 1))

#Mean square error lets us combine bias and efficiency
mean(my.mean); mean((my.mean-12)^2)
mean(my.median); mean((my.median - 12)^2)  #much worse
mean(my.max); mean((my.max-12)^2)        #much better
mean(my.minmax); mean((my.minmax-12)^2)        #much better
mean(my.sd); mean((my.sd-12)^2)          #bias contributes


#Topic 2 - an efficient estimator of the parameter p in a Bernoulli distribution
#Example 6.15 shows how an efficient estimator can beat an unbiased one

#We observed X successes in 16 trials. Estimate the probability p of success.
#X/n is an unbiased estimator, but it has a high mean square error(MSE) if p is small or large.
#(X+1)/18 is biased but has a lower variance if p is close to 1/2.
#The exact calculations of mean-square error are on page 156.
#A slight generalization is on the last page of the math notes.
#If p is the true parameter, we can estimate it by (x+s)/(n+t) (add s successes in t trials)
#The MSE is then (np(1-p)+(s-tp)^2)((n+t)^2)
#Using the xname parameter lets us use p rather than x as the variable name.
n<-16
curve(n*p*(1-p)/n^2, from = 0, to = 1, xlab = "p", ylab = "MSE", xname = "p") #s = t = 0
curve( (n*p*(1-p)+(1-2*p)^2)/((n+2)^2), col = "blue", add = TRUE, xname = "p") #s = 1,t = 2

#Simulation: a rookie (p = 0.3) bats 16 times in spring training.
#The unbiased estimate:
sum(dbinom( 0:16, 16, 0.3)*0:16/16)  #no bias
sum(dbinom( 0:16, 16, 0.3)*((0:16)/16 - 0.3)^2)  #the variance
0.3*0.7/16    #agrees with the theoretical variance
#With no bias, the variance is also the mean square error
#Alternative: add one hit and two at-bats
BA.hat <-sum(dbinom( 0:16, 16, 0.3)*(0:16+1)/18); BA.hat  #overestimate indicates bias
(0.3*n+1)/(n+2)    #agrees with theoretical expectation
sum(dbinom( 0:16, 16, 0.3)*((0:16+1)/(n+2) - BA.hat)^2)  #smaller variance
n*0.3*0.7/(n+2)^2     #agrees with theoretical variance
sum(dbinom( 0:16, 16, 0.3)*((0:16+1)/(n+2) - 0.3)^2)  #smaller MSE also

#We can do even better with an estimator that is unbiased for p = 1/3
#Add one hit and three at-bats to the observed data
curve( (n*p*(1-p)+(1-3*p)^2)/((n+3)^2), col = "red", add = TRUE, xname = "p")  #s=1, t=3
BA.hat <-sum(dbinom( 0:16, 16, 0.3)*(0:16+1)/19); BA.hat  #very small bias
(0.3*n+1)/(n+3)    #agrees with theoretical expectation
sum(dbinom( 0:16, 16, 0.3)*((0:16+1)/(n+3) - BA.hat)^2)  #smallest variance yet
n*0.3*0.7/(n+3)^2     #agrees with theoretical variance
sum(dbinom( 0:16, 16, 0.3)*((0:16+1)/(n+3) - 0.3)^2)  #smallest MSE

#Press this idea further and you will reinvent Bayesian statistics!











