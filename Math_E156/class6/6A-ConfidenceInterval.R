Math E-156 Script 6A-ConfidenceInterval.R

#This topic is explained very well in section 7.1.1, after it has been used in Chapter 5.
#We apply it to examples 5.1 and 5.2 from the textbook

#Topic 1 -- sampling from a normal distribution
#In this case we know everything about the population distribution and its sampling distribution.
#We are drawing a sample of 50 from the normal distribution N(23, 7^2).
#The sample mean has a normal distribution with mean mu = 23, variance 7^2/50.
#We will add on x.add so that x.top = X.bar + X.add will be greater than the true mean 97.5% of the time.
#In other words, X.bar + x.add - mu will be negative only 2.5% of the time
#So P(X.bar < mu -x.add) = .025 
#Thus mu - x.add = qnorm(0.025,23,7/sqrt(50)) and
X.add <- 23 -qnorm(0.025,23,7/sqrt(50)); X.add

counter <- 0
plot(x =c(20,26), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(50, 23, 7) #random sample
  U <- mean(x) + X.add #usually greater than the true mean
  if (23 < U) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) points(U, i, pch= 23)
}
abline (v = 23, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our statistic U exceed the true mean?

#Now do the same thing on the low end
#We will subtract x.sub so that x.bar - x.sub will be less than the true mean 97.5% of the time.
#In other words, X.bar - x.sub - mu will be positive only 2.5% of the time
#So P(X.bar < mu + x.sub) = .975
#Thus mu + x.sub = qnorm(0.025,23,7/sqrt(50)) and
X.sub <- qnorm(0.975,23,7/sqrt(50)) - 23; X.sub
counter <- 0
plot(x =c(20,26), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(50, 23, 7) #random sample
  L <- mean(x) - X.sub #usually less than the true mean
  if (L < 23) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) points(L, i, pch= 22)
}
abline (v = 23, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time was our statistic L less than the true mean?

#Note carefully: the value 23 is  a parameter, not a random variable.
#Our quantities L and U are random variables, calculated from the sample
#If we plot them both, they specify a confidence interval that includes 23 95% of the time

counter <- 0
plot(x =c(18,28), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(50, 23, 7) #random sample
  L <- mean(x) - X.sub #usually less than the true mean
  U <- mean(x) + X.add #usually greater than the true mean
  if (L < 23 && 23 < U) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) {
    points(L, i, pch= 22)
    points(U, i, pch= 23)
    segments(L, i, U, i)
  }    
}
abline (v = 23, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?


#Draw a sample of size 16 from a gamma distribution with shape 1, rate 1/2
#The sampling distribution for the mean is gamma with shape 16, rate 8 (proved in Appendix B.9)
#The sampling distribution has mean 2 and varaince 0.5

#This sampling distribution is skewed
curve(dgamma(x,shape = 16,rate = 8), xlim = c(0,6))
#Plot lines that exclude the extreme 2.5% on either side
abline(v = qgamma(0.025,shape = 16,rate = 8), col = "red")
abline(v = qgamma(0.975,shape = 16,rate = 8), col = "red")
#If we knew only that the sampling distribution had mean 2, variance 1/4, we could gamble on the CLT
abline(v = qnorm(0.025,2,0.5), col = "blue")
abline(v = qnorm(0.975,2,0.5), col = "blue")

#Calculate the amount X.add to add to the sample mean
X.add <- 2-qgamma(0.025,shape = 16,rate = 8); X.add
#Calculate an amount X.sub to subtract from the sample mean
X.sub <- qgamma(0.975,shape = 16,rate = 8)-2; X.sub

#Again we can generate some confidence intervals
counter.low <- 0; counter.high <- 0;
plot(x =c(0,5), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rgamma(16, 1, 1/2) #random sample
  L <- mean(x) - X.sub #usually less than the true mean
  U <- mean(x) + X.add #usually greater than the true mean
  if (L < 2 ) counter.low <- counter.low + 1 #count +1 if we were correct
  if (2 < U) counter.high <- counter.high + 1 #count +1 if we were correct
  if(i <= 100) {
    points(L, i, pch= 22)
    points(U, i, pch= 23)
    segments(L, i, U, i)
  }    
}
abline (v = 2, col = "red") #vertical line at true mean
counter.low/1000 ; counter.high/1000   #both should be close to .975

#If we used the CLT approximation we would miss on one side more than on the other

#Calculate the amount X.add to add to the sample mean
X.add <- 2-qnorm(0.025,2,0.5); X.add
#Calculate an amount X.sub to subtract from the sample mean
X.sub <- qnorm(0.975,2,0.5)-2; X.sub

#Again we can generate some confidence intervals
counter.low <- 0; counter.high <- 0;
plot(x =c(0,5), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rgamma(16, 1, 1/2) #random sample
  L <- mean(x) - X.sub #usually less than the true mean
  U <- mean(x) + X.add #usually greater than the true mean
  if (L < 2 ) counter.low <- counter.low + 1 #count +1 if we were correct
  if (2 < U) counter.high <- counter.high + 1 #count +1 if we were correct
  if(i <= 100) {
    points(L, i, pch= 22)
    points(U, i, pch= 23)
    segments(L, i, U, i)
  }    
}
abline (v = 2, col = "red") #vertical line at true mean
counter.low/1000 ; counter.high/1000   #unequal, although the average will be close to .975

#Question: what can we do if the sample is too small for the CLT to be trusted,
#but we do not know the sampling distribution of the population?
#Answer: use a bootstrap estimate of the sampling distribution


