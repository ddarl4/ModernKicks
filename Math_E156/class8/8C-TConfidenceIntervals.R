Math E-156 Script 8C-TConfidenceIntervals.R

#Topic 1 - review of the case where we know the population variance but not the mean
#End of Section 7.1, page 169
#The population distribution is specified: N(25,4^2), but we know only the variance
#From our sample of size 30, we form the sample mean
#Then we compute an interval of 1.96 standard errors on either side and hope it includes the population mean.
#Let's try it 1000 times (example on page 169)
counter <- 0
plot(x =c(22,28), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(30, 25, 4) #random sample
  L <- mean(x) + qnorm(0.025) * 4/sqrt(30) #usually less than the true mean
  U <- mean(x) + qnorm(0.975) * 4/sqrt(30) #usually greater than the true mean
  if (L < 25 && U > 25) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = 25, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?
qnorm(c(0.025, .975)) #quantiles are slightly less than than 2 standard deviations


#Topic 2 - the case where we have to estimate the standard deviation also
#Section 7.1.2 -- we do not know the standard deviation either
#Repeat the same example, but use the sample standard deviation as an estimate for the population
#In this case the rescaled variable is known to have a Student t distribution
counter <- 0
plot(x =c(22,28), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(30, 25, 4) #random sample
  L <- mean(x) + qt(0.025, 29) * sd(x)/sqrt(30) #usually less than the true mean
  U <- mean(x) + qt(0.975, 29) * sd(x)/sqrt(30) #usually greater than the true mean
  if (L < 25 && U > 25) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = 25, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?
qt(c(0.025, .975), 29) #quantiles are more than 2 standard deviations

#This process can all be automated
t.test(x, conf.level = .95)$conf
#Create a new sample, get a different interval
x <-rnorm(30, 25, 4) #random sample
t.test(x, conf.level = .95)$conf

#Topic 3 -- t confidence intervals with real-world data

#With real data we usually have just one sample.
#We cannot ask for 999 other samples to see how well our confidence interval does.
#Example 7.6 with birth weights of baby girls.
NCB <- read.csv("NCBirths2004.csv")
girls<- subset(NCB, select = Weight, subset = Gender =="Female", drop = TRUE)
t.test(girls, conf.level = .95)

#If the distribution is not normal but is not skewed, we come out OK.

#We got a pretty good t distribution by sampling from a uniform distribution.
#Repeat the confidence interval calculation with a uniform distribution
counter <- 0
plot(x =c(22,28), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
x <-runif(30, 25-4*sqrt(3), 25+4*sqrt(3)) #random sample with mean 25, variance 16
mean(x); var(x)
for (i in 1:1000) {
  x <-runif(30, 25-4*sqrt(3), 25+4*sqrt(3)) #random sample from population with mean 25, variance 16
  L <- mean(x) + qt(0.025, 29) * sd(x)/sqrt(30) #usually less than the true mean
  U <- mean(x) + qt(0.975, 29) * sd(x)/sqrt(30) #usually greater than the true mean
  if (L < 25 && U > 25) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = 25, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?
qt(c(0.025, .975), 29) 

#Run the automated t test using just a single sample
x <-runif(30, 25-4*sqrt(3), 25+4*sqrt(3)) #random sample with mean 25, variance 16
t.test(x, conf.level = .95)

#To see what can go wrong, repeat with a skewed distribution
#With a gamma distribution, for samples of 30 the CLT really does not apply!
plot(x =c(20,30), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
x <-rgamma(30, 40,8/5) #random sample with mean 25, variance 16
mean(x); var(x)
#Use Student's t to get a confidence interval
mean(x) + qt(0.025, 29) * sd(x)/sqrt(30); mean(x) + qt(0.975, 29) * sd(x)/sqrt(30)
#Run the automated t test
t.test(x, conf.level = .95)    #it gives the same numbers.

#In this case we know the population distribution and can see how well Student does.
means <- numeric(1000);lower = numeric(1000); upper = numeric(1000)
for (i in 1:1000) {
   x <-rgamma(30, 40,8/5) #random sample from population with mean 25, variance 16
   means[i] = mean(x) #accumulate statistics about the sample mean
#Estimate a confidence interval from Student t
   lower[i] <- mean(x) + qt(0.025, 29) * sd(x)/sqrt(30) #usually less than the true mean
   upper[i] <- mean(x) + qt(0.975, 29) * sd(x)/sqrt(30) #usually greater than the true mean
   if(i <= 100) segments(lower[i], i, upper[i], i)
}
abline (v = 25, col = "red") #vertical line at true mean
#Quantiles for the sampling distribution of the population
qnt<-quantile(means, c(.025, .975)); qnt ; sum(qnt)/2
#These are close to the mean confidence interval
mean(lower); mean(upper); (mean(lower)+ mean(upper))/2

sum(means > qnt[2])/1000 #what fraction of the time did our upper quantile fall below the true mean?
sum(25 > upper)/1000 #what fraction of the time did Student's confidence interval fall below the true mean?
sum(means < qnt[1])/1000 #what fraction of the time did our lower quantile lie above the true mean?
sum(25 < lower)/1000 #what fraction of the time did Student's confidence interval lie above the true mean?
 
#The assumption behind the Student t distribution was not satisfied.
#We did very well by looking at more samples, but that is cheating.
#Perhaps we could use a bootstrap, but not today.

