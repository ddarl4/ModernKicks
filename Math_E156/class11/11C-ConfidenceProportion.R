#Math E-156 Script 11C-Confidence Proportion.R

#Topic 1 -- using a normal approximation to the binomial distribution

#Here is a simulation based on Example 7.17
#Assume that p=68% of the population favor the death penalty for murder and we poll n=1308 of them
n<- 1308; p = 0.68

#Check our methodolgy by using a single sample
X <- rbinom(1, n, p); X #example in book has 899
p.hat <- X/n; p.hat

#The following relies on the messy algebra from page 191
#It works because the variance of a binomial distribution is np(1-p).
#Notice that sqrt(p.hat*(1-p.hat) is used below to calculate the spread.
#We calculate L and U so that P(p < L) =.05 and P(p > U) =.05
q <- qnorm(0.95); q #will lead to a 90% confidence interval
#These are the two terms from the quadratic formula on the third page of math notes
center <- (n*p.hat+ q^2/2)/(n+q^2); center 
spread <- (q*sqrt(p.hat*(1-p.hat)/n + q^2/(4*n^2))/(1+q^2/n)); spread
L <- center-spread
U <- center+spread
L; U #the confidence interval
#90% of the time the true proportion p will lie between L and U
#Yet again, this test has been automated
prop.test(X, n, conf.level = 0.9, correct = FALSE)

#Now for the simulation
counter <- 0; N <- 1000; q <- qnorm(0.95); 
plot(x =c(0.6, 0.75), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:N) {
  X <- rbinom(1, n, p)
  p.hat <- X/n
  center <- (n*p.hat+ q^2/2)/(n+q^2)
  spread <- (q*sqrt(p.hat*(1-p.hat)/n + q^2/(4*n^2))/(1+q^2/n))
  L <- center-spread
  U <- center+spread
  if (L < p && U > p) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100){
    segments(L, i, U, i)
    points(p.hat, i, col = "red", pch = ".", cex = 4) 
  }
}
abline (v = p, col = "red") #vertical line at true proportion
counter/1000 #what fraction of the time did our confidence interval include the true proportion?

