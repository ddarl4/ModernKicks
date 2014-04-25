#Math E-156 Script 11D-Bootstrap t.R

#Topic 1 -- bootstrapping the Student t statistic
#For a skewed distribution we do not want to assume the sampling distribution is normal
#We use the t statistic - sqrt(n)(X - mu)/S but estimate its distribution by bootstrapping
#The book's example 7.21 - arsenic in Bangladesh wells
Bangladesh <- read.csv("Bangladesh.csv"); head(Bangladesh)
Arsenic <- Bangladesh$Arsenic
hist(Arsenic)                #long, flat tail tothe right
xbar <- mean(Arsenic); xbar  #sample mean
S <- sd(Arsenic); S          #sample standard deviation
n <- length(Arsenic)
SE <- S/(sqrt(n)) ; SE       #sample standard error

#Check our methodology with a single bootstrap resample
x <-sample(Arsenic, size = n, replace = TRUE) #resample
Tstar<-(mean(x) - xbar)/(sd(x)/sqrt(n)); Tstar #a t statistic
#Now we will estimate the distribution of the t statistic

N = 10^4; Tstar = numeric(N) #vector of t statistics
means = numeric(N); StdErrs = numeric(N)
for (i in 1:N) {
  x <-sample(Arsenic, size = n, replace = TRUE)
  Tstar[i] <-(mean(x) - xbar)/(sd(x)/sqrt(n))
  means[i] = mean(x); StdErrs[i] = sd(x)/sqrt(n)
}

#For this bootstrap sampling distibution none of the usual assumptions are valid
#The standard error is not independent of the mean
plot(means, StdErrs)
#The correlation looks high, and it is!
cor(means, StdErrs)

#The bootstrap t statistic is not even approximately normal or Student t
qqnorm(Tstar)
qqline(Tstar)

#The bootstrap t statistic is skewed to the left, not to the right!
hist(Tstar, breaks = "FD",probability  = TRUE)
#A Student t curve matches the mean and variance but nothing else!
curve(dt(x,n-1), col = "red", add = TRUE) 

#The bootstrap quantiles are different from the t quantiles
q<-quantile(Tstar, c(.025, .975), names = FALSE); q #will differ slightly from page 198
qStud <- qt(c(.025, .975), n-1); qStud

#To get a confidence interval, use the bootstrap quantiles along with the sample mean and standard deviation
L <- xbar - q[2]*SE; U <- xbar - q[1]*SE; L; U

#Here, for comparison, is the bootstrap percentile confidence interval
quantile(means, c(.025, .975))

#Here is the confidence interval based on the assumption of a Student t distribution
xbar - qStud[2]*SE; xbar - qStud[1]*SE

#Topic 2 - checking the bootstrap t confidence interval by simulation

#We can do a simulation by pretending that the observed data are the population
counter <- 0; N = 10000; badL <- 0; badU <- 0
plot(x =c(50, 200), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:N) {
  x <-sample(Arsenic, size = n, replace = TRUE)
  Tstar[i] <-(mean(x) - xbar)/(sd(x)/sqrt(n))
#Calculate a confidence interval using the results from this sample
  L = mean(x)- q[2]* sd(x)/sqrt(n)
  U = mean(x)- q[1]* sd(x)/sqrt(n)
  if (L < xbar && U > xbar) counter <- counter + 1 #count +1 if we were correct
  if (L > xbar) badL <- badL + 1  #lower limit was wrong
  if (U < xbar) badU <- badU + 1  #upper limit was wrong
  if(i <= 100){
    segments(L, i, U, i)
    points(mean(x), i, col = "red", pch = ".", cex = 4) 
  }
}
abline (v = xbar, col = "red") #vertical line at true mean
counter/N #what fraction of the time did our confidence interval include the true mean
#In spite of the skewed data, this confidence interval misses equally on either side
badL/N    #what fraction of the time was L too large?
badU/N    #what fraction of the time was U too small?

