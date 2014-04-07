Math E-156 Script 8B-Student t.R
#"Student" (William Sealy Gosset) worked as a statistician and chemist for the Guinness brewery.
#His experiments with yields of various strains of barley led to data with high, unknown variance.
#Data was expensive: it could take an entire growing season on a farm to get one data point!
#Read the article by J. F. Box to get the full story.
#Then glance at the original paper the Gosset published as "Student"
#It was a trade secret that Guinness employed statistical techniques.

#Topic 1 - the sum of two standard normal random variables
#Generate one sample of size 2 from a standard normal distribution
x <- rnorm(2); x
x.bar <- mean(x); x.bar           #unbiased estimate of the population variance  
var(x)                            #unbiased estimate of the population variance
(x[1]-x.bar)^2 + (x[2]-x.bar)^2   #remember - we divide by N-1 = 1, not by 2
#The following two quantities are equal
x[1]^2 + x[2]^2         #sum of squares of independent normal random variables
var(x) + (sqrt(2)*x.bar)^2      #the second term is also the square ofa normal random variable
#Generate lots of samples of size 2 from a standard normal distribution
N<-10000; means <-numeric(N); vars<-numeric(N)
sq1 <- numeric(N);  sumsq <- numeric(N) ; sqbar <-numeric(N)
for (i in 1:N) {
  x <- rnorm(2); x
  means[i] <- mean(x) 
  vars[i]<- var(x)     #estimate of the population variance
  sq1[i] <- x[1]^2
  sumsq[i] <- x[1]^2 + x[2]^2
  sqbar[i] <- 2*mean(x)^2
}

#Confirm that we have unbiased estimators of the population mean and variance
mean(means) # close to 0: it is an unbiased estimator
mean(vars) #close to 1: it is an unbiased estimator
#We already know that the sampling distribution of the means is normal, but with variance 1/2
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(2)), col = "red", add = TRUE)

#The square of a normal random variable has a chi square distribution df = 1
hist(sq1, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#Twice the square of the sample mean also has a chi square distribution df = 1
hist(sq1, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#The sum of squares of two independent normal random variables has a chi square distribution df = 2
hist(sumsq, xlim = c(0,10), breaks= "FD", probability = TRUE)
curve(dchisq(x,2), col = "red", add = TRUE)

#The sample variance and the square of the sample mean are uncorrelated (in fact, they are independent)
cor(vars, sqbar)

#We earlier saw that x[1]^2 + x[2]^2 = var(x) + (sqrt(2)*x.bar)^2 
#This random variable (chi square with df = 2) is the sum of two chi-square variables with df = 1
#It is also (on the right) the sum of two independent random variables
#The second one is chi square with df = 1 and so is the first -- Student's great discovery!

hist(vars, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#Now suppose that we have just a single sample of 2
#we know that the population mean is zero, but we do not know the standard deviation
#We want to rescale the sample mean so that is is a random variable with variance 1
#If we know that the population variance is 1, we just multiply by sqrt(2)
#If we do not know the population variance, we divide by the sample standard deviation (our unbiased estimate)
student<-sqrt(2)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, 1), col = "red", add = TRUE) #it is the famous Student t distribution
#this distribution has much fatter tails than the normal distribution
curve(dcauchy(x), col = "blue", add = TRUE) #it is also the infamous Cauchy distribution!
mean(student); var(student) #variance might be enormous
qqnorm(student) #not at all normal in the tails


#Topic 2 - the sum of three standard normal random variables
N<-10000; means <-numeric(N); vars<-numeric(N) ; sumsq <- numeric(N); sqbar<- numeric(N); zeroes <- numeric(N)
for (i in 1:N) {
  x <- rnorm(3); x
  means[i] <- mean(x) 
  vars[i]<- var(x)
  sumsq[i] <- x[1]^2 + x[2]^2 + x[3]^2
  sqbar[i] <- 3*mean(x)^2
  zeroes[i] <- x[1]^2 + x[2]^2 + x[3]^2 - (2*var(x) + (sqrt(3)*mean(x))^2)
}

#Now x[1]^2 + x[2]^2 + x[3]^2 = 2*var(x) + (sqrt(3)*mean(x))^2 
head(zeroes)
#The left-hand side is chi-square with df = 3
hist(sumsq, xlim = c(0,15), breaks= "FD", probability = TRUE)
curve(dchisq(x,3), col = "red", add = TRUE)

#The rescaled square of the mean is chi-sqare with 1 degree of freedom
hist(sqbar, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#The square of the sample mean is independent of the sample variance
cor(sqbar, vars)



#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
hist(2*vars, xlim = c(0,10), breaks= "FD", probability = TRUE)
curve(dchisq(x,2), col = "red", add = TRUE)

#Again, to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(3)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, 2), col = "red", add = TRUE) #a Student t distribution with df = 2
mean(student); var(student) #mean should be close to zero but variance might be enormous
qqnorm(student) #not at all normal in the tails

#Topic 3 - the sum of k standard normal random variables with mean mu, variance sigma^2

k <- 10; mu <- 3; sigma <- 2    #any values will do
#First we try with one sample
x <- rnorm(k, mu, sigma);x
x.bar <- mean(x); x.bar   #the sample mean(expectation is mu)
S.2 <- var(x); S.2  #the sample variance(expectation is sigma^2)
sum((x-mu)^2)/sigma^2     #sum of squares of k independent standard normal variables
(k-1)*S.2/sigma^2 + k*(x.bar - mu)^2/sigma^2  #algebra says this is equal to the above

#Now repeat the experiment 10000 times
N<-10000; xbars <-numeric(N); S.2s<-numeric(N) ; sumsq <- numeric(N); sqbar<- numeric(N); zeroes <- numeric(N)
for (i in 1:N) {
  x <- rnorm(k, mu, sigma)
  x.bar <- mean(x); xbars[i] <- x.bar #compute and save the sample mean
  S.2 <- var(x); S.2s[i] <- S.2  #compute and save the sample variance
  sumsq[i] <- sum((x-mu)^2)/sigma^2     #sum of squares of k independent standard normal variables
  sqbar[i] <- k*(x.bar - mu)^2/sigma^2  #also the square of a standard normal variable
  zeroes[i] <- sumsq[i] - (k-1)*S.2/sigma^2 - sqbar[i] #algebra shows this should be zero
}

#First check that our algebraic identity is satisfied
head(zeroes)

#The expectation of x.bar is mu
mean(xbars)

#The expectation of S.2 is sigma^2
mean(S.2s)

#sumsq should be chi-square with k degrees of freedom
hist(sumsq, xlim = c(0,4*k), breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE)

#The rescaled square of the mean should be chi-square with 1 degree of freedom
hist(sqbar, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#The square of the sample mean is independent of the sample variance
#So any function of the mean is uncorrelated with the sample variance
cor(sqbar, S.2s)    #Student proved that this must be zero

#So the rescaled sample variance should also be chi square, with k-1 degrees of freedom


#Now we can make a random variable with the Student t distribution
#In the numerator we need a standard normal random variable
hist(sqrt(k)*(xbars-mu)/sigma, breaks= "FD", probability = TRUE)
curve(dnorm(x), col = "red", add = TRUE)

#For the denominator we need to start with a chi square random variable.
#This is the rescaled sample variance.
hist((k-1)*S.2s/sigma^2, xlim = c(0,4*(k-1)), breaks= "FD", probability = TRUE)
curve(dchisq(x,k-1), col = "red", add = TRUE)  #it is chi-square with df = k-1

#To make the denominator we divide by k-1 and take the square root

#Form the ratio: both numerator and denominator have a factor of sigma^2, which drops out
student<-(sqrt(k)*(xbars-mu)/sqrt(S.2s))
hist(student, breaks= "FD", xlim = c(-6, 6), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #a Student t distribution with df = k-1
mean(student); var(student) #mean should be close to zero, variance is k-1/(k-3) if k >3
qqnorm(student) #differs from a normal distribution in the tails

#Topic 4 - the sum of k independent random variables from a distribution that is not normal.

#The proof for the Student t distribution only works if the samples are drawn from a normal distribution
#That is the only case where the sample mean is independent of the sample variance!
#But we can "Studentize" any sample mean by dividing by the sample standard deviation

#Let's try this, sampling from a uniform distribution on [0,1]
N<-10000; xbars <-numeric(N); S.2s<-numeric(N) ; sumsq <- numeric(N); sqbar<- numeric(N); zeroes <- numeric(N)
mu <- 0.5; sigma <- sqrt(1/12)   #the theoretical values
for (i in 1:N) {
  x <- runif(k, 0,1)
  x.bar <- mean(x); xbars[i] <- x.bar #compute and save the sample mean
  S.2 <- var(x); S.2s[i] <- S.2  #compute and save the sample variance
  sumsq[i] <- sum((x-mu)^2)/sigma^2     #sum of squares of k independent standard normal variables
  sqbar[i] <- k*(x.bar - mu)^2/sigma^2  #also the square of a standard normal variable
  zeroes[i] <- sumsq[i] - (k-1)*S.2/sigma^2 - sqbar[i] #algebra shows this should be zero
}

#First check that our algebraic identity is satisfied
head(zeroes)   #not the problem -- this is always true

#The expectation of x.bar is mu
mean(xbars)

#The expectation of S.2 is sigma^2, which is 1/12 for the uniform distribution on [0,1]
mean(S.2s)

#sumsq is no longer chi-square with k degrees of freedom -- requires a normal distribution
hist(sumsq, xlim = c(0,4*k), breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE)

#IF the CLT applies, the rescaled square of the mean should be chi-square with 1 degree of freedom
hist(sqbar, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#The square of the sample mean is no longer independent of the sample variance
#So the square of the mean is now correlated with the sample variance
cor(sqbar, S.2s)    #Student proved that this is zero only for a normal distribution

#The rescaled sample variance is no longer chi square, with k-1 degrees of freedom

#We can still make a Studentized random variable
#In the numerator we use a variable with mean 0, variance 1
hist(sqrt(k)*(xbars-mu)/sigma, breaks= "FD", probability = TRUE)
curve(dnorm(x), col = "red", add = TRUE)

#For the denominator we start with the rescaled sample variance.
hist((k-1)*S.2s/sigma^2, xlim = c(0,4*(k-1)), breaks= "FD", probability = TRUE)
curve(dchisq(x,k-1), col = "red", add = TRUE)  #it is chi-square with df = k-1

#To make the denominator we again divide by k-1 and take the square root

#Form the ratio: both numerator and denominator have a factor of sigma^2, which drops out
student<-(sqrt(k)*(xbars-mu)/sqrt(S.2s))
hist(student, breaks= "FD", xlim = c(-6, 6), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #resembles a t distribution with df = k-1
mean(student); var(student); (k-1)/(k-3) #mean should be close to zero, variance is k-1/(k-3) if k >3
qqnorm(student) #differs from a normal distribution in the tails

#The problem gets much worse if we start with a skewed distribution
#Try again, using an exponential distribution with lambda = 1
N<-10000; xbars <-numeric(N); S.2s<-numeric(N) ; sumsq <- numeric(N); sqbar<- numeric(N); zeroes <- numeric(N)
k <- 100  #use a large sample because of the skewness
mu <- 1; sigma <- 1   #the theoretical values
for (i in 1:N) {
  x <- rexp(k, 1)
  x.bar <- mean(x); xbars[i] <- x.bar #compute and save the sample mean
  S.2 <- var(x); S.2s[i] <- S.2  #compute and save the sample variance
  sumsq[i] <- sum((x-mu)^2)/sigma^2     #sum of squares of k independent standard normal variables
  sqbar[i] <- k*(x.bar - mu)^2/sigma^2  #also the square of a standard normal variable
  zeroes[i] <- sumsq[i] - (k-1)*S.2/sigma^2 - sqbar[i] #algebra shows this should be zero
}

#First check that our algebraic identity is satisfied
head(zeroes)   #not the problem -- this is always true

#The expectation of x.bar is mu
mean(xbars)

#The expectation of S.2 is sigma^2, which is 1 for the exponential distribution
mean(S.2s)

#sumsq is no longer chi-square with k degrees of freedom -- requires a normal distribution
hist(sumsq, xlim = c(0,4*k), breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE)

#IF the CLT applies, the rescaled square of the mean should be chi-square with 1 degree of freedom
hist(sqbar, xlim = c(0,5), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)

#The square of the sample mean is no longer independent of the sample variance
#So the square of the mean is now correlated with the sample variance
cor(sqbar, S.2s)    #Student proved that this is zero only for a normal distribution

#The rescaled sample variance is no longer chi square, with k-1 degrees of freedom

#We can still make a Studentized random variable
#In the numerator we use a variable with mean 0, variance 1
hist(sqrt(k)*(xbars-mu)/sigma, breaks= "FD", probability = TRUE)
curve(dnorm(x), col = "red", add = TRUE)

#For the denominator we start with the rescaled sample variance.
hist((k-1)*S.2s/sigma^2, xlim = c(0,4*(k-1)), breaks= "FD", probability = TRUE)
curve(dchisq(x,k-1), col = "red", add = TRUE)  #it is not chi-square with df = k-1

#To make the denominator we again divide by k-1 and take the square root

#Form the ratio: both numerator and denominator have a factor of sigma^2, which drops out
student<-(sqrt(k)*(xbars-mu)/sqrt(S.2s))
hist(student, breaks= "FD", xlim = c(-6, 6), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #resembles a t distribution with df = k-1
mean(student); var(student); (k-1)/(k-3) #mean should be close to zero, variance is k-1/(k-3) if k >3
qqnorm(student) #differs from a normal distribution in the tails

