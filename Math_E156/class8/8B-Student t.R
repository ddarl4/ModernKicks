# 8- Student t
#Sum of two standard normal random variables
N<-10000; means <-numeric(N); vars<-numeric(N); i <-1
for (i in 1:N) {
  x <- rnorm(2); x
  means[i] <- mean(x) 
  vars[i]<- var(x)
}
#The means are normally distributed
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(2)), col = "red", add = TRUE)
#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
hist(vars, xlim = c(0,10), breaks= "FD", probability = TRUE)
curve(dchisq(x,1), col = "red", add = TRUE)
cor(means,vars) #should be zero for independent random variables
#Suppose that we know the mean is zero, but we do not know the standard devaiation
#Then to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(2)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, 1), col = "red", add = TRUE) #it is the well-known Student t distribution
#this distribution has much fatter tails than the normal distribution
mean(student); var(student) #variance might be enormous
qqnorm(student) #not at all normal in the tails

#Sum of three standard normal random variables
N<-10000; means <-numeric(N); vars<-numeric(N); i <-1
for (i in 1:N) {
  x <- rnorm(3); x
  means[i] <- mean(x) 
  vars[i]<- var(x)
}
#The means are normally distributed
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(3)), col = "red", add = TRUE)
#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
hist(2*vars, xlim = c(0,10), breaks= "FD", probability = TRUE)
curve(dchisq(x,2), col = "red", add = TRUE)
cor(means,vars) #should be zero for independent random variables
#Suppose that we know the mean is zero, but we do not know the standard devaiation
#Then to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(3)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, 2), col = "red", add = TRUE) #it is the well-known Student t distribution
#this distribution has much fatter tails than the normal distribution
mean(student); var(student) #variance might be enormous
qqnorm(student) #not at all normal in the tails

#Sum of k standard normal random variables
N<-10000; means <-numeric(N); vars<-numeric(N); k<-10
for (i in 1:N) {
  x <- rnorm(k); x
  means[i] <- mean(x) 
  vars[i]<- var(x)
}
#The means are normally distributed
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(k)), col = "red", add = TRUE)
#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
hist((k-1)*vars, xlim = c(0,10*sqrt(k)), breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE)
cor(means,vars) #should be zero for independent random variables
#Suppose that we know the mean is zero, but we do not know the standard devaiation
#Then to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(k)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #it is the well-known Student t distribution
#this distribution has much fatter tails than the normal distribution
mean(student); var(student) #variance is small if k >3
qqnorm(student) #starts to look normal for large values of k

#Let's repeat, using a uniform distribution with mean 0, variance 1
N<-10000; means <-numeric(N); vars<-numeric(N); k<-3
for (i in 1:N) {
  x <- runif(k, -sqrt(3), sqrt(3)); x
  means[i] <- mean(x) 
  vars[i]<- var(x)
}
#The means are normally distributed if k is large enough for the CLT to apply
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(k)), col = "red", add = TRUE)
#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
hist((k-1)*vars, xlim = c(0,10*sqrt(k)), breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE) #no longer chi square
cor(means,vars) #should be zero for independent random variables
#Suppose that we know the mean is zero, but we do not know the standard devaiation
#Then to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(k)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #it is the well-known Student t distribution
#this distribution has much fatter tails than the normal distribution
mean(student); var(student) #variance is small if k >3
qqnorm(student) #starts to look normal for large values of k

#Try again, using an exponential distribution with mean 0, variance 1
N<-10000; means <-numeric(N); vars<-numeric(N); k<-100
for (i in 1:N) {
  x <- rexp(k, 1)-1
  means[i] <- mean(x) 
  vars[i]<- var(x)
}
#The distribution is skewed so it takes large k for the CLT to apply
hist(means, breaks = "FD",probability = TRUE)
curve(dnorm(x,0,1/sqrt(k)), col = "red", add = TRUE)#a poor match
#What about the sample variance?
mean(vars) # 1: it is an unbiased estimator of the variance
# hist((k-1)*vars, xlim = c(0,10*sqrt(k)), breaks= "FD", probability = TRUE)
hist((k-1)*vars, breaks= "FD", probability = TRUE)
curve(dchisq(x,k), col = "red", add = TRUE) #no longer chi square
cor(means,vars) #mean and smaple variance are no longer independent!
#Suppose that we know the mean is zero, but we do not know the standard devaiation
#Then to try to get a variable with variance 1, we divide by the sample standard deviation
student<-sqrt(k)*means/sqrt(vars)
hist(student, breaks= "FD", xlim = c(-10, 10), probability = TRUE)
curve(dt(x, k-1), col = "red", add = TRUE) #no longer the Student t distribution
#this distribution has much fatter tails than the normal distribution
mean(student); var(student) #variance is small if k >3
qqnorm(student) #not normal, even for fairly large values of k

