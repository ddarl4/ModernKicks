Math E-156 Script 5C-MaxMinMedian.R

#Topic 1 - new sampling statistics: maximum, minimum, median

#If we know the distibution and density function for the population, there are formulas (page 83) for the maximum and minimum of a sample.
#The simplest example is the uniform distribution. In this case the sampling distribution has a name-- the beta distribution.

#Draw 10000 samples of 3 from a uniform distribution on [0,1] and tally all three statistics
N <- 10^4; max3<-numeric(N); min3<-numeric(N); med3<-numeric(N)
#A trial run with our sampling
x <- runif(3); x
max(x); min(x); median(x)

#Now repeat 10000 times
for (i in 1:N) {
  x <- runif(3)
  max3[i] <- max(x)
  min3[i] <- min(x)
  med3[i] <- median(x)
}

#Distribution of the maximum
hist(max3, breaks= "FD", probability = TRUE)
curve(dbeta(x,3,1),col = "red", add = T)
#Instead, use the formula from page 83
curve(3*punif(x,0,1)^2*dunif(x,0,1), col = "green",add = T) #should overwrite the red curve
mean(max3)     #theoretical value is 3/4

#Distribution of the minimum
hist(min3, breaks= "FD", probability = TRUE)
curve(dbeta(x,1,3),col = "red", add = T)
#Instead, use the formula from page 83
curve(3*(1-punif(x,0,1))^2*dunif(x,0,1), col = "green",add = T) #should overwrite the red curve
mean(min3)  #theoretical value is 1/4

#Distribution of the median
hist(med3, breaks= "FD", probability = TRUE)
curve(dbeta(x,2,2),col = "red", add = T)
curve(6*(1-punif(x,0,1))*punif(x,0,1)*dunif(x,0,1), col = "green",add = T) #should overwrite the red curve
mean(med3)  #theoretical value is 1/2

#Topic 2 - max, min, median for an exponential distribtion, lambda = 1

N <- 10^4; max3<-numeric(N); min3<-numeric(N); med3<-numeric(N)
for (i in 1:N) {
  x <- rexp(3, rate= 1)
  max3[i] <- max(x)
  min3[i] <- min(x)
  med3[i] <- median(x)
}

#Distribution of the maximum
hist(max3, breaks= "FD", probability = TRUE)
curve(3*pexp(x,1)^2*dexp(x,1), col = "green",add = T) 
mean(max3)   #probably a challenge to calculate

#Distribution of the minimum
hist(min3, breaks= "FD", probability = TRUE)
curve(3*(1-pexp(x,1))^2*dexp(x,1), col = "green",add = T) #should overwrite
mean(min3)

#Distribution of the median
hist(med3, breaks= "FD", probability = TRUE)
curve(6*(1-pexp(x,1))*pexp(x,1)*dexp(x,1), col = "green",add = T) #should overwrite
mean(med3)

#Topic 3 - max and min for a normal distribution
#So we now have a probability model for the distribution of any "order statistic" of a known distribution

#Exotic example: seven standard normal variables
N <- 10^4; max7<-numeric(N); min7<-numeric(N); med7<-numeric(N)
for (i in 1:N) {
  x <- rnorm(7)
  max7[i] <- max(x)
  min7[i] <- min(x)
}
#Distribution of the maximum
hist(max7, breaks= "FD", probability = TRUE)
#Use the formula from page 83
curve(7*pnorm(x)^6*dnorm(x), col = "green",add = T) 

#Distribution of the maximum
hist(min7, breaks= "FD", probability = TRUE)
#Use the formula from page 83
curve(7*(1-pnorm(x))^6*dnorm(x), col = "green",add = T) 


