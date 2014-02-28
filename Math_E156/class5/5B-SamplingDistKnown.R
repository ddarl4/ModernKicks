Math E-156 Script 5B-SamplingDistKnown.R

#Topic 1 -- the sampling distribution of an exponential distribution is a gamma distribution

#The gamma ditrbution is the sum of exponential distributions
#Textbook example 4.2 -- draw 1000 samples of size 100 from exponential with rate lambda = 1/15
curve(dexp(x, rate = 1/15), from =0, to = 100)   #Figure 4.4 on page 80

#First we will simulate the sampling distribution
N = 1000; my.means<-numeric(N)
for (i in 1:N) {
  x<-rexp(100, rate = 1/15)
  my.means[i] <- mean(x)
}
pl1<-hist(my.means,30, prob = TRUE)   #plot a histogram of the sample means

#In this case the sampling distribution for the sum is known to be a gamma distribution
#Since we have sample means we need to rescale both axes (tricky)
curve(100*dgamma(100*x, shape=100, rate = 1/15), col = "red", add = TRUE)

#Alternatively, we could sample from the gamma distribution
#Since we were looking at sample means we must divide the gamma sample (a sum) by 100
gamma.sample<-rgamma(1000, shape=100, rate = 1/15)/100  #sample from a gamma distribution
pl2<-hist(gamma.sample,30)


#Here is a clever way to overlay the histograms by using transparent colors
plot( pl1, col=rgb(0,0,1,1/4))  # first histogram
plot( pl2, col=rgb(1,0,0,1/4),  add=T)  # second

#Here is a similar trick for overlaying the quantile plots
q1 <- qqnorm(my.means, plot.it = FALSE)
q2 <-qqnorm(gamma.sample, plot.it = FALSE)
plot(range(q1$x, q2$x), range(q1$y, q2$y), type = "n")
points(q1, col=rgb(0,0,1,1/4))
points(q2, col=rgb(1,0,0,1/4))


#Topic 2 - the sampling distribution of sums from a Poisson distribution is another Poisson distribution
#Textbook example 4.6 -- draw 10000 samples of size 10 from Pois(2)
N = 10000; my.sums<-numeric(N)
for (i in 1:N) {
  x<-rpois(10, 2)
  my.sums[i] <- sum(x)
}
mean(my.sums)
barplot(table(my.sums))

#Equivalent: draw 10000 samples from Pois(20)
poisson.sample=rpois(N,20)
mean(poisson.sample)
barplot(table(poisson.sample))
barplot(rbind(table(my.sums), table(poisson.sample)), beside = T, col = c("red","blue"))

#Topic 3 - if you take the mean of samples from a normal distribution, the result is also normal
#An extreme case, 10000 samples of size 2 from N(0,1)
N = 10000; my.sums<-numeric(N)
for (i in 1:N) {
  x<-rnorm(2)
  my.sums[i] <- sum(x) 
}

hist(my.sums,breaks = "fd", probability = TRUE)

#The sum is a normal distribution with variance 1 + 1 = 2.
curve(dnorm(x,0,sqrt(2)), col = "red", add = TRUE)

#Of course, the sample mean and sample variance are not quite equal to 0 and 2.
mean(my.sums); var(my.sums) #the sample variance

#Remarkably (and easily proved) you can even sum two samples from different normal distributions
#First sample has mean 4, variance 3; second has mean 6, variance 16
N = 10000; my.sums<-numeric(N)
for (i in 1:N) {
  my.sums[i]<-rnorm(1,4,3)+rnorm(1,6,4)
}

hist(my.sums,breaks = "fd", probability = TRUE)
curve(dnorm(x,10,5), col = "red", add = TRUE)
#The sum has mean 4+6 = 10; variance 9 + 16 = 25

#Here are the estimates of the sample mean and sample variance
mean(my.sums); var(my.sums)  
