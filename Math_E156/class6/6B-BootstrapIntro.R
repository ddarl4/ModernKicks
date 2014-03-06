#Math E-156 Script 6B-BootstrapIntro.R

#Topic 1 - A bootstrap sampling distribution
#Start with the example from section 5.1 of the textbook
#For birth weights of NC babies, we do not know the population distribution.
NCB<-read.csv("NCBirths2004.csv"); head(NCB)
BabyWt<-NCB$Weight; hist(BabyWt, breaks = "FD")

#Now suppose that, instead of a sample of 1009, all we had was a sample of 6 (the book uses 3)
Sample6<-sample(BabyWt,6); Sample6

#Pretend that this was the entire population (like an unfair die)
#We can look at the means for all possible samples of size 6 (like rolling the die 6 times)
Baby6<-expand.grid(Sample6,Sample6,Sample6,Sample6,Sample6,Sample6); head(Baby6); nrow(Baby6)

#Work with this as if it were a population data frame with 6^6 rows
x<-rowMeans(Baby6)
hist(x,breaks="FD")    #shape is similar to the original full distribution

#Alternatively, we can draw random samples as we did with dice and cards
N=10000; result<-numeric(N)
for (i in 1:N){
  result[i] <-mean(sample(Sample6,6,replace = TRUE))
}
plot1<-hist(result, breaks= "FD")

#For comparison, we can draw random samples from the full sample of 2009 babies
N=10000; big6<-numeric(N)
for (i in 1:N){
  big6[i] <-mean(sample(BabyWt,6,replace = TRUE))
}
plot2<-hist(big6, breaks= "FD")

#Google "R histogram transparent colors" for details of the following trick.
#On overlaying the histograms, observe that they have different means but similar shape
plot( plot1, col=rgb(0,0,1,1/4))  # first histogram, using all the data
plot( plot2, col=rgb(1,0,0,1/4),  add=T)  # second, made from the bootstrap

#Now use samples of size 1009 from the original data.
#We get a good approximation to the sampling distribution
N=10000; bigsample<-numeric(N)
for (i in 1:N){
  bigsample[i] <-mean(sample(BabyWt,1009,replace = TRUE))
}
hist(bigsample, breaks= "FD", probability = TRUE)
#Of course, now the central limit theorem applies
curve(dnorm(x,mean(BabyWt), sd(BabyWt)/sqrt(1009)), col = "red", add = TRUE)

#Topic 2 -- trying a bootstrap where we know the population distribution
#Example 5.1, where the sample of 50 is from the normal distribution N(23, 7^2)
curve(dnorm(x, 23, 7), from = 6, to = 39)  #figure 5.2a
abline(v = 23, col = "red", lty = 2)
my.sample <- rnorm(50, 23, 7)
hist(my.sample, breaks ="FD", freq = F) #will resemble figure 5.2b
mean(my.sample) #will be close to 23, 
abline(v = mean(my.sample), col = "blue", lty = 2)
#Overlay the population density function
curve(dnorm(x, 23, 7), from = 6, to = 39, col = "red", add = TRUE)
abline(v = 23, col = "red", lty = 2)

#Now compare the bootstrap distribution with the sampling distribution
N=10^5; my.boot<-numeric(N)
for (i in 1:N) {
  my.boot[i] = mean(sample(my.sample, 50, replace = TRUE))
}
hist(my.boot,  breaks= "FD", probability = TRUE)
curve(dnorm(x,23,7/sqrt(50)),  col = "red", add = TRUE)
abline(v = mean(my.sample), col = "blue", lty = 2)
abline(v = 23, col = "red", lty = 2)
#The shape of the bootstrap distribution is a great match (CLT)
#As usual, though, the mean is wrong
#The variance of the bootstrap distribution is determined by the variance of the sample

#We could use the bootstrap distribution to determine what to add and subtract
#if we want to generate endpoints of a confidence interval

#Calculate the amount X.add to add to the sample mean
X.add <- mean(my.boot)-quantile(my.boot,0.025); X.add #compare with 1.940265
#Calculate an amount X.sub to subtract from the sample mean
X.sub <- quantile(my.boot,0.975)-mean(my.boot); X.sub #compare with 1.940265


#Topic 3 -- the bootstrap reveals skewness in the population
#Example 5.2, where the sample is from the skewed distribution Gamma(1,1/2)
curve(dgamma(x, 1, 1/2), from = 0, to = 10) #exponential with lambda = 1/2
my.sample <- rgamma(16, 1, 1/2)    #draw 16 samples from this population
mean(my.sample) #expected mean is 2 but will vary widely
hist(my.sample,  breaks= "FD", probability = TRUE)
abline(v = mean(my.sample), col = "blue", lty = 2)
curve(dgamma(x, 1, 1/2), col = "red", add = T)
abline(v = 2, col = "red", lty = 2)   #population mean is 2

#Now compare the bootstrap and sampling distributions
N=10^5; my.boot<-numeric(N)
for (i in 1:N) {
  my.boot[i] = mean(sample(my.sample, 16, replace = TRUE))
}
hist(my.boot,  breaks= "FD", probability = TRUE)
abline(v = mean(my.boot), col = "blue", lty = 2)
#Overlay the theoretical sampling distribution
curve(dgamma(x,16,8),  col = "red", add = TRUE)
abline(v = 2, col = "red", lty = 2)
#The bootstrap data has the shape of the sampling distribution but the mean of the sample
#We can compare the third central moments to check skewness
mean((my.boot - mean(my.boot))^3)   #from the bootstrap - should be positive
integrate(function(x) dgamma(x,16,8)*(x-2)^3, 0, Inf)

#Again we can use the bootstrap distribution to determine what to add and subtract
#if we want to generate endpoints of a confidence interval

#Calculate the amount X.add to add to the sample mean
X.add <- mean(my.boot)-quantile(my.boot,0.025); X.add #compare with 0.8153184
#Calculate an amount X.sub to subtract from the sample mean
X.sub <- quantile(my.boot,0.975)-mean(my.boot); X.sub #compare with 1.134036

#In spite of the single tiny sample, the bootstrap captures the fact
#that the confidence interval should not be centered on the sample mean.

#It makes a big difference whether a couple of our 16 samples came from the tail.
#Repeat topic 3 for several different samples of 16.

