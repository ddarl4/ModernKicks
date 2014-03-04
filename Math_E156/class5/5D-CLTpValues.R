# Math E-156 Script 5D-CLTpValues.R
#Topic 1 -- using the central limit theorem to estimate probabilities

#The central limit theorem says that for large enough samples the distribution of the sum or mean is approxiamtely normal
#All that matters about the population from which we sample is the mean and the variance.
#If we assume that the sampling distribution is normal, we can use the CLT to estmate probabilities.

#Textbook example 4.7 - draw 30 samples from a gamma distribution with shape 5, rate 2
#With this high a value for shape, the gamma distribution is already somewhat normal
#It is the sum of 5 exponential distributions, each with mean 1/2 and varaince 1/4
#So its mean is 5/2 and its variance is 5/4

curve(dgamma(x,shape = 5, rate = 2), from = 0, to = 10)  #the gamma density function- skewed
curve(dnorm(x,5/2, sqrt(5/4)), col = "red", add = TRUE)  #normal with same mean and variance

#When we sum 30 samples, we are using a gamma distribution with shape 150
curve(dgamma(x,shape = 150,rate = 2), from = 40, to = 110)   #much less skewed
curve(dnorm(x,150/2, sqrt(150/4)), col = "red", add = TRUE)  #pretty good match!

#To estimate the probability of a sum > 90 (same as book's mean > 3), use the normal as an approximation
pnorm(90, 150/2, sqrt(150/4), lower.tail = FALSE) #agrees with the book
#Of course, we could get the exact answer from the gamma distribution
pgamma(90,shape = 150,rate = 2, lower.tail = FALSE)

#Or we could do a simulation
N = 10000; my.sums = numeric(N)
for (i in 1:N) {
  my.sums[i] = sum(rgamma(30, shape =5, rate = 2))
}
mean(my.sums > 90) #result should be closer to the exact gamma result than the normal estimate

#If we were not so far out on the tail the CLT would have done better
#Let's estimate the probability for the sum to be > 80
pnorm(80, 150/2, sqrt(150/4), lower.tail = FALSE) #CLT approximation

#We could get the exact answer from the gamma distribution
pgamma(80,shape = 150,rate = 2, lower.tail = FALSE)

#Or we could do a simulation
N = 10000; my.sums = numeric(N)
for (i in 1:N) {
  my.sums[i] = sum(rgamma(30, shape =5, rate = 2))
}
mean(my.sums > 80) 

#-

#Textbook example 4.9 -- probability of getting 160 or fewer heads in 300 tosses
#Bernoulli distribution for one toss with p = 0.5 has mean 1/2, variance p(1-p) = 1/4

#The exact sum of 300 samples is binomial
counts = numeric(301)
for (i in 0:300) {
  counts[i] = dbinom(i, 300, 0.5)
}
#The mean is 150, the variance is 75: plot the normal approximation
curve(dnorm(x,150, sqrt(75)), from=100, to=200, col = "red")
#Now overlay the actual distribution
barplot(counts, space = 0,width = 1, xlim = c(100,200),add = TRUE)

#What is the probability of getting 160 or fewer heads?
pbinom(160, 300, 0.5) #the exact answer
#The obvious CLT approximation does nor work all that well
pnorm(160, 150, sqrt(75))  #too small

#The solution, for exact 160 (binomial),include an interval of width 1 centered on 160
pnorm(160.5, 150, sqrt(75))   #much better agreement

#Topic 3 --continuity correction
#This "continuity correction" becomes important when the counts are smaller.
#Textbook section 4.3.2: estimate probability of 3 or 4 heads in 10 tosses
#The distribution has mean 5 and variance 5/4
pbinom(4,10,0.5) - pbinom(2, 10, 0.5) #exact

#Even with only a sum of 10 Bernoulli variables, we can try using the CLT
pnorm(4,5,sqrt(5/4)) - pnorm(2,5,sqrt(5/4)) # very poor approximation

#If we center the interval on 3 and 4, we do better
pnorm(4.5,5,sqrt(5/4)) - pnorm(2.5,5,sqrt(5/4))#good continuity correction

#Here is how to plot a histogram with the bars centered on the integer values
my.binomial<-NULL; 
my.binomial$breaks <- (- 0.5) : (10.5)
my.binomial$counts <- dbinom(0:10, 10, 0.5)
attr(my.binomial,"class") <- "histogram"
plot(my.binomial,main="Histogram of binomial distribution") 
#The mean is 5, the variance is 5/2: plot the normal approximation
curve(dnorm(x,5, sqrt(5/2)), from  =0, to= 10, col = "red", add = TRUE)

#Topic 4 - beware of using the CLT with a small number of samples from a skewed population
#The tables in Appendix C give the expectation and variance for many well-known distributions.

#Draw 10 samples from a geometric distribution with p = 0.4: mean = 1.5, variance = 3.75
#The sum of the samples is a negative binomial distribution
my.negbinomial<-NULL; 
my.negbinomial$breaks <- (- 0.5) : (50.5)
my.negbinomial$counts <- dnbinom(0:50, 10, 0.4)
attr(my.negbinomial,"class") <- "histogram"
plot(my.negbinomial,main="Histogram of negative binomial distribution") #distinctly skewed

#The mean is 15, the variance is 37.5: plot the best normal approximation
curve(dnorm(x,15, sqrt(37.5)), from  =0, to= 50, col = "red", add = TRUE)  #skewness is zero

#Let's estimate the probability for the sum to be > 20
pnorm(19.5, 15, sqrt(37.5), lower.tail = FALSE) #CLT approximation with continuity correction

#We could get the exact answer from the negative binomial distribution
pnbinom(20,10,0.4, lower.tail = FALSE)       #CLT approximation was poor

#Or we could do a simulation
N = 10000; my.sums <- numeric(N)
for (i in 1:N) {
  my.sums[i] = sum(rgeom(10, 0.4))
}
mean(my.sums > 20) #much closer than the CLT approximation
 




