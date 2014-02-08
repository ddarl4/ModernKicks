#Math E-156, script 2P-Proof2.R
#Background for Proof 2
#Here is a vector whose mean is clearly 5
v<-3:7; v; mean(v)

#The vectorized operations of R let us square it
w<-v*v; w; mean(w)
#Now we can calculate the variance
mean(w)-mean(v)^2
#Or we can do it straight from the definition of variance
mean((v-mean(v))^2)

#Try using the built-in function in R
var(v) 
#As noted on page 4 of the first R tutorial, we need to multiply by 5/4 to get the population variance.
#The function var() computes the "sample variance"
#It assumes that v is a random sample from a larger population
#The expectation of this sample variance is equal to the population variance.

#We are going to use v as the population and sample from it, so var() is inappropriate for v!
#It is appropriate for the samples that we make, though.

#Make a vector of 5 samples from this population.
sample5<-sample(v,5,replace=TRUE); sample5; var(sample5)
#Keep repeating this operation
var(sample(v,5,replace=TRUE))
var(sample(v,5,replace=TRUE))
var(sample(v,5,replace=TRUE))
var(sample(v,5,replace=TRUE))
#We get a different answer every time: the "sample variance" is a random variable.

#Next, make a vector of sample variances. Here is the wrong way:
svars<-rep(var(sample(v,5,replace=TRUE)),100); svars

#We need to do a little programming
N <- 100; svars<-numeric(N) #initialize an empty vector
for (i in 1:N) {
  svars[i]=var(sample(v,5,replace=TRUE))
}
#This loop generates 100 different independent random samples
svars
hist(svars)
mean(svars) #expected to be close to 2, not to 2.5

#If the samples are large enough their expected variance is close to the population variance
#Try samples that range in size from 50 to 50000
#First lay out the axes for the plot (logarithmic x axis)
plot(1, xlim= c(40,40000), ylim = c(1.4, 2.6), log = "x", type = "n")
#Theory says that we can make the sample variances almost all lie within epsilon of 2.
#We just need to use large samples. 
#Set up a "target band" on the plot.
epsilon = .05; abline(h = c(2 - epsilon, 2 + epsilon), col = "red")
N = 1000 #this may need to  be smaller on a slow computer
for (n in c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)){
  svars = numeric(N);
  for (i in 1:N){
    svars[i] <- var(sample(v,n,replace=TRUE))
  }
  points( rep(n, N), svars) #use n as the x value for plotting
}

#Now try the same thing with some real data
#With Flight Delays we arguably have the entire population
FDelay<-read.csv("FlightDelays.csv"); head(FDelay)
delays<-FDelay$Delay   #extract a numeric vector
hist(delays)    #skewed to the right
mean(delays)
length(delays)  #so large that n vs. n-1 makes almost no difference
var(delays) #not quite right
mean((delays-mean(delays))^2) #the correct population variance

#Again we can look at samples of 5, taken without replacement
sample5<-sample(delays,5); sample5
var(sample5)
#We get a different answer every time: the "sample variance" is a random variable.
var(sample(delays,5))
var(sample(delays,5))
var(sample(delays,5))
var(sample(delays,5))
#In this case the assumptions of proof 2 are not quite satisfied.
#We never chose the same flight twice in a sample, so the flights in a sample are not independent

#With lots of repetitions of a small sample we get close to the population variance
#We sample with replacement so that we are summing independent random variables
#Now we are sampling 5 flights from the population and should use
var(sample(delays,5,replace = TRUE))
 
N<- 100000; svars<-numeric(N) #initialize; use 10000 on a slow computer
for (i in 1:N) {
  svars[i]=var(sample(delays,5,replace = TRUE))
}
head(svars)    #some small, some large
hist(svars, xlim = c(0, 30000))    #long tail to the right
mean(svars)    #compare with the population variance, which is 1732.7

