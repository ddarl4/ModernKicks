#Here is a vector whose mean is clearly 5
v<-3:7
v
mean(v)
#The vectorized operations of R let us square it
w<-v*v
w
#Now we can calculate the variance
mean(w)-mean(v)^2
#Or we can do it straight from the definition of variance
mean((v-mean(v))^2)
#Try using the built-in function in R
var(v) 
#As noted on page 4 of the R tutorial, we need to multiply by 5/4 to get the population variance.
#The function var() computes the "sample variance"
#Make a vector of 5 samples from this population.
sample5<-sample(v,5,replace=TRUE)
sample5
var(sample5)
var(sample(v,5,replace=TRUE))
#We get a different answer every time: the "sample variance" is a random variable.
#Make a vector of sample variances. Here is the wrong way
svars<-rep(var(sample(v,5,replace=TRUE)),100)
svars
#We need to do a little programming
svars<-rep(0,100) #initialize
for (i in 1:100) {svars[i]=var(sample(v,5,replace=TRUE))}
svars
hist(svars)
mean(svars) #should be close to 2, not to 2.5
#With lots of repetitions we get close to the true mean of this random variable
svars<-rep(0,10000) #initialize
for (i in 1:10000) {svars[i]=var(sample(v,5,replace=TRUE))}
head(svars)
hist(svars)
mean(svars) 
#with Flight Delays we have the entire population
FDelay<-read.csv("FlightDelays.csv")
delays<-FDelay$Delay
hist(delays)
mean(delays)
length(delays)  #so large that n vs. n-1 makes no difference
var(delays) #not quite right
mean((delays-mean(delays))^2) #the correct population variance
#Again we can look at samples of 5 
sample5<-sample(delays,5,replace=TRUE)
sample5
var(sample5)
var(sample(delays,5,replace=TRUE))
#We get a different answer every time: the "sample variance" is a random variable.
#With lots of repetitions we get close to the true mean of this random variable
svars<-rep(0,100000) #initialize
for (i in 1:100000) {svars[i]=var(sample(delays,5,replace=TRUE))}
head(svars)
hist(svars)
mean(svars) #compare with the population variance, which is 1732.668
#Suppose we look at the means of our 5-fight samples
smeans<-rep(0,100000) #initialize
for (i in 1:100000) {smeans[i]=mean(sample(delays,5,replace=TRUE))}
head(smeans)
hist(smeans)
mean(smeans) #compare with the population mean
mean(delays) #should be very close
#The central limit theorem says that if we use larger samples we should see a normal distribution
smeans<-rep(0,100000) #initialize
for (i in 1:100000) {smeans[i]=mean(sample(delays,400,replace=TRUE))}
head(smeans)
hist(smeans,50)
#If we subtract the mean and rescale appropriately, things look really nice
smeans = 20*(smeans-mean(delays))/sd(delays)
hist(smeans,50)



