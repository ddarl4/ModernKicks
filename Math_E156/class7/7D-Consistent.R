#Math E-156 Script 7D-Consistent.R

#Based on section 6.3.4, pages 157-159 of the textbook

#Topic 1 - the sample mean is a consistent estimator of the expectation.
#If the MSE of an estimator approaches zero for large n, it is consistent.
#Special case of Example 6.17 - sample from the exponential distribution with lambda = 1
#Samples have size 250, and we repeat the experiment 1000 times
#We will be pleased if our estimate of lambda (the sample mean) is within epsilon of lambda
n <- 250; N = 1000; means = numeric(N); epsilon = 0.05
for (i in 1:N){
  x <- rexp(n, 1)      #generate a sample
  means[i] = mean(x)   #save the sample mean
}
hist(means)            #with sample size of 250, the CLT is pretty good
mean(abs(means -1) > epsilon)     #but we miss our ambitious target most of the time

#Here is another way to display the variability of the estimator
plot( rep(250, N), means)

#Make a plot like Figure 6.7
#Repeat with still larger values of the sample size n
plot(1, xlim= c(200, 20000), ylim = c(0.75, 1.25), log = "x", type = "n") #use log scale on the x axis
abline(h = c(1 + epsilon, 1- epsilon), col = "red") #we want to get inside this band
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  means = numeric(N);
  for (i in 1:N){
    x <- rexp(n, 1)
    means[i] = mean(x)
  }
  points( rep(n, N), means)
}

#Topic 2 - an example of an estimator that is not consistent
#Consistent means that for large n all the points get between the red lines
n <- 250; N = 1000; means = numeric(N); epsilon = .5  #much larger epsilon
#Repeat our procedure with the Cauchy distribution, whose expectation is undefined
curve(dcauchy(x,5), from = 0, to = 10)
for (i in 1:N){
  x <- rcauchy(n, 5)   #symmetric around theta = 5
  means[i] = mean(x)
}
hist(means, breaks = 100, xlim = c(-50, 50))  #lots of outliers are not shown
mean(abs(means -5) > epsilon) #miss the target most of the time

#Now do this for several values of n
plot(1, xlim= c(200, 20000), ylim = c(0, 10), log = "x", type = "n")
abline(h = c(5 + epsilon, 5- epsilon), col = "red")
N = 1000;  epsilon = 0.05
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  means = numeric(N);
  for (i in 1:N){
    x <- rcauchy(n,5, 1)
    means[i] = mean(x)
  }
  points( rep(n, N), means)
}
#Increasing n accomplishes nothing -- the mean is not a consistent estimator

#Example 6.18: the median of a Cauchy distribution is a consistent estimator
plot(1, xlim= c(200, 20000), ylim = c(4.6, 5.4), log = "x", type = "n")
abline(h = c(5 + epsilon, 5- epsilon), col = "red")
N = 1000;  epsilon = 0.5
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  medians = numeric(N);
  for (i in 1:N){
    x <- rcauchy(n,5, 1)
    medians[i] = median(x)
  }
  points( rep(n, N), medians)
}

#Topic 3 - the sample mean can be a consistent estimator even when the variance is infinite
#Use a Student's t distribution with 2 degrees of freedom (one degree of freedom would be Cauchy)
#This distribution has an expectation of 0 but its variance is infinite!
curve(dt(x, 1),from = -5 , to = 5)   #Cauchy
curve(dt(x, 2),from = -5, to = 5)
n <- 250; N = 1000; means = numeric(N); vars = numeric(N); epsilon = .05
mean(rt(n,2)); var(rt(n,2)) #the sample variance has to be finite!
#Try again with larger samples
n <- 1000; mean(rt(n,2)); var(rt(n,2)) #the sample variance has to be finite but does not get smaller!
n<- 5000; mean(rt(n,2)); var(rt(n,2)) #the sample variance must be finite but does not get smaller!
#Look at the sampling distribution 
for (i in 1:N){
  x <- rt(n, 2)   #rt(n, 1) would give Cauchy with theta = 0
  means[i] = mean(x)
  vars[i] = var(x)
}
hist(means, breaks = 100, xlim = c(-1, 1))  #long tails
hist(vars, breaks = 1000, xlim = c(7, 30))  #long tails
mean(abs(means) > epsilon)   #miss the target about 1/3 of the time
#Again do a plot for several values of the sample size n
N = 1000;  epsilon = 0.25
plot(1, xlim= c(200, 20000), ylim = c(-1, 1), log = "x", type = "n")
abline(h = c(epsilon, -epsilon), col = "red")
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  means = numeric(N);
  for (i in 1:N){
    x <- rt(n,2)
    means[i] = mean(x)
  }
  points( rep(n, N), means)
}
#So the sample mean is a consistent estimator
#Do the same sort of plot for the sample variance
N = 1000;  epsilon = 5 #not a very ambitious target
plot(1, xlim= c(200, 20000), ylim = c(4, 30), log = "x", type = "n")
abline(h = c(10+ epsilon, 10 -epsilon), col = "red")  #10 is arbitrary - no limit exists
for (n in c(250, 500, 1000, 2000, 5000, 10000, 20000)){
  vars = numeric(N);
  for (i in 1:N){
    x <- rt(n,2)
    vars[i] = var(x)
  }
  points( rep(n, N), vars)
}
#The sample variance is finite, but it does not converge to anything

