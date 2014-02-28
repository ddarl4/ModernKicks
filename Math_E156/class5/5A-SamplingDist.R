#Math E-156 Script 5A-Sampling distributions.R
#Based on pages 77-80 of the textbook.

#Topic 1 - The sampling distribution for a Bernoulli distribution
#Toss a fair coin - Bernoulli distribution with p = 0.5
#50 experiments, each taking the sum of n samples.
y<-c(0,1) #a vector of outcomes, assumed equally likely
n <- 10    #the number of samples in an experiment


#In this case we can plot the exact sampling distribution (a probability distibution)
#We just make make a row for each possible sample of 10
x<-rowMeans(expand.grid(y,y,y,y,y,y,y,y,y,y))
barplot(table(x))

#The "standard error" is the standard deviation of the sampling distribution
SE <- sqrt(mean(x^2)-(mean(x)^2)); SE

#In this case it can be computed from the known variance of a Bernoulli distribution
#The variance of the mean of 10 undependent samples is the vaiance of one sample divided by 10
sqrt(0.5*(1-0.5)/10)

#Of course, we have re-created the binomial distribution
barplot(1024*dbinom(0:10,10,0.5))

#We can get an approximation to the sampling distribution by taking random samples
N <- 50; mymeans<-numeric(N)   #the number of experiments
for (i in 1:N){
  x <- rbinom(n,1,0.5)    #Bernoulli with p = 0.5
  mymeans[i]<-mean(x)
}
mymeans               #a vector of N sample means

#Here is a way to replicate Figure 4.1 on page 78.
stripchart(mymeans,method = "stack",  pch=19, offset = 1,ylab= "Count", ylim=c(0,20), axes = T)
axis(2) #add counts to the plot
#Here is an alternative that does not make the indivdual samples so clear
barplot(table(mymeans))

#Since the sum of binomials is a binomial, there is an easier way to simulate the sampling distribution
N <- 50
x <- rbinom(N,10,0.5)/n
#Display the result -- since we are sampling, it will be different
stripchart(x,method = "stack",  pch=19, offset = 1,ylab= "Count", ylim=c(0,20), axes = T)
axis(2) #add counts to the plot

#There is also a harder way that does not use the built-in binomial function
#It will work for any vector y
N <- 50; mymeans<-numeric(N)
for (i in 1:N){
  x <- sample(y,10,replace = TRUE)   #need "with replacement" to get lots of independent samples
  mymeans[i]<-mean(x)
}
mymeans
stripchart(mymeans,method = "stack",  pch=19, offset = 1,ylab= "Count", ylim=c(0,20), axes = T)
axis(2) #add counts to the plot

#Now all that we can get is an estimate of the standard error
SE.hat <- sqrt(mean(mymeans^2)-(mean(mymeans)^2)); SE.hat; SE

#If we do 5000 experiments instead of 50, we will get a better estimate
N <- 5000; mymeans<-numeric(N)
for (i in 1:N){
  x <- sample(y,10,replace = TRUE)   #need "with replacement" to get lots of independent samples
  mymeans[i]<-mean(x)
}
SE.hat <- sqrt(mean(mymeans^2)-(mean(mymeans)^2)); SE.hat; SE




#Topic 2 - sampling from an arbitrary small population
#Here is example 4.1 from the textbook
y<-c(3,4,6,6)     #a vector of 4 outcomes, all assumed equally likely
mean(y); popVar <-sqrt(mean((y-mean(y))^2)); popVar   #the population mean and variance

#Now take the mean of each possible sample of size 2
x<-rowMeans(expand.grid(y,y))
stripchart(x,method = "stack",  pch=19, offset = 1,ylab= "Count", ylim=c(0,20), axes = T)
axis(2) #add counts to the plot

#The "standard error" is the standard deviation of the sampling distribution
#It can be calculated from the standard deviation of the population y
SE <- sqrt(mean(x^2)-(mean(x)^2)); SE; popVar/sqrt(2)

#Do the same for samples of size 8
x<-rowMeans(expand.grid(y,y,y,y,y,y,y,y)); length(x)
barplot(table(x))

#The "standard error" is the standard deviation of the sampling distribution
SE <- sqrt(mean(x^2)-(mean(x)^2)); SE; popVar/sqrt(8)

#What we have been plotting is the exact sampling distribution (a probability concept)
#Alternatively, we could have taken 4^8 = 65536 samples from the original population
#The result will now be different every time!
N<-4^8; mymeans <- numeric(N); mymeans[1] = 3  #make sure table has the right length
for(i in 2:N) {
  mymeans[i] <-mean(sample(y,8, replace = TRUE))
}
barplot(table(mymeans))

#With this many experiments we get a good estimate of the standard error
#Note - the standard error is large because we are working with small samples of 8.
#Our estimate of the standard error is goodbecuse we did 65536 experiments
SE.hat <- sqrt(mean(mymeans^2)-(mean(mymeans)^2)); SE.hat; SE

#A clever way to combine the two tables in order to compare them
#This only works if the tables have exactly the same length
rbind(table(mymeans),table(x))
barplot(rbind(table(mymeans),table(x)), beside = T, col = c("red","blue"))
#The true and simulated sampling distributions look much the same



