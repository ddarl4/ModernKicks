# Larson Hogstrom - HW2
# MATH_E156 - 2/5/2014

#### Part 1 ###
states <- read.csv('States03.csv')

# make a scatter plot of percentage of high school graduates against teacher 
# pay, with a solid red vertical line at mean teacher pay and 
# a dashed blue horizontal line at mean percentage of high school graduates.
plot(states$TeachersPay,states$HSGrad, ylab="HSGrad", xlab="TeachersPay")
abline(v = mean(states$TeachersPay), lty= 1, col = "red")  #red vertical line at mean teacher pay
abline(mean(states$HSGrad),0, lty=2, col="blue") # a dashed blue horizontal line at mean percentage of high school graduates.
title("US - States Data from 2003")

### Part 2 ###

#import Black Spruce
spruce <- read.csv('Spruce.csv')
# a) compute the numeric summaries for the height change Ht.Change of the seedlings
summary(spruce$Ht.change)
# b) make histogram and normal quantile plot for the height changes
# of the seedlings. Is the  distribution appprox. normal?
par(mfrow = c(2, 1))   #2x2 layout
hist(spruce$Ht.change)
qqnorm(spruce$Ht.change)
qqline(spruce$Ht.change)
par(mfrow = c(1, 1))
print('this distribution appears approximately normal')
# c) make boxplot - change in diameter (Di.change) groupbed by plot fertilize status
boxplot(Di.change ~ Fertilizer, data=spruce, ylab="diameter", xlab="Fertilizer")
# d) use tapply to find the numeric summaries of the diameter changes for 
# the two levels of fertilization
FertSumm <- tapply(spruce$Di.change, spruce$Fertilizer, summary); FertSumm
# e) scatter plot of height changes by diameter changes 
plot(spruce$Ht.change, spruce$Di.change)
print('seedling hight and diameter are positivly correlated')

### Part 3 ###

# (a) Plot a graph of the Student’s t density function 
# for six degrees of freedom dt(x,6) 
# on the interval [-3,3]. This is a classic example of a distribution 
# with “fat tails.” On the same plot overlay, in green, a graph of 
# the density function for the standard normal distribution dnorm(x)
curve(dt(x,6), from = -3, to = 3)
curve(dnorm(x), from = -3, to = 3, col="green", add = TRUE)

# (b) Determine the 0.1 and 0.9 quantiles for each of these distributions and 
# add vertical lines (in black and green respectively) that mark off the 
# interval on which 80% of the area under the graph lies for each distribution.
curve(dt(x,6), from = -3, to = 3)
curve(dnorm(x), from = -3, to = 3, col="green", add = TRUE)
abline(v = qnorm(.9), lty= 1, col = "black", add = TRUE)  
abline(v = qnorm(.1), lty= 1, col = "black", add = TRUE)  
# add lines for student-t quantiles
abline(v = qt(.9,6), lty= 1, col = "green", add = TRUE)
abline(v = qt(.1,6), lty= 1, col = "green", add = TRUE)

# (c) Plot a graph of the Student’s t distribution function for six degrees of freedom 
# pt(x,6) on the interval [-3,3]. On the same plot overlay, in green, a 
# graph of the quantile function for the standard normal distri- bution 
# dnorm(x) Add horizontal lines (in black and green respectively) 
# at 0.1 and 0.9 and use them to estimate the 0.1 and 0.9 quantiles.
curve(pt(x,6), from = -3, to = 3, col="black")
curve(pnorm(x), from = -3, to = 3, col="green", add = TRUE)
# curve(qnorm(x), from = -3, to = 3, col="green")
abline(h = .9, lty= 1, col = "black")  
abline(h = .1, lty= 1, col = "black") 
print('.1 quantile is about -1.4 and -1.3 for the Students T and normal distribution respectively')
print('.9 quantile is about 1.4 and 1.3 for the Students T and normal distribution respectively')

# (d) Plot a graph of the Student’s t quantile function for 
# six degrees of free- dom qt(x,6) on the interval [-3,3]. 
# On the same plot overlay, in green, a graph of the distribution 
# function for the standard normal distribu- tion qnorm(x) Add vertical 
# lines (in black and green respectively) at 0.1 and 0.9 and use them to 
# estimate the 0.1 and 0.9 quantiles. (Since dnorm() and qnorm() are inverse 
# functions, this is the same plot as in part (c), with the axes reversed!)
curve(qt(x,6), from = 0, to = 1, col="black")
curve(qnorm(x), from = 0, to = 1, col="green", add = TRUE)
abline(v = .9, lty= 1, col = "black")  
abline(v = .1, lty= 1, col = "black") 
print('.1 quantile is about -1.4 and -1.3 for the Students T and normal distribution respectively')
print('.9 quantile is about 1.4 and 1.3 for the Students T and normal distribution respectively')

# (e) Compare the Student’s t distribution for six degrees of freedom 
# with the normal distribution by using qqnorm() and qqline().
# plot with random values
x <- rt(5000,6)
qqnorm(x)
qqline(x)
# plot with equally spaced values
x <- seq(-5, 5, by = 0.1)
 qqnorm(x)
 qqline(x)

### Part 4 ### 

# (a) Because a histogram of flight delays has a long tail to the right, 
# we would expect a positive skewness. Load FlightDelays.csv, and 
# by using the same approach that was used for the Poisson 
# distribution in script 2C, determine the skewness of the delays. You will 
# need to start by calculating the mean and variance. (One can argue about 
# whether the data are a population or a sample from a larger population, 
# but there are so many data points that it really doesn’t matter!)
fd <- read.csv("FlightDelays.csv")
# colnames(fd) # view column names
mu = mean(fd$Delay); mu
# MC2 = var(fd$Delay) # MC2 = Var
MC2 <- mean((fd$Delay-mu)^2); MC2  #the variance
MC3 <- mean((fd$Delay-mu)^3); MC3 
MC4 <- mean((fd$Delay-mu)^4); MC4
# MC2<-sum((0:30-mu)^2*dpois(0:30,5));MC2   #easily proved in general
# MC3<-sum((0:30-mu)^3*dpois(0:30,5));MC3
# MC4<-sum((0:30-mu)^4*dpois(0:30,5));MC4
sigma<- sqrt(MC2); sigma  #the standard deviation
skewness <- MC3/sigma^3; skewness  #zero in this case
kurtosis <- MC4/sigma^4-3; kurtosis 

# (b) Since the Student t distribution has fatter tails than the normal 
# dis- tribution, we would expect a positive kurtosis. The integrals are 
# a pain to evaluate by using calculus, even though μ = 0. By having R 
# evaluate the second and fourth moments by numerical integration (consult 
# script 2C for the tricky syntax of integrate), confirm that the kurtosis 
# of the Student’s t distribution for six degrees of freedom is 3.
curve(dt(x,6), from = -3, to = 3) 
integ<-integrate(function(x) x*dt(x,6), -Inf, Inf); integ
mu<-integ$value; mu     #this is just a number
MC2<-integrate(function(x) (x-mu)^2*dt(x,6), -Inf, Inf); MC2
sigma<- sqrt(MC2$value); sigma
MC3<-integrate(function(x) (x-mu)^3*dt(x,6), -Inf, Inf); MC3 #no skewness
MC4<-integrate(function(x) (x-mu)^4*dt(x,6), -Inf, Inf); MC4
skewness <- MC3$value/sigma^3; skewness
kurtosis <- MC4$value/sigma^4-3; kurtosis #zero for a normal dstribution

### Part 5 ### 

# 17) Compare the ecdf's for UA and AA lengths of flight delays
# (a) Problem 17 on page 33. By using the technique from 
# the last section of the plot tutorial, 
# you can place the graphs side by side in the top row of a 2x2 grid.
aaFd <- fd[fd$Carrier == "AA",]
uaFd <- fd[fd$Carrier == "UA",]
par(mfrow = c(2, 2)) #2x2 layout
plot.ecdf(aaFd$Delay, col = 'blue')
plot.ecdf(uaFd$Delay)

# (b) In the bottom row, plot the quantiles for 100 values ranging from 0.01 to 
# 0.99. You will get essentially the same graphs, but with the axes interchanged.
rng <- 1:99/100
qAA <- quantile(aaFd$Delay,probs=rng)
plot(rng,qAA, col= 'blue')
qUA = quantile(uaFd$Delay,probs=rng)
plot(rng,qUA)
par(mfrow = c(1, 1)) #reset to default layout