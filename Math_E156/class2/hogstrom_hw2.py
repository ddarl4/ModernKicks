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
tapply(spruce$Di.change, spruce$Fertilizer, summary)
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
par(new=T)
# curve(dnorm(x), from = -3, to = 3, color="green")
curve(dnorm(x), from = -3, to = 3, col="green")
legend("topright", legend = c("student t","normal"), col = c("black","green"))
par(new=F)

# (b) Determine the 0.1 and 0.9 quantiles for each of these distributions and 
# add vertical lines (in black and green respectively) that mark off the 
# interval on which 80% of the area under the graph lies for each distribution.
curve(dnorm(x), from = -3, to = 3)
abline(v = qnorm(.9), lty= 1, col = "black")  
abline(v = qnorm(.1), lty= 1, col = "black")  

curve(dt(x,6), from = -3, to = 3, col = "green")
abline(v = qt(.9,6), lty= 1, col = "green")  
abline(v = qt(.1,6), lty= 1, col = "green")  


# (c) Plot a graph of the Student’s t distribution function for six degrees of freedom 
# pt(x,6) on the interval [-3,3]. On the same plot overlay, in green, a 
# graph of the quantile function for the standard normal distri- bution 
# dnorm(x) Add horizontal lines (in black and green respectively) 
# at 0.1 and 0.9 and use them to estimate the 0.1 and 0.9 quantiles.
curve(pt(x,6), from = -3, to = 3, col="black")
curve(pnorm(x), from = -3, to = 3, col="green")
# curve(qnorm(x), from = -3, to = 3, col="green")
abline(h = .9, lty= 1, col = "black")  
abline(h = .1, lty= 1, col = "black") 


# (d) Plot a graph of the Student’s t quantile function for 
# six degrees of free- dom qt(x,6) on the interval [-3,3]. 
# On the same plot overlay, in green, a graph of the distribution 
# function for the standard normal distribu- tion qnorm(x) Add vertical 
# lines (in black and green respectively) at 0.1 and 0.9 and use them to 
# estimate the 0.1 and 0.9 quantiles. (Since dnorm() and qnorm() are inverse 
# functions, this is the same plot as in part (c), with the axes reversed!)
curve(qt(x,6), from = -3, to = 3, col="black")
curve(qnorm(x), from = -3, to = 3, col="green")


# (e) Compare the Student’s t distribution for six degrees of freedom 
# with the normal distribution by using qqnorm() and qqline().





