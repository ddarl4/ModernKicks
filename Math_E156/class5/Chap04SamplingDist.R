###Chapter 4: Sampling Distributions

#---------------------------------------------
#Example 4.2: Sampling distribution from Exp(1/15)
my.means<-numeric(1000)
#set.seed(300)
for (i in 1:1000)
{
  x<-rexp(100, 1/15)
  my.means[i]<- mean(x)
}

hist(my.means, main="Simulated sampling distribution", xlab="means")

dev.new()
qqnorm(my.means)
qqline(my.means)

mean(my.means)
sd(my.means)

#----------------------------------------------------
##Example 4.3: Samp Dist from Unif[0,1]

my.max<-numeric(1000)
#set.seed(100)
for (i in 1:1000)
 {
   y <- runif(12)        #draw random sample of size 12
   my.max[i] <- max(y)   #find max, save in i_th position of my.max
 }
 
hist(my.max, main="", xlab="maximums")

#To create a histogram with a density curve imposed
#scale bars to have area one with prob=TRUE option
hist(my.max, main="", xlab = "maximums", prob=TRUE)

#add pdf to histogram
curve(12*x^{11}, col="blue", add=TRUE)

#------------------------------------------------
#Example 4.6
#Sampling distribution simulation
#Sample of size 30 from gamma r=5, lambda=2

#set.seed(10)
my.means<-numeric(1000)
for (i in 1:1000)
  {
    x <- rgamma(30, shape=5, rate=2)
    my.means[i] <- mean(x)
  }

hist(my.means, main="Distribution of means")

dev.new()
qqnorm(my.means)
qqline(my.means)

mean(my.means)
sd(my.means)
sum(my.means > 3)/1000
#alternatively
mean(my.means > 3)

#----------------------------------------------
