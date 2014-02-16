##Chapter Sampling Distributions
##Exercises
##R Scripts
##
##-----------------------------
#Exercise 13
## X1,X2,..X10 ~ N(20, 8^2), Y1, Y2,..Y15 ~ N(16,7^2)
## W = mean(X)+mean(Y)
 W <- numeric(1000)
 set.seed(0)
    for (i in 1:1000)
    {
         x <- rnorm(10, 20, 8)  #draw 10 from N(20, 8^2)
         y <- rnorm(15, 16, 7)  #draw 15 from N(16, 7^2)
        W[i] <- mean(x)+mean(y) #save sum of means
    }

    hist(W)
    dev.new()
    qqnorm(W)
    qqline(W)
    
    mean(W < 40)
    
#--------------------
#Exercise 16

X<-runif(1000,40, 60)
Y<-runif(1000,45, 80)

total <- X+Y

hist(total)
dev.new()
qqnorm(total)
qqline(total)

mean(total < 90)


#----------------
#27 Finite pop simulation
N <- 400 # population size
n <- 5 # sample size
finpop <- rexp(N, 1/10) # Create a finite pop. of size N=400 from
# Exp(1/10)
hist(finpop) # distribution of your finite pop.
mean(finpop) # mean (mu) of your pop.
sd(finpop) # stdev (sigma) of your pop.
sd(finpop)/sqrt(n) # theoretical standard error of sampling
# dist. of mean(x), with replacement
sd(finpop)/sqrt(n) * sqrt((N-n)/(N-1)) # without replacement

my.means <- numeric(1000)
for (i in 1:1000)
{
x <- sample(finpop, n) # Random sample of size n (w/o replacement)
my.means[i] <- mean(x) # Find mean of sample, store in my.means
}
hist(my.means)
dev.new() # new graphics device
qqnorm(my.means)
qqline(my.means)
mean(my.means)
sd(my.means) # estimated standard error of sampling
             # distribution

#----------------------------
#28
my.vars <- numeric(1000)
for (i in 1:1000)
{
x <- rnorm(20, 25, 7)
my.vars[i] <- var(x)
}
mean(my.vars)
var(my.vars)
hist(my.vars)
dev.new() # new graphics device
qqnorm(my.vars)
qqline(my.vars)
