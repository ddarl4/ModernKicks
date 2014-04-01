#Chapter 7 Classical Inference: Confidence Intervals

#-----------------------------------------------------
#Section 7.1.1 CI for normal with known sigma

#set.seed(1)
counter <- 0                 # set counter to 0
plot(x = c(22, 28), y = c(1, 100), type = "n", 
    xlab = "", ylab = "")
abline(v = 25, col="red")               # vertical line at mu
for (i in 1:1000) 
 {
  x <- rnorm(30, 25, 4)           # draw a random sample of size 30
  L <- mean(x) - 1.96*4/sqrt(30)  # lower limit
  U <- mean(x) + 1.96*4/sqrt(30)  # upper limit
  if (L < 25 && 25 < U)           # check to see if 25 is in interval
     counter <- counter + 1       # increase counter by 1
  if (i <= 100)                   #plot first 100 intervals 
      segments(L, i, U, i) 
 }
 
 abline(v = 25, col = "red")     #vertical line at mu
 
 counter/1000    # proportion of times interval contains mu.

#---------------------------------------------------------------
# Section 7.1.2
# Simulate distribution of t statistic
N <- 10^4
w <- numeric(N)
n <- 15  #sample size
for (i in 1:N) 
 {
  x <- rnorm(n, 25, 7)  #draw a size 15 sample from N(25, 7^2)
  xbar <- mean(x)
  s <- sd(x)
  w[i] <- (xbar-25) / (s/sqrt(n))
 }

 hist(w)
 dev.new()
 qqnorm(w, pch = ".")
 abline(0, 1, col = 2) # y = x line

#pch = "."  is point character. This option says to use . for the points.

#----------------------------------------------------------
# Example 7.7 Simulation 95% confidence interval from
# skewed gamma distribution
# set.seed(0)

tooLow <- 0           #set counter to 0
tooHigh <- 0          #sest counter to 0
n <- 20               # sample size
N <- 10^5
for (i in 1:N) 
 {
  x <- rgamma(n, shape=5, rate=2)
  xbar <- mean(x)
  s <- sd(x)
  lower <- xbar - abs(qt(.025, n-1))*s/sqrt(n)
  upper <- xbar + abs(qt(.025, n-1))*s/sqrt(n)
  if (upper < 5/2) tooLow <- tooLow + 1
  if (lower > 5/2) tooHigh <- tooHigh + 1
 }
tooLow/N
tooHigh/N



#----------------------------------------
# Example 7.21 One sample bootstrap t confidence interval

Bangladesh <- read.csv("Bangladesh.csv")
Arsenic <- subset(Bangladesh, select = Arsenic, drop = T)

xbar <- mean(Arsenic)
N <- 10^4
n <- length(Arsenic)
Tstar <- numeric(N)
#set.seed(100)
for (i in 1:N)
 {
   x <-sample(Arsenic, size = n, replace = T)
   Tstar[i] <- (mean(x)-xbar)/(sd(x)/sqrt(n))
 }

quantile(Tstar, c(0.025, 0.975))

hist(Tstar, xlab = "T*", main = "Bootstrap distribution of T*")

dev.new()
qqnorm(Tstar)
qqline(Tstar)

#-------------------------------------------------------
# Exampe 7.22 Verizon
# 2-Sample bootstrap t confidence interval

# Verizon <- read.csv("Verizon.csv")
Time.ILEC <- subset(Verizon, select=Time, Group == "ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time, Group == "CLEC", drop=T)

thetahat <- mean(Time.ILEC)-mean(Time.CLEC)
nx <- length(Time.ILEC)  #nx=1664
ny <- length(Time.CLEC)  #ny=23
SE <- sqrt(var(Time.ILEC)/nx + var(Time.CLEC)/ny)

N <- 10000
Tstar <- numeric(N)
set.seed(0)
for(i in 1:N)
 {
  bootx <- sample(Time.ILEC, nx, replace=TRUE)
  booty <- sample(Time.CLEC, ny, replace=TRUE)
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat) /
               sqrt(var(bootx)/nx + var(booty)/ny)
 }
 
thetahat - quantile(Tstar, c(.975, .025)) * SE

t.test(Time.ILEC, Time.CLEC)$conf

#----------------------------------------------------------------

#Exercise 18
%%Simulation to compare pooled/unpooled t-confidence intervals

pooled.count <- 0
unpooled.count <- 0

m <- 80
n <- 80

B <- 10000
for (i in 1:B)
{
    x <- rnorm(m, 8,10)
    y <- rnorm(n, 3, 15)
    
    CI.pooled <- t.test(x,y,var.equal=T)$conf
    CI.unpooled <- t.test(x,y)$conf
    
    if (CI.pooled[1] < 5 & 5 < CI.pooled[2])
    pooled.count <- pooled.count + 1  

   if (CI.unpooled[1] < 5 & 5 < CI.unpooled[2])
    unpooled.count <- unpooled.count + 1
}

pooled.count/B

unpooled.count/B
