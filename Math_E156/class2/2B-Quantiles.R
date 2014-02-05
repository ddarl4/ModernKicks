#Math E-156, Script 2A-Quantiles.R
#Covers section 2.4 of the textbook

#Start with the three-dice probability example
Dice <- read.csv("Dice3.csv")
total <- Dice$Red3+ Dice$Green3 + Dice$White3   #a random variable
CDF <- ecdf(total); mode(CDF)  #create the cumulative distribution function
plot(3:18, CDF(3:18), type = "b") #lines that join points are misleading

#Here is a better display of the cumulative distribution function
plot.ecdf(total)
deciles = seq(0.1, 0.9, by = 0.1); deciles
abline(h = deciles, col = "blue")
CDF(6)     #less than 10% chance of rolling 6 or less
quantile(total, 0.1)   #first possible value with more than 10% chance
q<-quantile(total, deciles); q
abline(v = q, col = "red")
#The quantile function is the inverse of the CDF,
#except that the CDF is not quite invertible

#For standard probability distributions, R has all the functions you need.
#Suppose that you toss a fair coin 18 times.
plot(0:18,dbinom(0:18, 18, 0.5), type = "h")   #probability density
plot(0:18,pbinom(0:18, 18, 0.5), pch = 3)      #cumulative distribution
abline(h = deciles, col = "blue")
qbin<-qbinom(deciles, 18, 0.5); qbin      #there are some duplicates
abline(v = qbin, col = "red")

#With a continuous probability distribution there is no invertibility problem.
#Use an exponential distribution with lambda = 1.
curve(dexp(x, 1), from = 0, to = 5)   #density
curve(pexp(x, 1), from = 0, to = 5)   #distribution
abline(h = deciles, col = "blue")
qexpon<-qexp(deciles, 1); qexpon
abline(v = qexpon, col = "red")
curve(dexp(x, 1), from = 0, to = 5)   #density
abline(v = qexpon, col = "red")       #equal area under graph in each strip

#We can also get quantiles for an experimentally determined distribution
NCB <- read.csv("NCBirths2004.csv")
weights <- NCB$Weight
par(mfrow = c(1, 2)) #2x1 layout
plot.ecdf(weights)
abline(h = deciles, col = "blue")
qweight<-quantile(weights,deciles); qweight
abline(v = qweight, col = "red")

#If we plot the quantile function for 200 values we get the same graph with axes reversed
p200 = seq(0.005, 0.995, by = 0.005)  #vector of 200 valuse
plot(p200,quantile(weights,p200))
abline(v = deciles, col = "blue")
abline(h = qweight, col = "red")
par(mfrow = c(1, 1)) #restore 1x1 layout

#Comparison with a normal distribution
hist(weights, breaks = "FD", freq = FALSE)
abline(v = qweight, col = "red")
#Overlay a normal distribution with the same mean and standard deviation
curve(dnorm(x, mean(weights), sd(weights)), col = "blue", add = TRUE)
qn <- qnorm(deciles, mean(weights), sd(weights)); qn
abline(v = qn, col = "green")
plot(qn, qweight)

#This useful type of plot has been automated, with lots more quantiles
qqnorm(weights)
qqline(weights)    #passes through 0.25 and 0.75 quantiles

#With skewed data a normal distribution is not a good approximation
#Replicate figure 2.10a from the text
FD <- read.csv("FlightDelays.csv"); head(FD)
UAdelays<-subset(FD, select = Delay, subset = (Carrier == "UA"), drop = TRUE) 
qqnorm(UAdelays); qqline(UAdelays)
AAdelays<-subset(FD, select = Delay, subset = (Carrier == "AA"), drop = TRUE)

#It is also possible to compare the quantiles of two vectors
qqplot(UAdelays, AAdelays)   #similar distribution for the two carriers
