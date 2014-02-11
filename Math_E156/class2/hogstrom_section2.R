# 1. The lower right-hand plot of figure 2.15 appears to be the density function 
# for a gamma distribution with shape = 2, rate = 0.5. The density function in 
# R is dgamma(x, 2, 0.5). Plot this function to confirm the identification, 
# then see if you get the same skewness and kurtosis that are shown on 
# the plot. Why would you expect positive values for both these quantiities?
curve(dgamma(x, 2, 0.5),from = 0, to = 14)

integ<-integrate(function(x) x*dgamma(x, 2, 0.5), -Inf, Inf); integ
mu<-integ$value; mu     #this is just a number
MC2<-integrate(function(x) (x-mu)^2*dgamma(x, 2, 0.5), -Inf, Inf); MC2
sigma<- sqrt(MC2$value); sigma
MC3<-integrate(function(x) (x-mu)^3*dgamma(x, 2, 0.5), -Inf, Inf); MC3 #no skewness
MC4<-integrate(function(x) (x-mu)^4*dgamma(x, 2, 0.5), -Inf, Inf); MC4
skewness <- MC3$value/sigma^3; skewness
kurtosis <- MC4$value/sigma^4-3; kurtosis #zero for a normal dstribution

# 2. For the same gamma distribution, find the 0.2 and 0.8 quantiles. Illustrate your answers by drawing appropriate horizontal or vertical lines on graphs of dgamma(x, 2, 0.5), pgamma(x, 2, 0.5),andrgamma(x, 2, 0.5),putting all three plots into a 2x2 grid by the technique at the end of the plot tutorial.

# 3. Problem 5 on page 31 of the textbook (General Social Survey Case Study).
# General social survey case Study
cs <- read.csv('GSS2002.csv')

table(cs$DeathPenalty)
summary(cs$DeathPenalty)


