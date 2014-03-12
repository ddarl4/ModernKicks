# Larson Hogstrom - HW6
# MATH_E156 - 3/10/2014

### Part 1 
# 1. “Mortal cyberpets,” whose lifetime is specified by an exponential distri- 
# bution, have become big business, and you have been hired by Consumer Reports 
# to write a feature article on them. It turns out that many young- sters have 
# been buying 20 of these cyberpets, keeping vital statistics on them, and then 
# complaining when the mean lifetime for their sample is less than the expected 
# lifetime advertised by the pets’ creator.

# So you plan to publish a formula for a confidence interval that has a 95% 
# probability of including the true expectation of the lifetime, along 
# with a warning that there is stiil a 2.5% chance that the true expected 
# lifetime will lie outside the interval on either side. Since the central 
# limit theorem does not give a good approximation to the mean of a sample 
# of just 20 exponential random variables, you plan to develop and test a 
# formula for the endpoints of the confidence interval by using the exact 
# sampling distribution (a gamma distribution with shape = 20), then see if 
# you can replicate your results by using a bootstrap percentile confidence 
# interval from a single sample.

# For simplicity, measure time in lunar months (1 month= 28 days) and 
# take lambda = 1. Replicate the approach of script 6A, with the bootstrap 
# then done as in script 6B. It is OK to paste and edit code from these 
# scripts. Include a graphical display of 100 confidence intervals.
# My suspicion is that the bootstrap percentile confidence interval will 
# pass the 95% test reasonably well but that when the confidence interval 
# fails to include 1 month it is far more likely to miss on one side than 
# on the other. We will try after spring break to deal with this problem!

# exact sampling distribution (a gamma distribution with shape = 20)
# 20 exponential random variables
curve(dgamma(x,shape = 20,rate =20), xlim = c(0,3))
#Plot lines that exclude the extreme 2.5% on either side
ciLow <- qgamma(0.025,shape = 20,rate = 20); ciUp
ciUp <- qgamma(0.975,shape = 20,rate = 20); ciLow
abline(v = ciUp, col = "red")
abline(v = ciLow, col = "red")
# measure time in lunar months (1 month= 28 days) and take lambda = 1
ciUp.days <- ciUp *28; ciUp.days
ciLow.days <- ciLow *28; ciLow.days

# replicate your results by using a bootstrap percentile confidence 
# interval from a single sample.
curve(dgamma(x, 1, 1), from = 0, to = 10) 
my.sample <- rgamma(20, 1, 1)    #draw 20 samples from this population
mean(my.sample) 
hist(my.sample,  breaks= "FD", probability = TRUE)
abline(v = mean(my.sample), col = "blue", lty = 2)
curve(dgamma(x, 1, 1), col = "red", add = T)
abline(v = 1, col = "red", lty = 2)  

#Now compare the bootstrap and sampling distributions
N=10^5; my.boot<-numeric(N)
for (i in 1:N) {
  my.boot[i] = mean(sample(my.sample, 20, replace = TRUE))
}
hist(my.boot,  breaks= "FD", probability = TRUE)
abline(v = mean(my.boot), col = "blue", lty = 2)
#Overlay the theoretical sampling distribution
curve(dgamma(x,shape = 20,rate =20), col = "red", add = TRUE)
abline(v = 1, col = "red", lty = 2)
CI <- quantile(my.boot, c(0.025, 0.975)); CI
abline(v = CI[1], col = "blue")
abline(v = CI[2], col = "blue")

print(paste("Pet Warning: There is a 95% percent chance that the mean age
    of 20 “Mortal cyberpets” lies between ",round(ciLow.days,3), " and ",
    round(ciUp.days,3), "days. There is stiil a 5% chance that the true
    expected lifetime will lie outside this interval."))

### Part 2 
# p.132 - #12
# FishMercury --> mercury ppm for 30 fish cought in Minesota
# a) Historgram and boxplot of data - what do you observe?
Mercury <- read.csv('FishMercury.csv'); head(fm)
fm <- Mercury$Mercury
hist(fm,50)
boxplot(fm)
print('there is a single outlier sample with high mercury level')

# b) Bootstrap the mean and record the bootstrap standard error and the 
# 95% confidence interval

n<-length(fm);n
N<-10^4; m.mean <- numeric(N)
for (i in 1:N) {
  m.mean[i]<-mean(sample(fm, n, replace = TRUE))
}
hist(m.mean, breaks= "FD",main = ("Bootstrap distribution of means"), probability = TRUE)
print('there is a multiple peaks in the distribution of bootstraped means - the different
    peaks represent how many times the outlier was included in the sample')
#bootstrap standard error
bootSE <- sqrt(var(m.mean)); bootSE
#We can use the bootstrap distribution to get a 95% "bootstrap percentile confidence interval" for the true mean
hist(m.mean, breaks= "FD",main = ("Bootstrap distribution of means"), probability = TRUE)
quantile(m.mean, c(.025, .975)) #not symmetric about the mean
bootlow <- quantile(m.mean, .025); boothigh <- quantile(m.mean, .975);
abline(v = bootlow, col = "blue", lty = 2); abline(v = boothigh, col = "blue", lty = 2)

# c) Remove the ouutlier and bootstrap the mean of the remaining data. record
# the bootstrap standard error and the 95% bootstrap interval.
imax <- which.max(fm)
fmDrop <- fm[-imax] # remove max
n<-length(fmDrop);n
N<-10^4; drop.mean <- numeric(N)
for (i in 1:N) {
  drop.mean[i]<-mean(sample(fmDrop, n, replace = TRUE))
}
#bootstrap standard error
bootSE.drop <- sqrt(var(drop.mean)); bootSE.drop
#We can use the bootstrap distribution to get a 95% "bootstrap percentile confidence interval" for the true mean
hist(drop.mean, breaks= "FD",main = ("Bootstrap distribution of means"), probability = TRUE)
quantile(drop.mean, c(.025, .975)) #not symmetric about the mean
bootlow <- quantile(drop.mean, .025); boothigh <- quantile(drop.mean, .975);
abline(v = bootlow, col = "red", lty = 2); abline(v = boothigh, col = "blue", lty = 2)
bootSE # origonal SE
bootSE.drop # outlier removed

# d) what effect did removing the outlier have on the bootstrap distribution,
# in particular, the standard error?
print('removing the outlier reduced the variance of the bootstraped distribution')

### Part 3
# Bookprices --> hardcover textbook prices
bp <- read.csv('Bookprices.csv'); head(bp)
# a) Perform some exploratory data analysis on book prices for each 
# of the thwo disciplinary areas
boxplot(bp$Price ~ bp$Area)
#permuted difference in mean book price
nBooks <- length(bp$Price)
iMS <- which(bp$Area =='Math & Science')
iSS <- which(bp$Area =='Social Sciences')
nMS <- length(iMS)
nSS <- length(iSS)
ObsDiff <- mean(bp$Price[iMS]) - mean(bp$Price[iSS])
N<-10^4; perm.mean.diff <- numeric(N)
for (i in 1:N) {
  iMS.rand <- sample(1:nBooks,nMS,replace=FALSE)
  iSS.rand <- sample(1:nBooks,nSS,replace=FALSE)
  perm.mean.diff[i] <- mean(bp$Price[iMS.rand]) - mean(bp$Price[iSS.rand])
}
hist(perm.mean.diff, xlim=c(-70,70))
abline(v = ObsDiff, col = "red", lty = 2);
pValue = (sum (perm.mean.diff >= ObsDiff) + 1)/(N+1); 2*pValue
print('the observed difference in mean book price is much higher than
    the expected difference from permutation')

# b) Bootstrap the mean of book price for each area separetly and 
# describe the distributions
N<-10^4; 
MS.mean <- numeric(N)
SS.mean <- numeric(N)
for (i in 1:N) {
  MS.mean[i]<-mean(sample(bp$Price[iMS], nMS, replace = TRUE))
  SS.mean[i]<-mean(sample(bp$Price[iSS], nSS, replace = TRUE))
}
hist(MS.mean,col=rgb(0,0,1,1/4), xlim=c(25,200))
hist(SS.mean,col=rgb(1,0,0,1/4), add=T)
print('both bootstraped distributions are roughly normal
    the Math&Science distribution is centered on higher prices and shows 
    less variance')

# c) Bootstrap the ratio of means. Provide plots of the bootstrap distributionand 
# describe the distribution.
N<-10^4; 
MS.mean <- numeric(N)
SS.mean <- numeric(N)
ratio <- numeric(N)
for (i in 1:N) {
  MS.mean[i]<-mean(sample(bp$Price[iMS], nMS, replace = TRUE))
  SS.mean[i]<-mean(sample(bp$Price[iSS], nSS, replace = TRUE))
  ratio[i] <-MS.mean[i]/SS.mean[i]
}
hist(ratio)
obsRatio <- mean(bp$Price[iMS])/mean(bp$Price[iSS]); obsRatio

# d) Find the 95% bootstrap percentile interval for the ratio of means. Interpret
# this interval
CI = quantile(ratio, c(0.025, 0.975)); CI
abline(v = CI[1], col = "blue")
abline(v = CI[2], col = "blue")

# e) What is the bootstarp estimate of the bias? What fraction of the 
# bootstrapstandard error does it represent?
bias <- mean(ratio) - obsRatio; bias
stderr <- sd(ratio); stderr
biasFrac <- bias/stderr; biasFrac

### Part 4

# Of the students who entered Harvard as freshmen in Fall 1997, 41 of 819 men 
# and 23 of 756 women failed to graduate within 5 years. Conduct a bootstrap 
# analysis, in the manner of Example 5.7, of the relative risk of failing to 
# graduate within five years based on gender. Include a plot like figure 5.17 
# that illustrates the bootstrap percentile confidence interval for the 
# relative risk.
gender <- c(rep("Men", 819),rep("Women",756))
Outcome<- c(rep("grad",778),rep("not_grad",41),rep("grad",733),rep("not_grad",23))
grad<-data.frame(gender, Outcome);head(grad)
table(grad$gender, grad$Outcome)

n1<-sum(gender == "Men") 
n2 <-sum(gender == "Women")
drop.risk.men <- sum((gender == "Men") & (Outcome == "not_grad"))/n1; drop.risk.men
drop.risk.women <- sum((gender == "Women") & (Outcome == "not_grad"))/n2; drop.risk.women
relative.dropout <- drop.risk.men/drop.risk.women
# split the data 
MenGrad <- subset(grad, select = Outcome, subset = (gender == "Men"), drop = TRUE)
WomenGrad <- subset(grad, select = Outcome, subset = (gender == "Women"), drop = TRUE)
# bootstrapping 
N <- 10^4; ratio <- numeric(N); prop1 <-numeric(N); prop2 <- numeric(N)
for (i in 1:N) {
  sample1 <-sample(MenGrad,n1, replace = TRUE)
  sample2 <-sample(WomenGrad,n2, replace = TRUE)
  prop1[i] <- mean(sample1 == "not_grad")
  prop2[i] <- mean(sample2 == "not_grad")
  ratio[i] <-prop1[i]/prop2[i]
}
hist(ratio, xlab = "Relative Risk of Not Graduating") 
abline(v = mean(ratio), col = "red")
abline(v = relative.dropout, col = "blue")
bias <- mean(ratio) - relative.dropout; bias  #close to the number on page 123
stderr <- sd(ratio); stderr
# plot percentile confidence intervals
CI = quantile(ratio, c(0.025, 0.975)); CI
abline(v = CI[1], col = "green")
abline(v = CI[2], col = "green")
print('1997 Men enrolled at Harvard likely had higher dropout rates than women')
