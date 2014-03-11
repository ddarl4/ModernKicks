# Larson Hogstrom - HW6
# MATH_E156 - 3/10/2014

### Part 1 



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
abline(v = bootlow, col = "red", lty = 2); abline(v = boothigh, col = "blue", lty = 2)

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

# d) what effect did removing the outlier have on the bootstrap distribution,
# in particular, the standard error?
bootSE # origonal SE
bootSE.drop # outlier removed
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
N <- 10^4; ratio <- numeric(N); prop1 <-numeric(N); prop2 <- numeric(N)
for (i in 1:N) {
  sample1 <-sample(highBP,n1, replace = TRUE)
  sample2 <-sample(lowBP,n2, replace = TRUE)
  prop1[i] <- mean(sample1 == "Die")
  prop2[i] <- mean(sample2 == "Die")
  ratio[i] <-prop1[i]/prop2[i]
}

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
obsRatio <-

# d) Find the 95% bootstrap percentile interval for the ratio of means. Interpret
# this interval

# e) What is the bootstarp estimate of the bias? What fraction of the 
# bootstrapstandard error does it represent?

### Part 4

# Of the students who entered Harvard as freshmen in Fall 1997, 41 of 819 men 
# and 23 of 756 women failed to graduate within 5 years. Conduct a bootstrap 
# analysis, in the manner of Example 5.7, of the relative risk of failing to 
# graduate within five years based on gender. Include a plot like figure 5.17 
# that illustrates the bootstrap percentile confidence interval for the 
# relative risk.
