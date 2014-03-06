#Math E-156 Script 6C-BootstrapRealData.R

#Topic 1 - a case where the population distribution is both unknown and skewed
#This is example 5.3 from the textbook -- arsenic in Bangladesh well water
Bangladesh<-read.csv("Bangladesh.csv"); head(Bangladesh)
Arsenic<-Bangladesh$Arsenic
hist(Arsenic, breaks= "FD") #not remotely normal; seriously skewed
qqnorm(Arsenic)    #compare Figure 5.6 from the textbook
n<-length(Arsenic);n
#The distribution is so skewed that even with n = 271 the CLT does not give a good approximation
#Now generate the bootstrap samples
N<-10^4; arsenic.mean <- numeric(N)
for (i in 1:N) {
  arsenic.mean[i]<-mean(sample(Arsenic, n, replace = TRUE))
}
hist(arsenic.mean, breaks= "FD",main = ("Bootstrap distribution of means"), probability = TRUE)
curve(dnorm(x, mean(arsenic.mean), sd(arsenic.mean)), col = "red", add = TRUE)
#The tails do not match the normal distribution
qqnorm(arsenic.mean); qqline(arsenic.mean) #figure 5.7

#It is traditional to use a normal approximation to get a confidence interval
#For the lowest and highest 2.5% of a normal distribution, go 1.96 standard deviations from the mean
low<-qnorm(.025, mean(arsenic.mean), sd(arsenic.mean))
high<-qnorm(.975, mean(arsenic.mean), sd(arsenic.mean))
curve(dnorm(x,mean(arsenic.mean), sd(arsenic.mean)), from = 60, to = 210)
abline(v = low, col = "red"); abline(v = high, col = "blue")
#2.5% below the red line; 2.5% above the blue line
sum(arsenic.mean < low)/N #only 1.6% below this value
sum(arsenic.mean > high)/N #3.4% below this value
hist(arsenic.mean, breaks= "FD",main = ("Bootstrap distribution of means"), probability = TRUE)
abline(v = low, col = "red"); abline(v = high, col = "blue")

#We can use the bootstrap distribution to get a 95% "bootstrap percentile confidence interval" for the true mean
#Careful -- for a non-symmetrical distribution, this leads to a different interval than in script 6A.
#Its main merit is its extreme simplicity.
quantile(arsenic.mean, c(.025, .975)) #not symmetric about the mean
bootlow <- quantile(arsenic.mean, .025); boothigh <- quantile(arsenic.mean, .975);
abline(v = bootlow, col = "red", lty = 2); abline(v = boothigh, col = "blue", lty = 2)
#The dotted lines capture a bootstrap percentile confidence interval with 2.5% on either side

#Here is the approach of script 6A, leading to a "basic bootstrap confidence interval."
#This worked for samples drawn from a gamma distribution
#Calculate the amount X.add to add to the sample mean
#X.add <- mean(my.boot)-quantile(my.boot,0.025); X.add 
#Calculate an amount X.sub to subtract from the sample mean
#X.sub <- quantile(my.boot,0.975)-mean(my.boot); X.sub 

#Adding and subtracting the appropriate quantities we get
boothigh.basic <-2*mean(arsenic.mean) -quantile(arsenic.mean,0.025)
bootlow.basic <- 2*mean(arsenic.mean)-quantile(arsenic.mean,0.975)
#Here is the percentile bootstrap confidence interval
bootlow; boothigh
#Here is the basic bootstrap confidence interval
bootlow.basic; boothigh.basic
abline(v = bootlow.basic, col = "red", lty = 3); abline(v = boothigh.basic, col = "blue", lty = 3)

#The two styles of confidence interval disagree about which way to adjust for skewness!


#Topic 2 - a two-sample bootstrap
#This is  Example 5.4 from the textbook.

#Read data about minutes of commercials per half hour on basic and extended cable TV.
TV<-read.csv("TV.csv")
times.Basic<-subset(TV, select = Times, subset = (Cable == "Basic"), drop = T)
times.Ext<-subset(TV, select = Times, subset = (Cable == "Extended"), drop = T)
rbind(times.Basic, times.Ext) # table 5.4 from the book

#Let's draw 10000 bootstrap samples and look at the difference in mean commercial times
N<-10^4; times.diff.mean <- numeric(N)
for (i in 1:N) {
  Basic.sample <- sample(times.Basic, 10, replace = TRUE)
  Ext.sample <- sample(times.Ext, 10, replace = TRUE)
  times.diff.mean[i] <- mean(Basic.sample) - mean(Ext.sample)
}
hist(times.diff.mean, breaks = "FD", main= "Bootstrap distribution of time difference")
#Extract a 95% bootstrap percentile confidence interval
quantile(times.diff.mean, c(.025, .975))

#Topic 3 -- Bootstrapping other statistics

#Use the Verizon repair time data
Verizon<-read.csv("Verizon.csv");head(Verizon)
#Split into two groups
Time.ILEC <- subset(Verizon, select = Time, Group == "ILEC", drop = T)
Time.CLEC <- subset(Verizon, select = Time, Group == "CLEC", drop = T)
length(Time.ILEC); length(Time.CLEC)
#Get the ratio of repair times for bootstrap samples
#This statistic is "biased": its mean does not equal the ratio of the means.

N = 10^4; time.ratio.mean <- numeric(N)
ILEC.mean<-numeric(N); CLEC.mean<-numeric(N) # save sample means for plotting

for (i in 1:N) {
  ILEC.sample <- sample(Time.ILEC, 1664, replace = TRUE)
  CLEC.sample <- sample(Time.CLEC, 23, replace = TRUE)
  ILEC.mean[i]<-mean(ILEC.sample)
  CLEC.mean[i]<-mean(CLEC.sample)
  time.ratio.mean[i] <- mean(ILEC.sample)/mean(CLEC.sample)
}
hist(time.ratio.mean, main = "Bootstrap distribution of ratio of means")
abline(v=mean(time.ratio.mean), col = "red", lty = 2)
abline(v=mean(Time.ILEC)/mean(Time.CLEC), col = "blue", lty = 4)

#Compare with a normal distribution
qqnorm(time.ratio.mean)
qqline(time.ratio.mean)#really skewed!
#This time it makes sense to calculate the bias as well as the quantiles
quantile(time.ratio.mean, c(.025, .975))
mean(time.ratio.mean) - mean(Time.ILEC)/mean(Time.CLEC) #the bias
sd(time.ratio.mean) # assess bias relative to this

#Making a scatter plot like Figure 5.17
plot(CLEC.mean, ILEC.mean, xlim=c(0,40), ylim = c(0,10), xlab = "Repair times for non-Verizon customers", ylab = "Repair times for Verizon customers")
abline(v=mean(Time.CLEC), col = "red")
abline(h=mean(Time.ILEC), col = "red")
abline(0, quantile(time.ratio.mean, .025), col = "blue", lty = 2)
abline(0, quantile(time.ratio.mean, .975), col = "blue", lty = 2)
abline(0, quantile(time.ratio.mean, .5), col = "green", lty = 4)

#Now the bootstrap confidence interval is between the blue lines
#The fact that the green 50% line misses the means is another indication of bias