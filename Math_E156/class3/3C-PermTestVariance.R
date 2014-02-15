#Math E-156 Script 3C-PermTestVariance.R
#Relevant to the end of example 3.5 on page 51

#Topic 1 - A permutation test for equal variance
#Draw 32 samples from a normal distribution with expectation 0, variance 3^2
x <- rnorm(32, 0, 3);x   #sample 1
#Draw 40 samples from a normal distribution with expectation 2, variance 2^2
y <- rnorm(40, 2, 2);y   #sample 2
var(x); var(y)  #look at the sample variances, which will not be exactly 9 and 4
#Because we used var(), their expectations are 9 and 4, however
Fratio = var(x)/var(y); Fratio  #called the "F ratio" statistic

#We will do a permutation test to try to show that sample 1 has a significantly larger variance
#Before permuting, subtract the sample means: this does not change the variances
x.zero <- x - mean(x)   # has zero mean
y.zero <- y - mean(y)   #has zero mean
#Now we can do a permutation test
#The idea is to select a subset of 16 at random and look at the F ratio.
z <- c(x.zero, y.zero)  #combine the samples into a single vector.
N<- 10^4 - 1; ratios <- numeric(N)
for (i in 1:N) {
  index <- sample(1:72, 32); index   #select a random subset
  x.new = z[index]    #32 samples chosen at random
  y.new = z[-index]   #the remaining 40 samples
  ratios[i] <- var(x.new)/var(y.new) #F ratio for random selection
}
hist(ratios, breaks = "FD", probability = TRUE)
abline(v= Fratio, col = "red")
pValue = (sum (ratios > Fratio) + 1)/(N+1); pValue
#For normal variables, the distribution of the F ratio is known
curve(df(x,31,39), col = "blue", add = TRUE)
#So is the probability of exceeding the observed F ratio
pf(Fratio, 31, 39, lower.tail = FALSE)
#And this has been automated, of course
var.test(x,y, alt = "greater")
#Of course, if we could sample from the population the fit would be better
N<- 10^4 - 1; ratios <- numeric(N)
for (i in 1:N) {
  x.pop = rnorm(32, 0, 1)    #32 samples chosen from N(0,1)
  y.pop = rnorm(40, 2, 1)    #40 samples chosen from N(0,1)
  ratios[i] <- var(x.pop)/var(y.pop) #F ratio for random selection
}
hist(ratios, breaks = "FD", probability = TRUE)
curve(df(x,31,39), col = "blue", add = TRUE)
#So if we know that the samples come from normal distributions we don't need the permuation test
#The classical F ratio distribution replicates the histogram


#Topic 2 - Testing real-world data for unequal variance
#Do men and women have the same variance for their consumption of hot wings?
#The central limit theorem says nothing about spicy chicken wings.
Beerwings<-read.csv("Beerwings.csv") #load the data
index=which(Beerwings$Gender=="M")   #find the rows for males
x <- Beerwings$Hotwings[index]   #data for males
y <- Beerwings$Hotwings[-index]   #data for females
nM <- length(x); nF <- length(y); nM; nF   #15 rows for each gender
Fratio = var(x)/var(y); Fratio  #men show higher variance - might be significant
#Before permuting, subtract the sample means
x.zero <- x - mean(x)   #has zero mean
y.zero <- y - mean(y)   #has zero mean
Wings <- c(x.zero,y.zero) #make one vector

#Now do a permutation test by extracting random samples of 15
N=2*10^4-1 ; ratios<-numeric(N)   #use smaller N on a laptop!
for (i in 1:N) {
  index = sample(nM+nF, size=nM, replace = FALSE) #random subset
  ratios[i]=var(Wings[index])/var(Wings[-index])
}
hist(ratios, breaks = "FD", prob = TRUE)
abline(v = Fratio, col = "red")
pValue = (sum (ratios > Fratio) + 1)/(N+1); pValue   #about 10% chance to exceed to observed ratio by chance
#Compare with the F distribution
curve(df(x,14,14), col = "blue", add = TRUE)
pf(Fratio, 14, 14, lower.tail = FALSE)
#In this case the classical test does not replicate the P-value from the permutation test

#Topic 3 - Testing for equal variance in skewed data
#Try to replicate the P-value on page 50 of the textbook
Verizon<-read.csv("Verizon.csv") 
Time<-Verizon$Time
Extract the times for Verizon and non-Verizon customers
Time.ILEC <- subset(Verizon, select = Time, subset = Group == "ILEC", drop = T) #data for Verizon customers
Time.CLEC <- subset(Verizon, select = Time, subset = Group == "CLEC", drop = T) #data for Verizon customers

#Check that the counts and means agree with pages 44-45
#Non-Verizon customers - larger mean and variance
nCLEC <- length(Time.CLEC); nCLEC ; mean(Time.CLEC); var(Time.CLEC);
#Verizon customers - smaller mean and variance
nILEC <- length(Time.ILEC); nILEC; mean(Time.ILEC); var(Time.ILEC)

#Compute the observed ratio of varainces
observed = var(Time.ILEC)/var(Time.CLEC); observed  #smaller for Verizon customers, might be significant
Now do the permutation test just as above
N=2*10^4-1 ; result<-numeric(N)   #use smaller N on a laptop!
for (i in 1:N) {
  index <- sample(nCLEC+nILEC, size=nILEC, replace = FALSE) #random large subset
  result[i]<- var(Time[index])/var(Time[-index])
}
hist(result, breaks = "FD", prob = TRUE, xlim = c(0, 15))
abline(v = observed, col = "red")
pValue = (sum (result <= observed) + 1)/(N+1); 2*pValue #double for 2-sided test
#So a ratio this small arises 13.5 percent of the time if we choose 23 customers at random

#One might worry about dividing by a small variance from the sample of 23
#Same analysis with the smaller sample in the numerator
observed = var(Time.CLEC)/var(Time.ILEC); observed  #larger for non-Verizon customers, might be significant
N=2*10^4-1 ; result<-numeric(N)   #use smaller N on a laptop!
for (i in 1:N) {
  index <- sample(nCLEC+nILEC, size=nCLEC, replace = FALSE) #random small subset
  result[i]<- var(Time[index])/var(Time[-index])
}
hist(result, breaks = "FD", prob = TRUE, xlim = c(0, 10))
abline(v = observed, col = "red")
pValue = (sum (result >= observed) + 1)/(N+1); 2*pValue #double for 2-sided test
#The result is exactly the same!
#This time the ratio with a random sample of 23 exceeds the observed ratio 13.5 percent of the time

#Compare with the F distribution
curve(df(x,nCLEC-1,nILEC-1), col = "blue", add = TRUE) #quite different from the histogram
pf(observed, nCLEC-1,nILEC-1, lower.tail = FALSE)
#The data are so skewed, and the samples so unequal in size, that this test is meaningless




