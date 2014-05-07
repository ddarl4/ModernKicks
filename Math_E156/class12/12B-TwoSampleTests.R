#Math E-156 Script 12B-TwoSampleTests.R

#Topic 1 -- two samples might come from normal distributions with same mean, unknown sigma
#Subtract the means; add the standard errors; use Welch's clever formula for degrees of freedom.

#Example 8.4 in the textbook applies this approach to NC babies
NCB <- read.csv("NCBirths2004.csv"); head(NCB)

#The null hypothesis is that the mean is the same whether or not the mother smokes.
#Extract a vector for each sample.
weightNS <- subset(NCB, select=Weight, subset = (Smokers =="No"), drop = T)
weightS <- subset(NCB, select=Weight, subset = (Smokers =="Yes"), drop = T)

#Calculate mean, standard deviation, and size for each sample
xbarNS <- mean(weightNS); xbarS <- mean(weightS)
sNS <- sd(weightNS); sS <- sd(weightS)
nNS <- length(weightNS); nS <- length(weightS)

#If the means are equal, we can make a statistic with the t distribution
t <- (xbarNS - xbarS)/sqrt(sNS^2/nNS + sS^2/nS); t    

#Welch's formula gives a reasonable value for degrees of freedom.
nu <- (sNS^2/nNS + sS^2/nS)^2/((sNS^2/nNS)^2/(nNS-1)+ (sS^2/nS)^2/(nS-1));nu
#Here is the probability for the observed difference in means to arise by chance.
pt(t, df = nu, lower.tail = FALSE)
#Of course this test has been automated, and we even get a confidence interval
t.test(weightNS, weightS, alt = "greater")

#Topic 2 -- testing whether two binomial distributions have the same proportion

#550 of 684 women and 425 of 563 men beieve in an afterlife
#Using the exact binomial distributions would be messy, so use a normal approximation
X1 = 550; n1 = 684; X2 = 425; n2 = 563

#Combine all the data to get the proportion from which to calculate the variance
p.hat = (X1+X2)/(n1+n2); p.hat

#If all the data come from this distibution, this statistic should be N(0,1).
z = (X1/n1-X2/n2)/sqrt(p.hat*(1-p.hat)*(1/n1+1/n2));z   

#This value could have been looked up in table a hundred years ago.
pnorm(z, lower.tail = FALSE)    #agrees with page 220
#Of course we can get the same number from a chi-square test
pchisq(z^2, df =1, lower.tail = FALSE)/2   #divide by 2 for a 1 sided test
#It is the chi-square version that is automated.
prop.test(c(550,425), c(684, 563), correct = FALSE, alt = "greater")
#Page 221 of the text does a two-sided test insted of a one-sided test.

#Topic 3 -- comparing two binomial distributions by a permutation test

#Make two vectors to simulate the data
Gender = c(rep("F", n1), rep("M", n2))
Afterlife = c(rep(TRUE, X1), rep(FALSE,n1-X1), rep(TRUE,X2), rep(F, n2-X2))
table(Gender, Afterlife) #display results as a contingency table

#Check our sampling methodology
x = sample(Gender)   #random permutation
diff <- sum((x=="F") & (Afterlife ==  TRUE)) - sum ((x=="M") & (Afterlife ==  TRUE)); diff
table(x, Afterlife)   #display results as a contingency table

#Now just permute the gender vector and compute the preponderance of females
N <- 2*10^4;  diff <- numeric(N)
for (i in 1:N) {
  x = sample(Gender)   #random permutation of the gender vector
  diff[i] <- sum((x=="F") & (Afterlife ==  TRUE)) - sum ((x=="M") & (Afterlife ==  TRUE))
}
hist(diff)
abline(v = X1-X2,col = "red")
sum(diff >= X1-X2)/N     #disagrees with normal approximation but is probably better

#An alternative approach is to use Fisher's exact test for the hypergeometric distribution.
fisher.test(table(Afterlife, Gender), alt = "less")  #this is exact


