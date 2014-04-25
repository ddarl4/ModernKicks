#Math E-156 Script 11A-DifferenceMeans.R

#Topic 1 -- confidence interval for the difference of means
#Example 7.8 from the textbook
#Load data about reading scores for kids
#The "Treated" group participated in directed reading activities
Reading <- read.csv("Reading.csv"); Reading

#Extract vectors for the two groups
treated <- subset(Reading, select = Response, subset = (Treatment == "Treated"), drop = TRUE)
control <- subset(Reading, select = Response, subset = (Treatment == "Control"), drop = TRUE)
M1<-mean(treated); M2<-mean(control); M1; M2 #is this difference significant?
n1 <- length(treated); n2 <- length(control); n1; n2 #not the same size

#We do not know the variances but we can calculate the sample standard deviations
S1<- sd(treated); S2<- sd(control); S1; S2

#Suppose the groups were independent and the distributions were normal.
#Then each sample mean comes from a normal distribution with known variance.
#So we know the standard error of the difference of means.
#The best we can do (Student style) is to use the sample variance instead.
#This estimate of the standard error is the appropriate Student denominator.

SE <- sqrt(S1^2/n1 + S2^2/n2); SE

#Welch's approximation (equation 7.11) gives a reasonable value for degrees of freedom
ndf <- (S1^2/n1 + S2^2/n2)^2/(S1^4/(n1^2*(n1-1)) + S2^4/(n2^2*(n2-1))); ndf

    #Assume that the difference in means divided by the standard error is Student t
#Get the quantiles for the t distribution with this many degrees of freedom
tqnt<- qt(c(.025, .975),ndf); tqnt

#Multiply by the sample standard error to create a 95% confidence interval
interval<- (M1-M2)+ SE*tqnt; interval #large, but does not include zero!

#Topic 2 -- using Student t to compute a P value

#We can calculate the probability that such a large rescaled mean could arise by chance
curve(dt(x, df  = ndf), from = -5, to =5)
abline(v = (M1-M2)/SE, col = "red" )
pval <-pt((M1-M2)/SE, df = ndf, lower.tail = FALSE); pval

#For a two-sided test we ought to double this
2*pval    #compare with the automated test

#Of course, this has all been automated
t.test(treated, control)
interval #same as we got by hand

