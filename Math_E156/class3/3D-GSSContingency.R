#Math E-156 Script 3D-GSSContingency.R
#Based on pp. 54-57 of the textbook, but using different factors

#Topic 1 - the chi-square statistic
#Suppose we have "Observed" and "Expected" values for a quantity.
#Then (Obs-Exp)^2 is a statistic that measures the discrepancy.
#There are sound theoretical reasons to rescale and compute (Obs-Exp)^2/Exp.
#Here is a demonstration -- proof will come later
#Do an experiment with n trials, each with probability p of success.
#The random variable X (number of successes) has a binomial distribution.
#We expect N*p successes and N*(1-p) failures,
n <- 5000; p <- 0.2
#Here is a function that generates a table of expected successes and failures
outer(n,c(p,1-p))
N<- 10^4; values <- numeric(N) #vector to store results
for (i in 1:N) {
  successes = rbinom(1,n,p)
  #Make a vector of successes and failures
  Obs = c(successes, n-successes)
  #Sum the scaled discrepancy for successes and failures
  Expected <- outer(n,c(p,1-p))
  values[i] <- sum((Obs-Expected)^2/Expected)
}
#Display a histogram of the result
hist(values, breaks = 40, probability = TRUE, xlim = c(0,10))
mean(values)   #theoretical value for chi-square is 1
#Overlay the density function for chi-square with one degree of freedom
curve(dchisq(x,df=1), col = "red", add = TRUE)

#The same approach works for a 2 x 2 contingency table.
#We can make a data frame with n rows, two factors
Factor X has r1 rows of TRUE, r2 = n-r1 rows of FALSE
Factor Y has c1 rows of TRUE, c2 = n-c1 rows of FALSE
X <- c(rep("Blue",75),rep("Red",125))
Y <- c(rep("Large",150),rep("Small", 50))
#Assuming that factors X and Y are independent we can make a data frame
XYFrame <- expand.grid(X,Y); head(XYFrame)
table(XYFrame$Var1, XYFrame$Var2)
#Now suppose that we permute the sizes in the second column
#The number of large blue objects is now given by a hypergeometric distribution
LargeBlue <- rhyper(1,30000,10000,15000); LargeBlue
#This means, "with 30000 large objects and 10000 small objects, choose 15000 and make them blue."
#Permuting the colors in the first column is an equally good alternative.
LargeBlue <- rhyper(1,15000,25000,30000); LargeBlue
#This means, "with 15000 blue objects and 25000 red objects, choose 30000 and make them large."

#Now the other entries in the table are all determined
Obs <- array(c(LargeBlue, 30000- LargeBlue,15000-LargeBlue,LargeBlue-5000),c(2,2));Obs
#We can reconstruct the expected values, assuming independence
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs);Expected
#Sum the scaled discrepancy for successes and failures over the table
sum((Obs-Expected)^2/Expected)
#Notice that there is still only one "degree of freedom"
#Now do this many times and tally the results
N<- 10^4; values <- numeric(N) #vector to store results
for (i in 1:N) {
  LargeBlue <- rhyper(1,30000,10000,15000)
  Obs <- array(c(LargeBlue, 30000- LargeBlue,15000-LargeBlue,LargeBlue-5000),c(2,2));Obs
  Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs);Expected
  values[i] <- sum((Obs-Expected)^2/Expected)
}
#Display a histogram of the result
hist(values, breaks = 40, probability = TRUE, xlim = c(0,10))
#Overlay the density function for chi-square with one degree of freedom
curve(dchisq(x,df=1), col = "red", add = TRUE)

#Define the function that computes the chi square statistic from a contingency table
chisq <-function(Obs){
  Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}

#Topic 2 - A permutation test for independence
#This is like section 3.4.1, but using different columns
#Load the General Social Survey data set
GSS<-read.csv("GSS2002.csv"); head(GSS)
#Identify the columns with non-empty entries for both Marital and SpendEduc
index=which(!is.na(GSS$Marital) & !is.na(GSS$SpendEduc))
#Extract the two desired columns, keeping just the selected rows
Marital2<-GSS$Marital[index]; length(Marital2)
SpendEduc2<-GSS$SpendEduc[index]; length(SpendEduc2)
#Look at the contingency table
Obs<-table(Marital2,SpendEduc2); Obs
#In this case, after we know the 8 values in the top left 4x2 block,
#the row sums and column sums determine the last 7 entries
#Compare with the table that would be expected if the factors were independent
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs); Expected
#Calculate a statistic that would be small in the case of independence
observed = chisq(Obs); observed
#Now permute the second column to see the distribution of the statistic
#This takes a few seconds even on a fast computer.
N = 10^4-1; result<- numeric(N)
for (i in 1:N) {
  SE.permuted <- sample(SpendEduc2)
  GSS.table <- table(Marital2, SE.permuted)
  result[i]= chisq(GSS.table)
}
hist(result, probability = TRUE)
abline(v = observed, col="red") #observed chi square is highly unlikely
Pvalue <- (sum(result >= observed) +1)/(N+1); Pvalue
#For comparison, use the built-in chi square test from R
chisq.test(Marital2, SpendEduc2)
#Overlay the chi-square distribution on the histogram
#In this case there are (5-1)*(3-1) = 8 degrees of freedom
curve(dchisq(x, df=8), col = "red", add= TRUE)

#We can cut down to just two alternatives for each factor
Marital3<-factor(subset(GSS,select = Marital,subset = ((Marital=="Married")|(Marital=="Never Married")) & ((SpendEduc=="About right")|(SpendEduc=="Too little")),drop=TRUE))
SpendEduc3<-factor(subset(GSS,select = SpendEduc,subset = ((Marital=="Married")|(Marital=="Never Married")) & ((SpendEduc=="About right")|(SpendEduc=="Too little")),drop=TRUE))
#Look at the contingency table
Obs<-table(Marital3,SpendEduc3); Obs
#Compare with the table that would be expected if the factors were independent
Expected <- outer(rowSums(Obs), colSums(Obs))/sum(Obs); Expected
observed = chisq(Obs); observed
#Now permute the second column to see the distribution of the statistic
#This takes a few seconds even on a fast computer.
N = 10^4-1; result<- numeric(N)
for (i in 1:N) {
  SE.permuted <- sample(SpendEduc3)
  GSS.table <- table(Marital3, SE.permuted)
  result[i]= chisq(GSS.table)
}
hist(result,probability = TRUE, xlim = c(0,10))
abline(v = observed, col="red") #observed chi square could easily arise by chance
Pvalue <- (sum(result >= observed) +1)/(N+1); Pvalue
#For comparison, use the built-in chi square test from R
chisq.test(Marital3, SpendEduc3)
#Overlay the chi-square distribution on the histogram
curve(dchisq(x, df=1), col = "red", add= TRUE); just one degree of freedom

