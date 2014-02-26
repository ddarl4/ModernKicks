# Larson Hogstrom - HW4
# MATH_E156 - 2/20/2014

#### Part 1 ###

# 25 on page 73:
# Fantasy 5 - a player tries to match 5 numbers chosen from 1 through 39.
# determin whether  or not the winning numbers are randomly drawn

# solve by chi-square goodness-of-fit
Lottery <-read.csv("Lottery.csv"); 
# create contingency table
N=39; counts<-numeric(N) 
for (i in 1:N) {
  counts[i] = sum(Lottery$Win == i)
}
plot(1:39,counts, type = "h")
expected <- rep(500/39,39)
# calculate chi squared statistic manually
obsChisq <- sum((counts-expected)^2/expected); obsChisq
# put in table form
countTable <- matrix(counts,ncol=39,byrow=TRUE)
colnames(countTable) <- 1:N
rownames(countTable) <- c("freq")
countTable <- as.table(countTable); countTable
# run built in test
chisq.test(countTable)
print("The built in chisq test gives good agreement with the manually
calculated value ")
# solve it by assuming all numbers equally likely and drawing lots 
# of random samples of 500 numbers
B <- 10^4-1
result<-numeric(B)
for (j in 1:B)
 {
    vec.rand <-sample(1:39,500,replace=TRUE)
    counts<-numeric(N) 
    for (i in 1:N) {
      counts[i] = sum(vec.rand == i)
    }
    result[j] <- sum((counts-expected)^2/expected)
 }
hist(result, xlab="May-June percent difference", main="Difference between fraction of delays over 20 minutes")
abline(v=obsChisq,col="blue",lty=5)
# two-tailed test
pVal<-(length(which(result >= obsChisq))+1)/(length(result)+1); pVal
print("Results from both chi-square tests do not support rejecting the null 
    hypothesis that Cal. Lottery data come from random data.")

#### Part 2 ###

# 70 randoms numbers:
# 30 in interval [1,1.5)
# 18 in interval [1.5,2)
# 9 in interval [2,3)
# 10 in interval [3,5)
# 3 in [5+)
# is it plausable that these numbers were drawn from a distribution with the pdf:

### load distribution
# install.packages("actuar") #comment this out after the first time
library(actuar)
# f(x) = 2/x^3 for x>=1s
# The Pareto distribution with parameters ‘shape’ = a and ‘scale’ =
# s has density:
#           f(x) = a s^a / (x + s)^(a + 1)

### reproduce observed table
# Obs <- c(30,18,9,10,3)/70
Obs <- matrix(c(30,18,9,10,3),ncol=5,byrow=TRUE)
colnames(Obs) <- c("[1,1.5)","[1.5,2)","[2,3)","[3,5)","[5+)")
rownames(Obs) <- c("counts")
Obs <- as.table(Obs); Obs

### calculated expected table using integration - Method 1
curve(2/x^3,from = 1, to = 14)
integ<-integrate(function(x) {2/x^3}, 1, Inf); integ
print("The expected function integrates to one between 1 and Inf")
bin1 <- integrate(function(x) {2/x^3}, 1, 1.5)
bin2 <- integrate(function(x) {2/x^3}, 1.5, 2)
bin3 <- integrate(function(x) {2/x^3}, 2, 3)
bin4 <- integrate(function(x) {2/x^3}, 3, 5)
bin5 <- integrate(function(x) {2/x^3}, 5, Inf)
Expected.proportion <- c(bin1$value,bin2$value,bin3$value,bin4$value,bin5$value)
Expected <- Expected.proportion*70

### observed chi squared - compare observed counts to expected
obsChisq <- sum((Obs-Expected)^2/Expected); obsChisq
### permutation
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
    rp <- rpareto(1000, shape=2, scale=1)
    rpGt1 <- rp[rp >1][1:70]
    bin1 <- sum(rpGt1 <= 1.5)
    bin2 <- sum(rpGt1 <= 2 & rpGt1 >=1.5)
    bin3 <- sum(rpGt1 <= 3 & rpGt1 >=2)
    bin4 <- sum(rpGt1 <= 5 & rpGt1 >=3)
    bin5 <- sum(rpGt1 > 5)
    perm.table <- c(bin1,bin2,bin3,bin4,bin5)
    result[i] <- sum((perm.table-Expected)^2/Expected) 
 }
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic")
abline(v=obsChisq,col="blue",lty=5)
chMore<-which(result >= obsChisq)
pVal<-(length(chMore)+1)/(length(result)+1); pVal # one tailed

### calcualte expected table with R function:
# 1) generate a large set of random values from the expected Pareto distribution
# 2) see which of these are greater than 1
# 3) calculuate an expected table using the same bins in the observed counts
rp <- rpareto(100000, shape=2, scale=1)
rpGt1 <- rp[rp >1]
bin1 <- sum(rpGt1 <= 1.5)
bin2 <- sum(rpGt1 <= 2 & rpGt1 >=1.5)
bin3 <- sum(rpGt1 <= 3 & rpGt1 >=2)
bin4 <- sum(rpGt1 <= 5 & rpGt1 >=3)
bin5 <- sum(rpGt1 > 5)
Expected.proportion <- c(bin1,bin2,bin3,bin4,bin5)/length(rpGt1)
Expected <- Expected.proportion*70
### method 2 
bin1 <- dpareto(1, shape=2, scale=1) - dpareto(1.5, shape=2, scale=1)
bin2 <- dpareto(1.5, shape=2, scale=1) - dpareto(2, shape=2, scale=1)
bin3 <- dpareto(2, shape=2, scale=1) - dpareto(3, shape=2, scale=1)
bin4 <- dpareto(3, shape=2, scale=1) - dpareto(5, shape=2, scale=1)
bin5 <- dpareto(2, shape=2, scale=1)
allbins <- c(bin1,bin2,bin3,bin4,bin5) 
Expected.proportion <- allbins * (1/sum(allbins))
Expected <- Expected.proportion*70


print("not sure why the ")


#### Part 3 ###

#     Like Much   Like   Neither   Dislike   Dislike Mutch
# B     180       260    137       96        52 
# G     210       266    145       85        49 
# a) replicate contingency table
candyTable <- matrix(c(180,260,137,96,52,210,266,145,85,49),ncol=5,byrow=TRUE)
colnames(candyTable) <- c("Like Much","Like", "Neither", "Dislike", "Dislike Mutch")
rownames(candyTable) <- c("Boy","Girl")
candyTable <- as.table(candyTable); candyTable
Expected <- outer(rowSums(candyTable), colSums(candyTable))/sum(candyTable);Expected
obsChisq <- sum((candyTable-Expected)^2/Expected); obsChisq
# use built-in chi-squared test
chisq.test(candyTable)

# b) create data using dataframe, permute the Sex column 
# and caluculate chi square distance
Gender <- c(rep("Boy",725),rep("Girl",755))
likeBoy <- c(rep("Like_Much",180),rep("Like",260),rep("Neither",137),rep("Dislike",96),rep("Dislike_Mutch",52))
likeGirl <- c(rep("Like_Much",210),rep("Like",266),rep("Neither",145),rep("Dislike",85),rep("Dislike_Mutch",49))
likeStatus <- c(likeBoy,likeGirl)
Candy <- data.frame(Gender,likeStatus); head(Candy)
candyTable2 <- table(Candy$Gender, Candy$likeStatus); candyTable2
Expected <- outer(rowSums(candyTable2), colSums(candyTable2))/sum(candyTable2);Expected
#permutation test
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
   Gender.permutation <-sample(Candy$Gender)
   perm.table <- table(Gender.permutation, Candy$likeStatus)
   # result[i]<-chisq.test(perm.table)
   result[i] <- sum((perm.table-Expected)^2/Expected) 
 }
#Create a histogram
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic", xlim=c(0,30))
abline(v=obsChisq,col="blue",lty=5)
chMore<-which(result >= obsChisq)
pVal<-(length(chMore)+1)/(length(result)+1); pVal # one tailed
print("Results from the chi-square test do not support rejecting the null 
    hypothesis that candy preferences for boys and girls are the same.")

#### Part 4 ###
vec22 <-read.csv("Exercise22.csv");
# could 50 values have come from the normal distribution N(22,7^2)
# a) use qnorm command in R to find .2, .4, .6, .8 quantiles of the normal
# distribution - (points that mark off equal probabilities). 
hist(vec22$var1, breaks = "fd", freq = FALSE)
curve(dnorm(x,22,7), col = "red", add = TRUE)
quantiles <-qnorm(c(.2,.4,.6,.8),22,7)

# b) use these quantiles to determine your intervals and count the number 
# of values that fall in each interval

nValues <- length(vec22$var1)
c1 <- sum(vec22$var1 <= quantiles[1]); c1
c2 <- sum((vec22$var1 > quantiles[1]) & (vec22$var1 <= quantiles[2])); c2
c3 <- sum((vec22$var1 > quantiles[2]) & (vec22$var1 <= quantiles[3])); c3
c4 <- sum((vec22$var1 > quantiles[3]) & (vec22$var1 <= quantiles[4])); c4
c5 <- sum((vec22$var1 > quantiles[3]) & (vec22$var1 <= quantiles[4])); c5
c6 <- sum(vec22$var1 > quantiles[4]); c6
counts <- c(c1,c2,c3,c4,c5,c6)

# put quantile counts into table form
countTable <- matrix(counts,ncol=6,byrow=TRUE)
colnames(countTable) <- 1:6
rownames(countTable) <- c("count")
countTable <- as.table(countTable); countTable

# c) Finish the goodness-of-fit test
chisq.test(countTable)
print("Results from the chi-square test do not support rejecting the null 
    hypothesis the values come from the distribution N(22,7^2).")


