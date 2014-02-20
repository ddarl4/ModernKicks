# Larson Hogstrom - HW3
# MATH_E156 - 2/16/2014

#### Part 1 ###

# 1. Problem 9 on page 70 (permutation test for a numeric variable)
# a) compare the empirical distribuion functions of the number of strikeouts
# per game (StrikeOuts) for games played at home and games played away (Locastion)
phil <-read.csv("Phillies2009.csv"); 
hist(phil[phil$Location == 'Home',]$StrikeOuts, breaks ="FD",col=rgb(1,0,0,0.5))
hist(phil[phil$Location == 'Away',]$StrikeOuts, breaks ="FD",col=rgb(0,0,1,0.5),add=TRUE)
legend("topright", c("Away", "Home"), fill=c("blue", "red"))
# b) Find the mean number of strikeouts per game for the games played at
# home and games played away from home
index <- which(phil$Location == 'Home'); index    #row numbers for the home games
SOhome <- mean(phil$StrikeOuts[index]); SOhome
SOAway <- mean(phil$StrikeOuts[-index]); SOAway
homefield <- mean(phil$StrikeOuts[index])-mean(phil$StrikeOuts[-index]); homefield

# c) perform a permutation test to see if the differences in means is statstically 
# significant
N=10^5-1; result<-numeric(N)  #99999 random subsets should be enough
for (i in 1:N) {
  index = sample(162, size=81, replace = FALSE) #random subset
  result[i]=mean(phil$StrikeOuts[index])-mean(phil$StrikeOuts[-index])
}
hist(result, breaks = "FD")
abline(v = homefield, col = "red")
#calculate p-value
soMore<-which(result >= homefield)
pVal<-(length(soMore)+1)/(length(result)+1); pVal # one tailed
# testing the hypothesis that Home games would result in higher strike outs
#(I'm not sure if these are offensive or definsive strikeouts, so not sure about
    # directionality)

#### Part 2 ###

# 2. Problem 12 on page 71 (permutation and chi square test for a 2 x 2 con- tingency table)
# a) create a table to summarize the relationship between age of target
# consumer and shelf location
Cereals <-read.csv("Cereals.csv"); 
mytable <- xtabs(~Age+Shelf, data=Cereals); mytable
table(Cereals$Age, Cereals$Shelf)
#check that there are two cerials marketed for adults on the bottom shelf
# Cereals[(Cereals$Age == 'adult') & (Cereals$Shelf == 'bottom'),]

# b) Conduct a chi-square test using chisq.test command
chisq.test(Cereals$Age, Cereals$Shelf)
# c) R returns a warning message. Compute the expected counts for each 
# cell to see why
Expected <- outer(rowSums(mytable), colSums(mytable))/sum(mytable);Expected
print("did chisq.test throw a warning because of the small values in the cells?")
obsChisq <- sum((mytable-Expected)^2/Expected); obsChisq
# d) Conduct a permuatation test for independence, adapting the code on
# page 56
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
   Age.permutation <-sample(Cereals$Age)
   perm.table <- table(Age.permutation, Cereals$Shelf)
   # result[i]<-chisq.test(perm.table)
   result[i] <- sum((perm.table-Expected)^2/Expected) 
 }
#Create a histogram
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic", xlim=c(0,30))
abline(v=obsChisq,col="blue",lty=5)
chMore<-which(result >= obsChisq)
pVal<-(length(chMore)+1)/(length(result)+1); pVal # one tailed
 
#### Part 3 ###

# 3. Problem 7 on page 69 (flight delays, including comparison of variances)
# a) compute the proportion of times the flights in May and June were
# delayed more than 20min, and conduct a two-sided test of whether the
# difference between months is statistically significant.
fd <- read.csv("FlightDelays.csv")
# fdMay <- fd[fd$Month == 'May',]
# fdJune <- fd[fd$Month == 'June',]
# grtr20June <- sum(fdJune$Delay > 20) / nrow(fdJune); grtr20June
# grtr20May <- sum(fdMay$Delay > 20) / nrow(fdMay); grtr20May
nFlights <- nrow(fd)
nJune <- sum(fd$Month == 'June')
nMay <- nFlights - nJune
grtr20June <- sum((fd$Month == 'June') & (fd$Delay > 20)) / nJune; grtr20June
grtr20May <- sum((fd$Month == 'May') & (fd$Delay > 20)) / nMay; grtr20May
obsMonthDiff <- grtr20May - grtr20June
print("To test if difference between months is statistically significant
    I am chosing the proportion of flights delayed more than 
    20 minutes as my tests statistic and performing a permutation test")
# permutation testing 
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
    MO.permuted <- sample(fd$Month)
    propJune <- sum((MO.permuted == 'June') & (fd$Delay > 20)) / nJune
    propMay <- sum((MO.permuted == 'May') & (fd$Delay > 20)) / nMay
    result[i] <- propMay - propJune
 }
hist(result, xlab="May-June percent difference", main="Difference between fraction of delays over 20 minutes")
abline(v=obsMonthDiff,col="blue",lty=5)
# two-tailed test
p1<-(length(which(result >= obsMonthDiff))+1)/(length(result)+1)
p2<-(length(which(result <= obsMonthDiff))+1)/(length(result)+1)
pVal <- min(p1,p2)*2; pVal
print("May had significantly less delays over 20 minutes than June")

# b) Compute the variance of the flight delay times in May and June and
# then conduct a two-sided test of whether the ratio of variance is statistically
# significantly different from 1.
varJune <- var(fd[fd$Month == 'June',]$Delay); varJune
varMay <- var(fd[fd$Month == 'May',]$Delay); varMay
obsVarRatio <- varJune/varMay; obsVarRatio
#permutation test
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
    MO.permuted <- sample(fd$Month)
    varJ <- var(fd[MO.permuted == 'June',]$Delay)
    varM <- var(fd[MO.permuted == 'May',]$Delay)
    result[i] <- varJ/varM
 }
hist(result, xlab="May-June percent difference", main="Difference between fraction of delays over 20 minutes")
abline(v=obsVarRatio,col="blue",lty=5)
# two-tailed test
p1<-(length(which(result >= obsVarRatio))+1)/(length(result)+1)
p2<-(length(which(result <= obsVarRatio))+1)/(length(result)+1)
pVal <- min(p1,p2)*2; pVal
print("June had significantly higher variance in flight dealys than May.")

#### Part 4 ###

# 4. In the 2013 regular season, the San Diego Chargers played 16 games, 
# win- ning 9 and losing 7, in the sequence LWLWLWWLLLWLWWWW.
# It is generally felt that they “finished strong” in winning four of their 
# last six games.
# (a) Carry out an exact permutation test to determine the probability 
# that is the wins and losses were scrambled, the last six games would 
# include four or more wins.

gameNumber <- c(1:16)
outcomes <- c("L","W","L","W","L","W","W","L","L","L","W","L","W","W","W","W")
print("the sequence listed on in question 4 shows 5 wins in the last 6 games")
nWin <- sum(outcomes == "W"); nWin
nWinLast <- sum(lastSix == "W"); nWinLast
lastSix <- outcomes[-10:-1]

#exact permutation test
AllSubsets<-combn(1:16,9) # all possible placements of wins
N <-ncol(AllSubsets); N; 
result<-numeric(N)
for (i in 1:N)
 {
    winIndex <- AllSubsets[,i]
    # count wins in the last 6 games
    result[i] <- sum(winIndex >= 11) 
 }
hist(result, xlab="games won", main="Number of wins in the last 6 games")
abline(v=nWinLast,col="blue",lty=5)
# one-tailed test
pVal<-(length(which(result >= nWinLast))+1)/(length(result)+1); pVal

# (b) Replicate your answer by using the multinomial distribution.
pWin <- nWin/length(outcomes) # probability of winning
# dbinom(x, size, prob, log = FALSE) # x, q: vector of quantiles.
# dmultinom(x, size = NULL, prob, log = FALSE)
pVal5Wins <- dmultinom(c(1,0,1,1,1,1), prob=c(pWin,1-pWin,pWin,pWin,pWin,pWin), log = FALSE)

# (c) Do a binomial approximation to find the probability that if the 
# Charg- ers play six games with a probability p = 9/16 of winning each, 
# they will win four or more of the six games.
plot(0:6,dbinom(0:6, 6, pWin), type = "h")
bDist <- dbinom(0:6, 6, pWin, log = FALSE)
pVal4Wins <-sum(bDist[5:7]); pVal4Wins # probability of getting 4 or more wins in the last 6 games
pVal5Wins <-sum(bDist[6:7]); pVal5Wins # probability of getting 5 or more wins in the last 6 games

# (d) Rerun the permutation test by using 10,000 randomly chosen samples of 6 games.
N <-ncol(AllSubsets); N; 
result<-numeric(N)
for (i in 1:N)
 {
    win.permutation <-sample(outcomes,6)
    isWin <- win.permutation == "W"
    result[i] <- sum(isWin) 
 }
hist(result, xlab="games won", main="Number of wins in the last 6 games")
abline(v=nWinLast,col="blue",lty=5)
# one-tailed test
nWinLast <- 4;
pVal4Wins<-(length(which(result >= nWinLast))+1)/(length(result)+1); pVal5Wins


#### Part 5 ###
# 10a) identify the hypothesis to test to see if there is a relationship 
# between gender and diet and carry out the test
# gender vs low-fat diet in midwestern college students
#       Yes    No
# W     35     146
# M     8      97

#Problem 10a on page 70. First carry out the built-in chi-square test 
# in R (The hard part may be getting the data into the right format), 

lowFat <- matrix(c(35,146,8,97),ncol=2,byrow=TRUE)
colnames(lowFat) <- c("Yes","No")
rownames(lowFat) <- c("Women","Men")
lowFat <- as.table(lowFat); lowFat
chisq.test(lowFat)

### permutation test
# repeat the test by creatting a data frame with two columns and 286 rows 
# and carrying out a permutation test using the chi square statistic. You 
# should get good but not perfect agreement.

Gender <- c(rep("W",181),rep("M",105))
lfat <- c(rep("Yes",35),rep("No",146),rep("Yes",8),rep("No",97))
Diet <- data.frame(Gender,lfat)
lFatTable <- table(Diet$Gender, Diet$lfat); lFatTable

Expected <- outer(rowSums(lFatTable), colSums(lFatTable))/sum(lFatTable);Expected
obsChisq <- sum((lFatTable-Expected)^2/Expected); obsChisq
# d) Conduct a permuatation test for independence, adapting the code on
# page 56
B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
   gender.permutation <-sample(Diet$Gender)
   perm.table <- table(gender.permutation, Diet$lfat)
   # result[i]<-chisq.test(perm.table)
   result[i] <- sum((perm.table-Expected)^2/Expected) 
 }
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic", xlim=c(0,30))
abline(v=obsChisq,col="blue",lty=5)
chMore<-which(result >= obsChisq)
pVal<-(length(chMore)+1)/(length(result)+1); pVal # one tailed


 