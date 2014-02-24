### part 1

# Exercise 15 on page 72:
# For the Flight Delays Case Study in Section 1.1, conduct a test of 
# ho- mogeneity to determine if there is a relationship between carrier 
# and the number of flights delayed more than 30 minutes (Delayed30). 
# The answer is on page 401.
fd <- read.csv("FlightDelays.csv")
fdTbl <- table(fd$Carrier, fd$Delayed30)
chisq.test(fdTbl)
Expected <- outer(rowSums(fdTbl), colSums(fdTbl))/sum(fdTbl);Expected
obsChisq <- sum((fdTbl-Expected)^2/Expected) 

B <- 10^4-1
result<-numeric(B)
for (i in 1:B)
 {
   carrier.permutation <-sample(fd$Carrier)
   perm.table <- table(carrier.permutation, fd$Delayed30)
   result[i] <- sum((perm.table-Expected)^2/Expected) 
 }
#Create a histogram
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic")
abline(v=obsChisq,col="blue",lty=5)
chMore<-which(result >= obsChisq)
pVal<-(length(chMore)+1)/(length(result)+1); pVal # one tailed



### Part 2
# According to Stirzaker, Elementary Probability:
# “During 1979, in Bristol, 1103 postmen sustained 215 dog bites. 
# A total of 191 postmen were bitten, of whom 145 were bitten just once.”

# Let random variable X be the number of dog bites sustained by 
# an in- dividual postman. If a dog, having decided to bite, chooses 
# its victim at random, X will have a binomial distribution that is 
# very well approximated by a Poisson distribution with λ = 215/1103.
#  Carry out a goodness-of-fit test for this model.

# test statistic: number of postmen bitten 2 or more times
obsNmulti <- 191-145 # number of multi-bit postmen
obsNbitten <- 191

B <- 10^4-1
bittenResult<-numeric(B)
multiResult<-numeric(B)
for (j in 1:B)
 {
    # pVec <- rpois(11000,315/1103) # take a long array of postmen
    pVec <- rpois(1103,315/1103) # take a long array of postmen
    bittenVec <- pVec[pVec > 0] #[1:1103] # take counts of the first 1103 postmen
    nBitten <- length(bittenVec)
    nMulti <- sum(bittenVec > 1)
    bittenResult[j] <- nBitten
    multiResult[j] <- nMulti
 }
### bite count test
hist(bittenResult, xlab="number postmen with at leaste one bite", xlim = c(obsNbitten, max(bittenResult)))
abline(v=obsNbitten,col="blue",lty=5)
p1<-(length(which(bittenResult >= obsNmulti))+1)/(length(bittenResult)+1)
p2<-(length(which(bittenResult <= obsNmulti))+1)/(length(bittenResult)+1)
pVal <- min(p1,p2)*2; pVal
### multi-bite test
hist(multiResult, xlab="number of postment with multiple bites") #, xlim = c(obsNmulti, max(result)))
abline(v=obsNmulti,col="blue",lty=5)
p1<-(length(which(multiResult >= obsNmulti))+1)/(length(multiResult)+1)
p2<-(length(which(multiResult <= obsNmulti))+1)/(length(multiResult)+1)
pVal <- min(p1,p2)*2; pVal

