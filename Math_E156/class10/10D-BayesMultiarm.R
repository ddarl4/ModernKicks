#Math E-156 Script 10D-BayesMultiarm.R

#Topic 1 - trying to identify the best version of a Web site

#Here is the number of visits for six different versions of the site
n<-c(1874, 1867, 1871, 1868, 1875, 1875) # visits to 6 sites
X<-c(52,41,55,49,39,39) #purchases from those sites

#For each site, we can get parameters for a probability of purchase
#We are using a "noninformative" prior where alpha and beta are tiny and equal

alpha<- X # parameters for posterior beta distributions
beta<- n-X # parameters for posterior beta distributions

#Now we simulate 10000 random selections of the parameter values
N <- 10^5  #replications
theta<- matrix(0,0, nrow = N, ncol = 6) #10000 rows of 6 zeroes
for (j in 1:6) {
  theta[ ,j] <- rbeta(N, alpha[j], beta[j]) #fill in column j
}
head(theta)
#Each row contains a probability of purchase for each version

#Now, for each row, find which site has the highest probability of purchase
probBest <- numeric(6) #vector for results
best <- apply(theta, 1, max) # 1 means apply max over rows
for (j in 1:6) {
  probBest[j] = mean(theta[ ,j] == best)
}
probBest #versions 2, 5, and 6 rarely produce the best results

#Version 1 and version 3 are the usual winners
#Plot column 1 against column 3
plot(theta[1:10^4,1], theta[1:10^4,3], pch = ".")
abline(0,1, col = "red") #on this line, 1 and 3 are tied
text(.037, .042, substitute(theta[3] > theta[1]))
text(.042, .037, substitute(theta[1] > theta[3])) #typo in book

#Do the same thing with 2 and 3
plot(theta[1:10^4,2], theta[1:10^4,3], pch = ".")
abline(0,1, col = "red")
text(.034, .042, substitute(theta[3] > theta[2]))
text(.034, .030, substitute(theta[2] > theta[3]))
#This graph makes a strong case for abandoning site 2

#Topic 2 - getting the same results using a binomial simulation

#Here is the number of visits for six different versions of the site
n<-c(1874, 1867, 1871, 1868, 1875, 1875) # visits to 6 sites
X<-c(52,41,55,49,39,39) #purchases from those sites
p<-X/n ;proportion of purchase from each site

#Now we simulate 10000 cases of 1900 visits to each site
N <- 10^5  #replications
sales<- matrix(0,0, nrow = N, ncol = 6) #10000 rows of 6 zeroes
for (j in 1:6) {
  sales[ ,j] <- rbinom(N, 1900, p[j]) #fill in column j
}
head(sales)
#Each row contains the number of purchases for each version

#Now, for each row, find which site has the highest number of purchases
numTop <- numeric(6) #vector for results
best <- apply(sales, 1, max) # 1 means apply max over rows
#In this case, there may be a tie for the most purchases
for (j in 1:6) {
  numTop[j] = mean(sales[ ,j] == best) 
}
numTop; sum(numTop)  #sum exceeds 1 because of ties
probTop<-numTop/sum(numTop); round(probTop,5)
probBest   #for comparison
#The Bayesian approach gives the same result as brute-force simulation


