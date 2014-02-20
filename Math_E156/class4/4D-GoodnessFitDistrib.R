#Math E-156 Script 4D-GoodnessFitDistrib.R

#Topic 1 - hypothesized distribution has all parameters specified
#This is example 3.9 from the textbook
#Create a table with the observed data
TheData<-c(rep("0-0.25",30),rep("0.25-0.75",30),rep("0.75-1.25",22),rep("1.25",18))
Numbers=table(TheData); Numbers    #top line of table in middle of page 65

#Generate the expected counts, assuming the data are exponential
#The pexp() function gives the values of the integrals that the textbook calculates
Expected<-100*c(pexp(0.25),pexp(0.75)-pexp(0.25),pexp(1.25)-pexp(0.75),1-pexp(1.25)); Expected
#This is the bottom row of the table in middle of page 65
#Define a chi square function for comparing two vectors
chisquare <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
#Calculate chi square for the observed data
Chi2<-chisquare(Numbers,Expected);Chi2 
#This value is correct - typo in the textbook, using 28.6 instead of 18.6
#How probable is this large a value, given the chi-square distribution?
#Since there are only three independent value, use three degrees of freedom
Pvalue<- pchisq(Chi2,3,lower.tail = FALSE); Pvalue  #0.06 is correct- textbook is wrong

#As you will learn, the sum of 100 numbers from an exponemtial distribution has a gamma distribution.
#We saw that the gamma distribution leads to a chi-square distribution.

#Alternative approach - simulate drawing a lot of samples of 100 from the hypothesized distribution
N = 10^4-1; result = numeric(N)
for (i in 1:N){
  expData = rexp(100) #generate 100 random samples
  Counts=numeric(4)   #simulate top row of the table on page 65
  Counts[1] = sum(expData <= 0.25) 
  Counts[2] = sum((expData > 0.25) & (expData <= 0.75))
  Counts[3] = sum((expData > 0.75) & (expData <= 1.25))
  Counts[4] = sum(expData > 1.25)
  result[i] = chisquare(Counts, Expected)
}
hist(result, xlim = c(0,30), breaks = "FD",probability =TRUE)
curve(dchisq(x, df=3), col = "blue", add= TRUE)     #nearly perfect fit, as expected
abline(v = Chi2, col = "red")
Pvalue=(sum(result >= Chi2)+1)/(N+1); Pvalue   #again 0.059 -- this is how I found the typo in the textbook

#Topic 2 - hypothesized distribution has a parameter to be estimated from the data

#This is example 3.11 from the textbook
#Extract the observed data
Phillies= read.csv("Phillies2009.csv");head(Phillies)
Homeruns<-subset(Phillies, select =HomeRuns, drop = TRUE)   #a vector with home runs for each game
lambda <- mean(Homeruns); lambda     #Poisson parameter is equal to the expectation -- use the sample mean
HomeTable<-table(Homeruns); HomeTable
#Should not do chi square with tiny counts, so combine all games with 4 or more home runs
#Make the vector have only 5 elements
HomeTable[5]<-sum(HomeTable[5:6]);HomeTable <- HomeTable[-6]; HomeTable #Table 3.10 (top row) in the textbook

#Generate the expected counts for a 162-game season, assuming the data are Poisson with the calculated lambda = 1.3827
Expected<-162*dpois(0:3, lambda);Expected[5]<-162*(1-ppois(3, lambda));Expected #Table 3.10 (bottom row)

Chi2<-chisquare(HomeTable,Expected);Chi2 #value 0.084 agrees with the textbook
#How probable is this large a value, given the chi-square distribution?
#Because we estimated one parameter from the data, there are only three degrees of freedom
Pvalue<- pchisq(Chi2,3,lower.tail = FALSE); Pvalue  #value 0.84 agrees with the textbook
#Conclusion -- the observed data are consistent with a Poisson distribution

#Let's simulate a lot of samples from the hypothesized distribution
#We simulate 10000 seasons of 162 games each
N = 10^4-1; result <- numeric(N)
for (i in 1:N){
  expData = rpois(162,lambda) #generate 162 random samples
  Counts<-numeric(5)
  Counts[1] <- sum(expData ==0) 
  Counts[2] <- sum(expData ==1)
  Counts[3] <- sum(expData ==2)
  Counts[4] <- sum(expData ==3)
  Counts[5] <- sum(expData >=4)   #4 or more home runs
  result[i] = chisquare(Counts, Expected)
}
hist(result, probability =TRUE, 20)
curve(dchisq(x, df=3), col = "blue", add= TRUE)    #not a great fit
#The problem is that the expected number of home runs per game is so small
abline(v = Chi2, col = "red")
Pvalue=(sum(result >= Chi2)+1)/(N+1); Pvalue   #value of 0.93 is different from built-in chi square test
#In this case the result of the simulation is unassailable, while the built-in chi square test is suspect.


