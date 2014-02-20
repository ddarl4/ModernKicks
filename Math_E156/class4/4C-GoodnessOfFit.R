#Math E-156 Script 4C-GoodnessFit.R

#Topic 1 - comparing data with a probability model

#Create a vector containing the data from Example 3.8 in the textbook:
Births<-c(rep("Aug-Oct",150),rep("Nov-Jan",138),rep("Feb-April",140),rep("May-July",100))
Obs<-table(Births);Obs
Expected <- rep(sum(Obs)/length(Obs),length(Obs)); Expected
#Now write a new function to compute chi square for the single row
chisq1 <-function(Obs){
  Expected <- rep(sum(Obs)/length(Obs),length(Obs))
  sum((Obs-Expected)^2/Expected)
}
observed <-chisq1(Obs);observed #value 10.97 agrees with the textbook
Pvalue <-chisq.test(Obs); Pvalue #P-value of .012 suggests that distribution is not uniform

#We can make a probability vector with just four elements
months=c("Aug-Oct","Nov-Jan","Feb-April","May-July"); months
#NOw sample from this vector to get the right total number of observations
Birth.sim<-sample(months,sum(Obs), replace= TRUE); head(Birth.sim)
table(Birth.sim)   #this set of counts could have arisen by chance
chisq1(table(Birth.sim)) #random, but probably much smaller than for the observed data
#Now we just do a lot of simulations:
N =10^4 -1; result<-numeric(N)
for (i in 1:N){
  Birth.sim<-sample(months,sum(Obs), replace= TRUE)
  result[i]<-chisq1(table(Birth.sim))
}
hist(result)
abline(v = observed, col = "red") 
Pvalue <- (sum(result >= observed)+1)/(N+1); Pvalue   #P-value will be close to .012
#The Pvalue from our simulation agrees with the chi square test because our samples do:
hist(result, probability = TRUE)
curve(dchisq(x, df=3), col = "red", add= TRUE)
