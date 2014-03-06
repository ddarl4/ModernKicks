#Math E-156 Script 6D-BootstrapCardio.R

#Topic 1 -- doing a bootstrap when we have only a couple of proportions to work with
#Reconstruction of textbook example 5.7 on page 123
#55 men out of 3338 with high blood pressure died; 3283 lived
#21 men out of 2676 with low blood pressure died; 2655 lived

#Make two vectors and combine them into a data frame
BP <- c(rep("High", 3338),rep("Low",2676))
Outcome<- c(rep("Die",55),rep("Live",3283),rep("Die",21),rep("Live",2655))
Cardio<-data.frame(BP, Outcome);head(Cardio)
table(Cardio$BP, Cardio$Outcome)
write.csv(Cardio,"Cardio.csv")    #save for possible future use

#Although large, this is a sample, not a population
#Calculate the relative risk from the raw data
n1<-sum(BP == "High"); n2 <-sum(BP == "Low")
#Compute the propotion of deaths in the high BP group
riskHigh <- sum((BP == "High") & (Outcome == "Die"))/n1; riskHigh

#Compute the propotion of deaths in the low BP group

riskLow <- sum((BP == "Low") & (Outcome == "Die"))/n2; riskLow
RelRisk <- riskHigh/riskLow; RelRisk    #number in book is different due to roundoff

#To bootstrap this statistic - first split the data
highBP <- subset(Cardio, select = Outcome, subset = (BP == "High"), drop = TRUE)
lowBP <- subset(Cardio, select = Outcome, subset = (BP == "Low"), drop = TRUE)

#No harm in checking that our resampling works as intended
sample1 <-sample(highBP,n1, replace = TRUE); head(sample1)
sample2 <-sample(lowBP,n2, replace = TRUE)
risk1 <- mean(sample1 == "Die"); risk1
risk2 <- mean(sample2 == "Die"); risk2
risk1/risk2; ratio for bootstrapping

#This analysis could also be done using rbinom()
#Now do the bootstrapping.
N <- 10^4; ratio <- numeric(N); prop1 <-numeric(N); prop2 <- numeric(N)
for (i in 1:N) {
  sample1 <-sample(highBP,n1, replace = TRUE)
  sample2 <-sample(lowBP,n2, replace = TRUE)
  prop1[i] <- mean(sample1 == "Die")
  prop2[i] <- mean(sample2 == "Die")
  ratio[i] <-prop1[i]/prop2[i]
}
hist(ratio, xlab = "Relative Risk")  #Figure 5.16
abline(v = mean(ratio), col = "red")
abline(v = RelRisk, col = "blue")
bias <- mean(ratio) - RelRisk; bias  #close to the number on page 123
stderr <- sd(ratio); stderr

#Calculate the bootstrap confidence interval
CI = quantile(ratio, c(0.025, 0.975)); CI
abline(v = CI[1], col = "green")
abline(v = CI[2], col = "green")

#95% of the bootstrap samples fall between the green vertical lines
#A confidence interval based on a normal approximation would be in error
CINorm = qnorm(c(0.025, 0.975), mean(ratio), sd(ratio)); CINorm
abline(v = CINorm[1], col = "magenta")
abline(v = CINorm[2], col = "magenta")

#A scatter plot illustrates what is going on -- replicate Figure 5.17
plot(prop2, prop1, xlim=c(0,0.02), ylim=c(0,0.03), xlab = "Low BP", ylab = "High BP")
abline(h = mean(prop1), col = "red")
abline(v = mean(prop2), col = "red")
#These red lines intersect at the relative risk
abline(0, mean(ratio), col = "blue")   #slope is the bootstrap mean
abline(0, CI[1], col = "black") #2.5% below this line
abline(0, CI[2], col = "black") #2.5% above this line

 

  
  