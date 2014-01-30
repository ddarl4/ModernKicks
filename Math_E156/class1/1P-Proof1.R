#Math E-156, script 1P-Proof1.R
#Background for proof 1.
#Load Dice3.csv, created by script 1A
Dice3 <- read.csv("Dice3.csv", row.names = 1) # do not read row numbers
nrow(Dice3); head(Dice3)
#This is a "probability data frame" that lists the 216 possible outcomes of rolling three dice

#We can create some random variables
sum3 <- Dice3$Red3 + Dice3$Green3+ Dice3$White3; head(sum3)   #total roll
max2 <- apply(Dice3[,1:2], 1, max); head(max2) #larger of numbers on first two dice
range <- apply(Dice3, 1, max)-apply(Dice3, 1, min)   #largest minus smallest
#Add these to the data frame with cbind() "column bind"
Frame <- cbind(Dice3, sum3, max2, range); head(Frame)

#Since we are doing probability, we need a function to calculate population variance
popvar <- function(x) mean((x - mean(x))^2)  #this is how you define an R function
popvar(Dice3$Red3)
#Alternative calculation that can be done in one pass through the data
mean(Dice3$Red3^2) - mean(Dice3$Red3)^2   #prove that thiscomes out the same!

#The built-in function var() treats our data as a sample from a larger population
var(Dice3$Red3); (215/216)*var(Dice3$Red3) #can be corrected

#Look at the expectation of the product to test for dependence
mean(range*max2); mean(range)*mean(max2)  #dependent
mean(sum3*max2); mean(sum3)*mean(max2)    #dependent
mean(Dice3$White3*max2); mean(Dice3$White3)*mean(max2) #independent
#The last line does not prove that Dice3$White3 and max2 are independent.
#It only proves that they are uncorrelated.
cor(Dice3$White3,max2)     #see page 251 of the textbook

#For expectation of a sum, independence is not a requirement
mean(range+max2); mean(range) + mean(max2) #should be equal

#For variance of a sum, independence is a requirement
popvar(range+max2); popvar(range) + popvar(max2) #not independent
popvar(Dice3$White3+max2); popvar(Dice3$White3) + popvar(max2) #independent

#When you add n independent random variables, their variances add
popvar(sum3); 3*popvar(Dice3$Red3)

#When you multiply a random variable by a, you multiply its variance by a^2
popvar(3*Dice3$Red3 + 5); 9*popvar(Dice3$Red3)


