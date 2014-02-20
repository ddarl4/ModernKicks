#Math E-156 Script 4A-ChiSquare.R
#An exploration of sections 3.4.2 and 3.5 in the textbook

#Topic 1 -- when should we expecte a chi-square distribution?
#We have a cell in a contingency table where the expected number of observations is Exp.
#The observed number is Obs.
#A standard chi-squared test assumes that the sum of (Obs-Exp)^2/Exp has the chi-square distribution
#Here are some models for which this is a good approximation.

Exp = 156; N = 10^4; values <- numeric(N) #change this if you like, but Exp should be an even integer
#Case 1: Obs has a normal distribution whose mean and variance both equal Exp
#In this case the values we generate have the chi square distribution by definition
curve(dnorm(x,Exp,sqrt(Exp)), from = Exp/2, to = 3*Exp/2) #a continuous distribution
#Now generate lots of samples
for (i in 1:N) {
  values[i] <- (rnorm(1,Exp,sqrt(Exp))-Exp)^2/Exp
}
hist(values, xlim = c(0,10), breaks = "fd", freq = FALSE) #display probabilities
curve(dchisq(x,1), col = "red", add = TRUE)
mean(values); var(values) #should be close to 1 and 2

#Case 2: Obs has a Poisson distribution with parameter Exp, whose mean and variance both equal Exp
x <- 100:200
plot(x,dpois(x,Exp), ylim = c(0, 0.04),type = "h") #a discrete distribution
curve(dnorm(x,Exp,sqrt(Exp)), col = "red", add = TRUE) #well approximated by normal distribution
#Again generate lots of samples
for (i in 1:N) {
  values[i] <- (rpois(1,Exp)-Exp)^2/Exp
}
hist(values, xlim = c(0,10), breaks = 40, freq = FALSE) #display probabilities
curve(dchisq(x,1), col = "red", add = TRUE)
mean(values); var(values) #should be close to 1 and 2
#Try with a smaller value of Exp and the fit is less good

#Case 3: Obs has a binomial distribution with n = 100*Exp, p = .01.
#The mean is Exp and the variance is 0.99*Exp (not quite equal to the mean)
x <- 100:200
p <- .01     #small value makes the distribution nearly Poisson
plot(x,dbinom(x,Exp/p, p), ylim = c(0, 0.04),type = "h")
curve(dnorm(x,Exp,sqrt(Exp)), col = "red", add = TRUE) #normal approximation still pretty good
#Generate some samples
for (i in 1:N) {
  values[i] <- (rbinom(1,Exp/p,p)-Exp)^2/Exp
}
hist(values, xlim = c(0,10), breaks = 40, freq = FALSE) #display probabilities
curve(dchisq(x,1), col = "red", add = TRUE)
mean(values); var(values) #should be close to 1 and 2
#In this case Exp/p must be an integer.
#There is no way to make the variance precisely equal to the mean
#Try with a smaller value of Exp and the fit is less good
#Try with a larger value of p and the Poisson approximation is less good


#Case 4: Obs has a gamma distribution with r = Exp, rate = 1, whose mean and variance equal Exp
curve(dgamma(x,Exp,1), from = Exp/2, to = 3*Exp/2)
curve(dnorm(x,Exp,sqrt(Exp)), col = "red", add = TRUE)  #very similar density function
#Generate samples
for (i in 1:N) {
  values[i] <- (rgamma(1,Exp,1)-Exp)^2/Exp
}
hist(values, xlim = c(0,10), breaks = 40, freq = FALSE) #display probabilities
curve(dchisq(x,1), col = "red", add = TRUE)
mean(values); var(values) #should be close to 1 and 2

#Case 5: Obs has a negative binomial distribution with r = Exp/2, p = 0.5, whose mean and variance equal Exp
#Flip a fair coin and count how many flips you do until you get Exp/2 heads
#Careful -- the R function just counts the number of tails!
x <- 100:200    #the total number of flips, including the Exp/2 heads
#We are looking for x - Exp/2 tails (first argument) before we get Exp/2 heads (second argument)
plot(x, dnbinom(x-Exp/2,Exp/2, .5), ylim = c(0, 0.04),type = "h")
curve(dnorm(x,Exp,sqrt(Exp)), col = "red", add = TRUE)
for (i in 1:N) {
  values[i] <- (Exp/2+rnbinom(1,Exp/2,.5)-Exp)^2/Exp   #adding in the Exp/2 heads
}
hist(values, xlim = c(0,10), breaks =40, freq = FALSE) #display probabilities
curve(dchisq(x,1), col = "red", add = TRUE)
mean(values); var(values) #should be close to 1 and 2

#Topic 2 -- why do we sum over all the cells in the table?
#This was explored theoretically in the math notes for module 3.
#The table has just one row and two columns, but we sum over both cells.
Exp1 <- 100; Exp2 <- 200  #counts must sum to 300
Obs1 <- 120; Obs2 <- Exp1+Exp2 - Obs1
(Obs1-Exp1)^2/Exp1      #contribution from the first cell
(Obs2-Exp2)^2/Exp2      #contribution from the second cell
(Obs1-Exp1)^2/Exp1+(Obs2-Exp2)^2/Exp2   #sum is used for the standard chi-square test
#Keep track of sums for the individual cells
N <- 10^4; values1 <- numeric(N); values2 <- numeric(N);valuesSum <- numeric(N);values2Rescale <- numeric(N)
#To make the total constant, assume a binomial distribution with n = 300, p = 1/3
for (i in 1:N) {
  Obs1 = rbinom(1,Exp1+Exp2, 1/3)      #value in first cell
  Obs2 = Exp1+Exp2-Obs1                #calculated value in second cell
  values1[i] <- (Obs1 - Exp1)^2/Exp1   #contribution to chi square from first cell
  values2[i] <- (Obs2 - Exp2)^2/Exp2   #contribution to chi square from second cell
  valuesSum[i] <- (Obs1 - Exp1)^2/Exp1 + (Obs2 - Exp2)^2/Exp2  #contribution to chi square from second cell
  values2Rescale[i] <- (1+Exp2/Exp1)*(Obs2 - Exp2)^2/Exp2      #rescaled contribution from second cell
}
hist(values1, breaks = 30, freq = FALSE, xlim = c(0, 10))  #from first cell only
curve(dchisq(x,1), col = "red", add = TRUE) #same shape but but histogram is squeezed to the left
mean(values1); var(values1)  #both are too small
hist(values2, breaks = 30, freq = FALSE, xlim = c(0, 10))  #from second cell only
curve(dchisq(x,1), col = "red", add = TRUE) #same shape but but histogram is squeezed to the left
mean(values2); var(values2)  #both are much too small
hist(valuesSum, breaks = 30, freq = FALSE, xlim = c(0, 10)) #sum over both cells
curve(dchisq(x,1), col = "red", add = TRUE)  #much better fit
mean(valuesSum); var(valuesSum)  #right on target
hist(values2Rescale, breaks = 30, freq = FALSE, xlim = c(0, 10))
curve(dchisq(x,1), col = "red", add = TRUE) #exactly the same
mean(values2Rescale); var(values2Rescale)  #right on target

#Notice that we can get a chi square distribution from just one of the observations if we rescale.
#If we were just doing a permutation test using the chi square function, either cell would be OK.

#For a 3-way table we need to use a multinomial distribution, which R can handle
Exp1 <- 100; Exp2 <- 200; Exp3 <- 300 #three cells - counts must sum to 600
N <- 10^4; valuesSum <- numeric(N); valuesFirst2<- numeric(N)
#To make the total constant, assume a multinomial distribution with n = 600, p1 = 1/6, p2 = 2/6, p3 = 3/6
rmultinom(1, Exp1+Exp2+Exp3, prob = c(Exp1, Exp2, Exp3)) #probabilities get rescaled
for (i in 1:N) {
  Obs = rmultinom(1, Exp1+Exp2+Exp3, prob = c(Exp1, Exp2, Exp3)) 
  valuesSum[i] <- (Obs[1] - Exp1)^2/Exp1 + (Obs[2] - Exp2)^2/Exp2 + (Obs[3] - Exp3)^2/Exp3 #sum over 3 cells
  valuesFirst2[i] <- (Obs[1] - Exp1)^2/Exp1 + (Obs[2] - Exp2)^2/Exp2 #sum over 2 cells
}
#Now the result is chi square with two degrees of freedom
hist(valuesSum, breaks = 30, freq = FALSE, xlim = c(0, 10))
curve(dchisq(x,2), col = "red", add = TRUE)    #perfect fit
mean(valuesSum); var(valuesSum)  #should be 2 and 4
#If we use just the first two cells the scaling is wrong
hist(valuesFirst2, breaks = 30, freq = FALSE, xlim = c(0, 10))
curve(dchisq(x,2), col = "red", add = TRUE)    #right shape but histogram is squeezed to the left
mean(valuesFirst2); var(valuesFirst2)  #less than 2 and 4








