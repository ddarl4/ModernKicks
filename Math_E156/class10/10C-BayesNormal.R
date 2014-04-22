#Math E-156 Script 10C-BayesNormal.R

#Topic 1 - if the samples have a normal distribution, use a normal prior
#The prior distribution is normal: N(mu0, sigsq0)
#The n observed samples are normal with unknown mu but known sigsq
#A messy calculation on pp. 316-317 shows a posterior distribution N(mu1, sigsq1)
#Here is example 10.4 from the textbook

#The prior distribution is N(50, 6^2)
curve(dnorm(x,50,6), xlim = c(30,70),ylim = c(0,0.2))
#An ichthyologist observes a sample of 15 fish
#The mean length is 45 cm, with a variance assumed to be 8

#The formula on page 317 (skip the derivation!) gives the posterior variance
A<- 15/8^2 # 15 samples with variance assumed to be 8
A0<- 1/6^2 # inverse variance for prior
A1<- A0 + A # inverse variance for posterior
sqrt(1/A1)  #the standard deviation of the mean (posterior)

#Calculation of the posterior mean
M<- 45 # mean length for sample of 15 fish
M0 <- 50 #mean for prior
M1 <- (A0*M0 + A*M)/(A0 + A) #mean for posterior

curve(dnorm(x, M1, 1/sqrt(A1)), ,col = "red", add = TRUE) #posterior
#For comparison, here is the distribution of the mean with mu = 45, sd = 8
curve(dnorm(x, M1, 8/sqrt(15)), ,col = "blue", add = TRUE)

#Topic 2 - incorporating the data sequentially leads to the same posterior
#To illustrate the "sequential" idea of Example 10.5, do 5 fish at a time
#Catch large fish first, but the sample of 15 will have a mean of 45

#Start by plotting the prior distribution for the mean length statewide
curve(dnorm(x,50,6), xlim = c(30,70),ylim = c(0,0.2))  
A0<- 1/6^2 # inverse variance for prior
M0 <- 50 #mean for prior
A<- 5/8^2 # 5 samples with variance assumed to be 8
M<- 48 # mean length for first sample of 5 fish
M1 <- (A0*M0 + A*M)/(A0 + A) #mean for first posterior
A1<- A0 + A # inverse variance for first posterior

#Plot the updated estimate for the distribution of the mean length after 5 fish
curve(dnorm(x, M1, 1/sqrt(A1)),col = "green", add = TRUE) #first posterior
A<- 5/8^2 # 5 more samples with variance assumed to be 8
M<- 45 # mean length for second sample of 5 fish
M2 <- (A1*M1 + A*M)/(A1 + A) #mean for second posterior
A2<- A1 + A # inverse variance for second posterior

#Plot the updated estimate for the distribution of the mean length after 10 fish
curve(dnorm(x, M2, 1/sqrt(A2)),col = "blue", add = TRUE) #second posterior
A<- 5/8^2 # 5 more samples with variance assumed to be 8
M<- 42 # mean length for three sample of 5 fish -- overall mean now 45
M3 <- (A2*M2 + A*M)/(A2 + A) #mean for third posterior
A3<- A2 + A # inverse variance for third posterior

#Estimate for the distribution of the mean length after 15 fish is same as before
curve(dnorm(x, M3, 1/sqrt(A3)),col = "magenta", add = TRUE) #third posterior
#As more data are added, the mean shifts and the variance decreases

