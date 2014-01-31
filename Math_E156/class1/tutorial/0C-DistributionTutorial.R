#Math E-156 Script 0C-DistributionTutorial.R
#This script accompanies the "Probability Distributions in R" Tutorial from the textbook's Web site
#It will save you the trouble of typing the individual commands

#Probability
pnorm(1.25)
pnorm(2.8, 2, 3)
pchisq(13.9, 25)
1 - pexp(4, 1)     #changed 10 to 1 to get a larger value
pexp(4, 1, lower.tail=FALSE)   #same change
pt(3.9, 7) # pt(t-value, d.f), important but probably unfamiliar to you

#Quantiles
qnorm(.25)
qnorm(.75, 2,3)
qt(.975, 13)

#Random numbers
rnorm(100)
x <- rnorm(100)
hist(x)
hist(x, breaks = "FD")   #creates smaller bins
rchisq(10, 23)
#Graph of density function
curve(dnorm(x), from = -3, to = 3)
w <- rnorm(50) # random sample from N(0,1)
hist(w, probability = TRUE) # scale to area 1
curve(dnorm(x), add = TRUE) # impose normal density
hist(w, probability = TRUE, ylim = c(0, .5)) # widen y-axis range
curve(dnorm(x), add=TRUE)
curve(dchisq(x, 14), from = 0, to = 20)

#Binomial distribution
dbinom(5,16, .2)   #changed .8 to .2 to get fewer heads
choose(16,5)*.2^5*.8^11
pbinom(5,16,.2)
dbinom(0,16,.2)+dbinom(1,16,.2)+dbinom(2,16,.2)+dbinom(3,16,.2)+dbinom(4,16,.2)+dbinom(5,16,.2)

#Go back to .8 for doing quantiles
qbinom(.25,16,.8)
pbinom(11, 16, .8)
pbinom(12, 16, .8)

#Geometric -- careful: the first argument is the number of failures!
dgeom(3,.8)
pgeom(3,.8)

#Poisson -- important, although perhaps still unfamiliar
dpois(3,5)
5^3*exp(-5)/(3*2)

#Random numbers
rbinom(1,25,.8)
set.seed(0)
heads <- rbinom(10, 25, .8); heads
table(heads)
barplot(table(heads))   #this is not a histogram!
heads2 <- rbinom(100, 25, .8)
barplot(table(heads2)) 
hist(heads2)      #not quite the same
hist(heads2, breaks = seq(14.5, 23.5)) #center the bins on the integers
par(mfrow = c(2, 1)) #2x1 layout
barplot(table(heads2))
hist(heads2, breaks = seq(14.5, 23.5)) #identical with the barplot



