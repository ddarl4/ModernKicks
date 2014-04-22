#Math E-156 Script 10A-BayesDiscrete.R

#Topic 1 - Binomial data with a discrete prior distribution
#Example 10.1 from the text
#The parameter theta is your probability of winning a tennis match.
#In the "frequentist" apparoch, we would treat it as an unknown parameter.
#Now we assume that it is a random variable with 11 possible values.
theta <- seq(0, 1, by = .1) #ranges from 0.0 to 1.0
#The "Bayesian prior" specifies the probability of each value.
prior <- c(0, .02, .03, .05, .1, .15,.2, .25, .15, .05, 0); sum(prior) #must sum to 1
#A broken-line plot of the prior
plot(theta, prior, type = "b", ylim = c(0, 0.40), ylab = "Probability")
#These probabililities might be based on our experience against various opponents.

#We win one match, lose two, and we must update our probabilities for theta
#The likelihood is the probability of one win and two losses, given theta
likelihood <- theta*(1-theta)^2; likelihood   #this is a vector!

#To use Bayes's Theorem, we need the probability of one win and two losses.
#This will be much less than 1, since we are better than most opponents.
P1W2L<-sum(prior* likelihood); P1W2L

#Bayes says P(theta|1W2L) = P(theta)P(1W2L|theta)/P(1W2L)
#Probability of theta, conditioned on observing one win, two losses
posterior <-prior * likelihood/ P1W2L; posterior
sum(posterior) #normalization check - new distribution must sum to 1

#theta is a random variable, and its expectation has decreased
sum(theta*prior) #prior mean
sum(theta*posterior) #posterior mean

#Add the new "posterior" distribution to the plot
lines(theta, posterior, type="b", col = "red")

#Topic 2 - the posterior distribution does not depend on how we incorporate the data

#Suppose instead that we had played eight matches, won 3, lost 5.
#The likelihood is now the probability of three wins and five losses, given theta
likelihood2 <- theta^3*(1-theta)^5; likelihood
#Probability of three wins and five losses
P3W5L<-sum(prior* likelihood2); P3W5L #small because it's one of 9 outcomes
#Probability of theta, conditioned on observing three wins, five losses
posterior2 <-prior  * likelihood2/ P3W5L; posterior2
sum(posterior2) #normalization check
sum(theta*posterior2) #this posterior has an even lower expectation

#Add yet another posterior distribution to the plot
lines(theta, posterior2, type="b", col = "blue")
legend("topleft",legend = c("prior", "posterior1(1W2L)", "posterior2(3W5L)"), lty = 1, col = c("black", "red", "blue"))

#Alternative calculation - take the first posterior and add two wins, three losses
likelihood3 <- theta^2*(1-theta)^3
#Probability of two wins and three losses
P2W3L<-sum(posterior* likelihood3)
#Probability of theta, conditioned on observing two additional wins, three additional losses
posterior3 <-posterior * likelihood3/ P2W3L
sum(theta*posterior3) #expectation is same as before

#We should have recalculated the same posterior distribution
identical(posterior2, posterior3) #why does this fail?
posterior2- posterior3 # not quite zero because of computer arthmetic
identical(signif(posterior2), signif( posterior3)) #round to six digits


#Topic 3 - there is no need to make the prior mass function sum to 1
#Any scaling factor in the prior cancels out; so we only need relative probabilities.
#Use 100 times the original prior.
prior100 <- c(0, 2, 3, 5, 10, 15, 20, 25, 15, 5, 0); sum(prior) #sums to 100
likelihood <- theta*(1-theta)^2
#Probability of one win and two losses, times 100
P1W2LX100<-sum(prior100* likelihood)
#Probability of theta, conditioned on observing one win, two losses
posterior100 <-prior100  * likelihood/ P1W2LX100; posterior100
sum(posterior100) #this is now a properly normalized mass function
identical(signif(posterior), signif( posterior100))

