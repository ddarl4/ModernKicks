#Math E-156 Script 12C-TypeITypeIIErrors.R

#Topic 1 -- Type I and Type II errors

#A Type I error occurs when the null hypothesis is true but we erroneously reject it.
#The probability of this event is the P value.
#Most of the examples in section 8.2.1 are already familiar.
#Here is example 8.12

#The null hypothesis is that the pdf is 3*x^2 on [0,1].
#The test is to take 8 samples and reject the null hypothesis if 5 or more exceed 0.88.
#Here is the probability that a single will exceed 0.88.
integrate(function(x) 3*x^2, lower = 0.88, upper = 1)
#In fact this is a special case of the beta distribution
p <-pbeta(0.88,  3,  1, lower.tail = FALSE); p

#The binomial distribution reveals the probability that more than 4 samples exceed 0.88.
pbinom(4, 8, p, lower.tail = FALSE)
#So the probability of rejecting results that arise from the null hypothesis is .0736

#To lower this probability, change the test to require 10 or more of 16 samples
p <-pbeta(0.88,  3,  1, lower.tail = FALSE); p
pbinom(9, 16, p, lower.tail = FALSE)
#Now the probability of rejecting results that arise from the null hypothesis is .0113

#A Type II error requires an explicit alternative, e.g. the pdf is 7*x^6.
#Here is the probability that a single sample will exceed 0.88.
p <-pbeta(0.88,  7,  1, lower.tail = FALSE); p   

#If we get more than 4 samples that exceed 0.88, we correctly result the null hypothesis.
pbinom(4, 8, p, lower.tail = FALSE) #57% chance to reject the null hypothesis
#If we fail to reject the null hypothesis, we make a Type II error.
#This wiil happen 43% of the time!

#Again, we could change the test to require 10 or more of 16 samples
pbinom(9, 16, p, lower.tail = FALSE) #still only a 50% chance to reject the null hypothesis

#Topic 2 -- changing the test to make fewer Type II errors but more Type I errors

#We could change the test to require only 8 or more of 16 samples
p <-pbeta(0.88,  3,  1, lower.tail = FALSE); p
pbinom(7, 16, p, lower.tail = FALSE) #10% chance of Type I error

#Suppose that the alternative hypothesis is true
p <-pbeta(0.88,  7,  1, lower.tail = FALSE); p   #probability of exceeding 0.88
pbinom(7, 16, p, lower.tail = FALSE) #now an 84% chance to reject the null hypothesis
#We have reduced the probability of a type II error to 16%

#With a large sample size we can have a small probability of both types of error
#Change the test to require 15 or more of 32 samples
p <-pbeta(0.88,  3,  1, lower.tail = FALSE); p
pbinom(14, 32, p, lower.tail = FALSE) #5% chance of Type I error

#Consider the alternative hypothesis.
p <-pbeta(0.88,  7,  1, lower.tail = FALSE); p   #probability of exceeding 0.88
pbinom(14, 32, p, lower.tail = FALSE) #now a 94% chance to reject the null hypothesis
#So the probability of a type II error is only 6%

#The probability of correctly rejecting the null hypothesis when the alternative hypothesis is correct
#is called the power of the test.

#Topic 3 -- Likelihood ratio test for a simple hypothesis.

#In the preceding example, both the null hypothesis and the alternative are simple.
#This means that either hypothesis completely specifies the distribution of the population.
#An alternative like "the distribution is (a+1)x^a with a > 6" is not simple.

#For a simple hypothesis, compute the likelihood ratio T(x) for null vs. alternative.
#Reject the null hypothesis if T(x) is less than some value c (sets the type I error rate)

#What is the best that we could do with just a single sample X?
#The likelihood ratio is T(x) = 3*x^2/7*x^6 
#Choose c = 1 and set T(y) = c.
y <- (3/7)^0.25; y; 3*y^2/(7*y^6) 

#Reject the null hypothesis if X > y so that T(x) <1.
pbeta(y,  3,  1, lower.tail = FALSE) #47% probability of Type 1 error
pbeta(y,  7,  1, lower.tail = FALSE) #77% probability of rejecting the null hypothesis (23% Type II)

#The Neyman-Pearson Lemma says that this approach minimizes the Type II error.

