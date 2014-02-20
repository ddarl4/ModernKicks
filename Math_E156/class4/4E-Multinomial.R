#Math E-156 Script 4E-Multinomial.R
#This topic is covered in Appendix B.2 of the textbook

#Topic 1 - installing and using the multinomial test
#The multinomial functions are not in the default R installation
#Load the EMT package:
#Does an exact multinomial test -- improvement on chi-square
#install.packages("EMT") #comment this out after the first time
library(EMT)   #if this fails, uncomment the preceding line!

#Here is how to use the multinomial test
observed <- c(5,2,1) # observed data: 5 items in category one, 2 items in category two, 1 item in category three
#There are 10 choose 2 = 45 ways to divide the 8 items among the three categories
prob <- c(0.25, 0.5, 0.25) # model: hypothetical probability that an item falls into category one, two, or three
# Calculate p-value using default options:
out <- multinomial.test(observed, prob)
#The P-value is the probability of getting an "event" that is at least as unlikely as the observed one
#p.value = 0.0767
#Plot the probabilities for each event:
plotMultinom(out)
#Calculate p-value for the same input using Pearson’s chisquare as a distance measure:
out <- multinomial.test(observed, prob, useChisq = TRUE)
#p.value = 0.0596 ; not the same!

# Test the hypothesis that all sides of a die appear with the same probability (a 6-dimensional problem):
pdice = 1/6
prob6 <- c(pdice, pdice, pdice, pdice, pdice, pdice) # the model, determined by the hypothetical probabilities
observed <- c(4, 5, 2, 7, 0, 1) # an observation consisting of 19 throws ( = sample size)
out <- multinomial.test(observed, prob6)
#p.value = 0.0357 ; better get another die; this one seems to be biased
#In this case there are over 40000 events, many very unlikely
plotMultinom(out, showmax = 10000)

#Topic 2 -- using the multinomial test on examples from the textbook

#Do the birth season example from page 64 of the text
observed <- c(150, 138, 140, 100)
prob <- c(.25, .25, .25, .25)
#out <- multinomial.test(observed, prob)   #takes forever!
out <- multinomial.test(observed, prob, useChisq = TRUE, MonteCarlo = TRUE, ntrial = 500000)
#The P-value agees with the one from the contingency table

#Repeat with much less data
observed <- c(8, 4, 3, 1)   #counts are perhaps too small to trust chi square distribution
chisq.test(observed)        #pvalue is 0.0896
out <- multinomial.test(observed, prob)  #pvalue 0.1154 is probably more accurate
plotMultinom(out, showmax = 1000)

#Do Example B.2 (M&Ms) from the textbook
mmprob <- c(.24,.20,.16,.14,.13, .13)
colors <- c("blue","orange","green","yellow","red", "brown")
observed <- c(8,5,6,5,4,2)
rbind(colors, mmprob,observed)   #data from example B.2
dmultinom(observed, prob = mmprob)    #agrees with the book's solution for example B.2
out <- multinomial.test(observed, mmprob)  #not an especially unlikely distribution of colors
plotMultinom(out, showmax = 1000)
chisq.test(observed)  #gives quite a different P-value




