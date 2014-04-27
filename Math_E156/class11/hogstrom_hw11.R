# Larson Hogstrom - HW11
# MATH_E156 - 4/25/2014


### part 1 ###
# Exercise 12 on page 202. 
# Girls2004
Girls2004 <- read.csv("Girls2004.csv"); Girls2004
# a) create exploratory plots and compare the dist. of weights between babies
# born to nonsmokers and babies born to smokers

is_smoke <- Girls2004$Smoker == 'Yes'
not_smoke <- Girls2004$Smoker == 'No'
smkWeight <- Girls2004[is_smoke,]$Weight; smkWeight
nsmkWeight <- Girls2004[not_smoke,]$Weight; nsmkWeight

hist(smkWeight)
hist(nsmkWeight)
boxplot(Weight ~ Smoker, data=Girls2004)

# b) Find a 95% one-sided lower t confidence bound for the mean difference
# in weights between babies born to nonsmokers and smokers. 

M1<-mean(smkWeight); M2<-mean(nsmkWeight); M1; M2 #is this difference significant?
n1 <- length(smkWeight); n2 <- length(nsmkWeight); n1; n2 #not the same size
S1<- sd(smkWeight); S2<- sd(nsmkWeight); S1; S2
SE <- sqrt(S1^2/n1 + S2^2/n2); SE
ndf <- (S1^2/n1 + S2^2/n2)^2/(S1^4/(n1^2*(n1-1)) + S2^4/(n2^2*(n2-1))); ndf
tqnt<- qt(.05,ndf); tqnt
interval<- (M2-M1)+ SE*tqnt; interval

#use built-in test
t.test(nsmkWeight,smkWeight,alternative="greater")

print("If we hypothesize that babies from smokers will only be lighter, than
    we can reject the null hypothesis that smoker and nonsmokers are the same")

# Also find a 95% t confidence interval, as in exercise 11 (the first section 
# problem).

t.test(smkWeight, nsmkWeight)

# equations using T dist
M1<-mean(smkWeight); M2<-mean(nsmkWeight); M1; M2 #is this difference significant?
n1 <- length(smkWeight); n2 <- length(nsmkWeight); n1; n2 #not the same size
S1<- sd(smkWeight); S2<- sd(nsmkWeight); S1; S2
SE <- sqrt(S1^2/n1 + S2^2/n2); SE
ndf <- (S1^2/n1 + S2^2/n2)^2/(S1^4/(n1^2*(n1-1)) + S2^4/(n2^2*(n2-1))); ndf
tqnt<- qt(c(.025, .975),ndf); tqnt
interval<- (M1-M2)+ SE*tqnt; interval #large, but does not include zero!
curve(dt(x, df  = ndf), from = -5, to =5)
abline(v = (M1-M2)/SE, col = "red" )
pval <-pt((M2-M1)/SE, df = ndf, lower.tail = FALSE); pval
#For a two-sided test we ought to double this
2*pval    #compare with the automated test

print("If we test for both heavier and lighter babies from smokers, then
    the interval includes 0 so we can't conclude that girls from smokers have a 
    different weight than those from non-smokers at 95% confidence")

### Part 2 ###
# Exercise 20 on page 205. After solving the problem, make a dataframe to 
# simulate the survey results. Do a permutation test to see whether there 
# is gender difference in voter preference, and construct bootstrap t 
# confidence intervals to compare with your answers to (a), (b), and (c).
# This problem is very similar to the second section problem!

# General Social survey
# 459 of 980 women voted for Bush in 2000
# 426 of 759 men voted for Bush

# a) find a 95% confidence interval for the proportion of wome who voted 
# for Bush
prop.test(459,980,conf.level=.95,correct=FALSE)$conf

# b) Repeate for Men. Can you conclude anyting about gender differneces 
# in voter preference?
prop.test(426,759,conf.level=.95,correct=FALSE)$conf
print("the confidence intervals for the proportion of men vs. women Bush votes
    do not overlap. The book cautions however, 'Looking for overlap in individual 
    confidence intervals is not a good way to test for significan differences'.")

# c) compute a 95% confidence interval for the difference in proportion of
# Bush votes from men vs women
prop.test(c(459,426),c(980,759),correct=FALSE)
print("testing the proportion of Bush votes from men vs. women dirrectly, suggests that
Bush saw a lower proportion of votes from women.")

# ### Part 3 ###
# 3. Exercise 36 on page 208. This problem will encourage you to read 
# section 7.5 carefully!

# derive "bootstrap Z" interval
# a) Following the steps in the derivation of the bootsrap t interval in 
# section 7.5, derive a bootstrap Z inverval for u, for cases when var is knwon



# b) Calculate this interval for the Verison CLEC data; for var, use the sample
# variance of the Verizon ILEC data. 

# c) Compare the interval with a formula z interval. How does the bootsrap Z 
# interval adjust for skewness

# ### Part 4 ###
# 4. (a) Exercise 38 on page 208. You will need the result of exercise 37,
#  which in turn relies on familiar theorem B.16. In this case (n − 1)S2/σ2 
#  is a pivotal statistic because its chi-square distribution does not 
#  depend either on σ or on the unknown parameter μ.

# eight different cerial boxes
weights <- c(560, 568, 580, 550, 581, 581, 562, 550)

# Assuming the data are from a normal dstribution, find a 90% confidence 
# interval for the variance, theta^2. (use qchisq)

# interval for variance:
# ((n-1)S^2/q2,(n-1)S^2/q1)
n <- length(weights)
S <- sd(weights)
csqnt <- qchisq(c(.05, .95),(n-1)); csqnt
int.var <-  (n-1)*S^2/csqnt; int.var

# (b) Do a simulation where the weights of the eight cereal boxes are drawn 
# from N(560,102), and check that, when you calculate the confidence interval
#  as in part (a), the true variance of 100 falls within the confi- dence 
#  interval in roughly 95% of the cases.
N<- 10^4
counter <- 0;
for(i in 1:N) {
    x <- rnorm(8,560,sqrt(102))
    S <- sd(x)
    L_csqnt <- qchisq(.95,(n-1))
    U_csqnt <- qchisq(.05,(n-1))
    L <- (n-1)*S^2/L_csqnt
    U <- (n-1)*S^2/U_csqnt
    if (L < 102 && U > 102) counter <- counter + 1 #count +1 if we were correct
}
counter/N
print("the simulation shows that the true variance is caputred with in the 
    interval aproximatly 90% of the time. This is the what we would expect.")

# ### Part 5 ###
# Suppose that you draw two samples from N(μ,σ) with μ = 5,σ = 2. In
# this case the quantity T =√2*(X_bar -u)/S is a pivotal statistic, since its
# distribution (Student t with one degree of freedom, a.k.a. Cauchy) 
# does not depend on the unknown parameter σ. You can therefore calculate from
# the sample mean X_bar two quantities L and U such that the events 
# μ < L, L ≤ μ ≤ U, and U < μ all have probability 1/3. Do a 
# simulation with 3000 trials to show that this idea works. You can 
# get the required quantiles either from qcauchy() or from qt().

x <- rnorm(100,5,2)
S <- sd(x)
# T <- sqrt(2)*(mean(x) -5)/S
q1 <- qt(.3333,1)
q2 <- qt(.6666,1)

# re-arrange pivitol statistic to solve for u
L <- q1*S/sqrt(2) + mean(x)
U <- q2*S/sqrt(2) + mean(x)

# repeate many times in a simulation
N<-3000
low.count <- 0;
middle.count <- 0;
high.count <- 0;
for(i in 1:N) {
    x <- rnorm(2,5,2)
    S <- sd(x)
    # T <- sqrt(2)*(mean(x) -5)/S
    L <- q1*S/sqrt(2) + mean(x)
    U <- q2*S/sqrt(2) + mean(x)
    if (5 < L) low.count <- low.count +1
    if (L <= 5 && U >= 5) middle.count <- middle.count +1
    if (5 > U) high.count <- high.count +1
}
low.count/N
middle.count/N
high.count/N

print("all three events seem to occur with the same frequency - about 1/3 of 
    the time")

