# Larson Hogstrom - Section 11
# MATH_E156 - 4/25/2014

# 1. Exercise 11 on page 202 - answer on page 403.
# Girsl2004 - body weights

Girls2004 <- read.csv("Girls2004.csv"); Girls2004
# a) create exploratory plots and compare the distribution of weights 
# between babies born in Wyoming and Alaska
is_WY <- Girls2004$State == 'WY'
is_AK <- Girls2004$State == 'AK'
akWeight <- Girls2004[is_AK,]$Weight; akWeight
wyWeight <- Girls2004[is_WY,]$Weight; wyWeight

hist(akWeight)
hist(wyWeight)
boxplot(Weight ~ State, data=Girls2004)

# b) Find a 95% t confidence interval for the mean difference in weights for 
# girls born in these two states. 

# built in R function
t.test(akWeight, wyWeight)

# equations using T dist
M1<-mean(akWeight); M2<-mean(wyWeight); M1; M2 #is this difference significant?
n1 <- length(akWeight); n2 <- length(wyWeight); n1; n2 #not the same size
S1<- sd(akWeight); S2<- sd(wyWeight); S1; S2
SE <- sqrt(S1^2/n1 + S2^2/n2); SE
ndf <- (S1^2/n1 + S2^2/n2)^2/(S1^4/(n1^2*(n1-1)) + S2^4/(n2^2*(n2-1))); ndf
tqnt<- qt(c(.025, .975),ndf); tqnt
interval<- (M1-M2)+ SE*tqnt; interval #large, but does not include zero!
curve(dt(x, df  = ndf), from = -5, to =5)
abline(v = (M1-M2)/SE, col = "red" )
pval <-pt((M1-M2)/SE, df = ndf, lower.tail = FALSE); pval
#For a two-sided test we ought to double this
2*pval    #compare with the automated test

print("The interval does not include 0 so we conclude that Alaska girls have a 
    higher weight than those from WY")


# 2. Exercise 23 on page 205 - partial answers on page 403. After solving 
# the problem, make a dataframe to simulate the test results. Do a permuta- 
# tion test to see whether the drug is effective, and construct bootstrap t 
# confidence intervals to compare with your answers to (a), (b), and (d).

# 700 are randomly assigned either drug or placebo for hives. 
# 34 of the 350 who took the drug break out in hives compared to
# 56 of 350 students who took the placebo

# a) compute a 95% confidence interval for the the proportion of students
# taking the drug who break out in hives
prop.test(34,350,conf.level=.95,correct=FALSE)$conf

# b) repeate for placebo
prop.test(56,350,conf.level=.95,correct=FALSE)$conf

# c) Do the intervals overlap? can we conclude anything about the drug?
# (is this a good way to evlaute this?)
print("the confidence intervals for the proportion of hives on drug vs. placebo 
    do overlap. The book cautions however, 'Looking for overlap in individual 
    confidence intervals is not a good way to test for significan differences'.")

# d) compute a 95% confidence interval for the difference in proportion of
# hives in drug vs. placebo
prop.test(c(56,34),c(350,350),correct=FALSE)$conf
print("testing the placebo vs. drug proportions dirrectly, suggests there
is a positive effect for the drug.")


# 3. Exercise 34 on page 208. The quantity 2λX is a “pivotal statistic,” 
# since it has a known distribution (chi square) that does not depend on 
# lambda. Do a simulation where you draw a random X from Gamma(2,3)4000 
# times, compute the lower bound L and upper bound U of the confidence 
# interval in each case, and count how many times the true value of λ falls
#  outside your confidence interval on either side.

# Let X~Gamma(2,λ). 2λX has a chi-square dist. with 4 d.o.f.. Use this 
# fact to find a 95% confidence interval for λ
    
q1 <- qchisq(.025,4)
q2 <- qchisq(.975,4)

N<-4000
counter <- 0;
for(i in 1:N) {
    p<-3
    x <- rgamma(1,2,p)
    L <- q1/(2*x)
    U <- q2/(2*x)
    if (L < p && U > p) counter <- counter + 1 #count +1 if we were correct
}
counter/N
