# Larson Hogstrom - Section 11
# MATH_E156 - 4/25/2014

# 1. Exercise 11 on page 202 - answer on page 403.
# Girsl2004 - body weights
# a) create exploratory plots and compare the distribution of weights 
# between babies born in Wyoming and Alaska

# b) Find a 95% t confidence interval for the mean difference in weights for 
# girls born in these two states. 

print("the interval shows")


# 2. Exercise 23 on page 205 - partial answers on page 403. After solving 
# the problem, make a dataframe to simulate the test results. Do a permuta- 
# tion test to see whether the drug is effective, and construct bootstrap t 
# confidence intervals to compare with your answers to (a), (b), and (d).

# 700 are randomly assigned either drug or placebo for hives. 

# 34 of the 350 who took the drug break out in hives compared to
# 56 of 350 students who took the placebo

# a) compute a 95% confidence interval for the the proportion of students
# taking the drug who break out in hives

# b) repeate for placebo

# c) Do the intervals overlap? can we conclude anything about the drug?
# (is this a good way to evlaute this?)

# d) compute a 95% confidence interval for the difference in proportion of
# hives in drug vs. placebo

# 3. Exercise 34 on page 208. The quantity 2λX is a “pivotal statistic,” 
# since it has a known distribution (chi square) that does not depend on 
# lambda. Do a simulation where you draw a random X from Gamma(2,3)4000 
# times, compute the lower bound L and upper bound U of the confidence 
# interval in each case, and count how many times the true value of λ falls
#  outside your confidence interval on either side.

# Let X~Gamma(2,λ). 2λX has a chi-square dist. with 4 d.o.f.. Use this 
# fact to find a 95% confidence interval for n=400 when X_bar=50000.
# Compare the width of this interval with the interval based on Xmax.


