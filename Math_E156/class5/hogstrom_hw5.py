# Larson Hogstrom - HW5
# MATH_E156 - 3/3/2014

#### Part 1 ###
# 1. Page 93, exercise 10.
# in 2000 census 28.6% of adults have high school diploma
# In a random sample of 800 adults, what is the prob that betweeen 220
# and 230 (inclusive) people have a HS diploma.
n<-800
p<-.286
variance <- n*p*(1-p)
expected <- n*p; expected
# use CLT approximation with continuity correction
dist1 <- pnorm(c(230,220), expected, sqrt(variance), lower.tail = FALSE) #CLT approximation
clt <- diff(dist1); clt
# compare to the exact probability (use pbinorm in R)
dist2 <- pbinom(c(220,230),size=800,prob=.286)
exact <- diff(dist2); exact

# 2. Page 93, exercise 14. This is a twin of exercise 13, one of the section problems.
X1...X9, i.i.d. from N(7,3^2)
Y1...Y12, i.i.d. from N(10,5^2)
let W = X_bar - Y_bar

a) Give the exact sampling distribution of W

b) Simulate the sampling distribution of W in R and plot your results
(adapt code from the previous exercise). Check that the simulated mean and the 
standard error are close to the theoretical mean and standard error

c) Use your simulation to find P(W< -1.5). Calculate the exact answer and
compare


# 3. Page 94, exercise 16.

# 4. Page 95, exercise 24(b). You will have to work part (a) to do 
# the required comparison. Include a brief explanation of how you came 
# up with the theoretical expectation of Xmin.

# 5. Plot the moment-generating function for two dice, in two different ways.

# (a) Use outer() to get the probability mass function for two dice, 
# then write an R function for the moment generating function of this prob- 
# ability mass function and plot a graph of it. (See script 5P for the 
# “vectorize” trick.)

# (b) Write an R function for the moment generating function of the prob- 
# ability mass function for a single die and plot a graph of its square.