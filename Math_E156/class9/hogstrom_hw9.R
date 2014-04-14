# Larson Hogstrom - HW9
# MATH_E156 - 4/13/2014

### part 1 ###

# Frpm Section 8 - 5. Page 202, exercise 6. 

# 90% confidence interval 
# (1-apha/2) quantile of t-distribution - value looked up from table for n-1 df:
# q<- 1.729
q <- qt(.95,df=19)
#observed ice cream values
n <- 20
x.bar <- 18.05
S <- 5

# forumula 7.8 - T confidence interval for normal mean with unkown stdev:
# X.bar +/- (q*S/sqrt(n))
Lower <- x.bar - (q*S/sqrt(n)); Lower
Upper <- x.bar + (q*S/sqrt(n)); Upper

# After solving the problem, do a simulation of 
# 1000 tri- als, drawing ice cream samples from a normal distribution with 
# population mean 18.05 g and population variance 5 g, and make a plot like 
# the one in figure 7.1 to show that the t confidence interval performs as 
# expected.

print("We can check the confidence interval from the first sample")
counter <- 0
plot(x =c(10,27), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(n,x.bar, S) #random sample
  if (Lower < mean(x) && Upper > mean(x)) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) points(mean(x), i)
}
abline (v = Lower, col = "red") #vertical line at true mean
abline (v = Upper, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?


print("We can also adapt the t-interval to each sample")
counter <- 0
plot(x =c(10,27), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
for (i in 1:1000) {
  x <-rnorm(n,x.bar, S) #random sample
  S.x <- sd(x)
  L <- mean(x) - (q*S.x/sqrt(n)); Lower
  U <- mean(x) + (q*S.x/sqrt(n)); Upper
  if (L < x.bar && U > x.bar) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = x.bar, col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?
print("T-confidence interval got the anser 'right' about 90% of the time")

### part 2 ###

# 6. Page 204, exercise 19. 

n = 500
mean.te = 5.29
sd.te = 3.52
q <- qt(.75,df=(n-1))

print("compute a 75% one-sided upper t confidence interval for the true mean tax error")
# forumula from page 189: x.bar + (q *S/sqrt(n))
CI.upper <- mean.te - (q *sd.te/sqrt(n)); CI.upper

# After solving the problem (the answer is on 
#     page 403), do a simulation where the sales tax paid is a random 
# variable with a gamma distribution with a mean of 5 dollars and a standard 
# deviation of $3.50 (see page 408 for the formulas that will let you compute
#  the parameters). Do a simulation with samples of size 500, and determine 
# how well the 75% upper t confidence interval performs.

n = 500
mean.te = 5
sd.te = 3.5
q <- qt(.75,df=(n-1))
CI.upper <- mean.te - (q *sd.te/sqrt(n)); CI.upper

# properties of gamma distribution:
# x.mean = shape/r
# var = shape/r^2
# re-arrangment to get:
rate <- mean.te/sd.te^2
shape <- mean.te*rate

# check that the gamma parameters give the desired mean and stdev
curve(dgamma(x,shape,rate), col = "red",from=0,to=10)
mean(rgamma(10000,shape=shape,rate=rate))
sd(rgamma(10000,shape=shape,rate=rate))

# perform simulation
counter <- 0
for (i in 1:10000) {
  x <-rgamma(n,shape=shape,rate=rate) #random sample
  if (CI.upper < mean(x)) counter <- counter + 1 #count +1 if we were correct
}
counter/10000 #what fraction of the time did our confidence interval include the true mean?
print("The upper confidence interval is correct about 75% of the time as expected.
This is true even though the data does not fit the normal asumption.")

# This is interesting because with samples of 500, the CLT works pretty 
# well even with a skewed distribution, but the theory behind Student t 
# assumes that the underlying distribution is normal. This subtlety was 
# probably wasted on the officials who drew up the policy.

### Part 3 ### 

# Exercise 12 on page 295. For part (c) look on page 261 of page 5 of the math notes.



# ### Part 4 ### 

# (a) Exercise 14 on pages 295-296. 
Ill <- read.csv('Illiteracy.csv'); head(Ill)
ill <- Ill$Illit
births <- Ill$Births

# a) scatter plot of birth x Illiteracy
plot(ill,births)

print("birth rate and illiteracy seem positivly related")

# b) find equation of least-squates line, and r^2
b <- sum( (ill-mean(ill)) * (births-mean(births)) / sum((ill-mean(ill))^2));b #equation 9.4
#Here is the formula for the intercept
a <- mean(births) - b*mean(ill);a   #equation 9.5
abline(a, b, col = "red")

# c) create residual plots 
PredictBirths <- a + b * ill
#The residual is the observation minus the prediction
ResidBirths <- births - PredictBirths
plot(ill,ResidBirths)    #by construction, the residuals have a mean of zero
abline(h=0, col = "red")
print("the residuals seem well suited for a linear model")

# d) Can we say that improving literacy will  cause the 
# brith rates to go down?
print("no, correlation does not imply causation")

# (b) Exercise 24 on page 298.
a) find correlation between illiteracy rates and births via bootstrap

# find 95% CI using bootstrap

# b) using a permuation test, find if illiteracy rates and birth rates
# are independent


# 

# ### Part 5 ### 

# Exercise 34 on pages 299-300 – very similar to the last section problem.
