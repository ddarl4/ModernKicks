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

# forumula from page 189: x.bar + (q *S/sqrt(n))
CI.upper <- mean.te - (q *sd.te/sqrt(n)); CI.upper
paste(round(CI.upper,3), " to infinity is the 75% one-sided upper t confidence interval for the true mean tax error")

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
Beer <- read.csv('Beerwings.csv'); head(Beer)
br <-Beer$Beer
hw <- Beer$Hotwings

# a) scatter plot of ber consumed against wings eaten
# #find correlation between the two variables
plot(hw,br)
cor(hw,br)

# b) find the least-squares regression line (hotwings as the independent variable)
b <- sum( (hw-mean(hw)) * (br-mean(br)) / sum((hw-mean(hw))^2));b #equation 9.4
#Here is the formula for the intercept
a <- mean(br) - b*mean(hw);a   #equation 9.5
abline(a, b, col = "red")
print("There is a positive relationship between beer and hotwing consumption")

# c) compute R-squared and state the interpretation of this statistice
r_squared <- cor(hw,br)^2; r_squared
paste(round(r_squared, 3),"% of the variance in beer consumption is explained by the hotwings consumed",sep="")

# ### Part 4 ### 

# (a) Exercise 14 on pages 295-296. 
Ill <- read.csv('Illiteracy.csv'); head(Ill)
ill <- Ill$Illit
births <- Ill$Births

# a) scatter plot of birth x Illiteracy
plot(ill,births)
print("birth rate and illiteracy seem positivly related")

# b) find equation of least-squates line, and r^2
#Look at the correlation
cor(ill,births)
# slope
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
print("the residuals seem well suited for a linear model. They are evenly
  distributed above and bellow 0")

# d) Can we say that improving literacy will  cause the 
# brith rates to go down?
print("no, correlation does not imply causation")

# (b) Exercise 24 on page 298.
# a) find correlation between illiteracy rates and births via bootstrap

n<- length(ill)
N <- 10^4; cor.boot <- numeric(N); 
for (i in 1:N){
  index <- sample(1:n,n, replace = TRUE); index 
  ill.boot <- ill[index]
  births.boot <- births[index]
  cor.boot[i] <- cor(ill.boot,births.boot)
}
mean(cor.boot)
hist(cor.boot)
abline(v = mean(cor.boot), col = "red")   #from the resampled data
abline(v = cor(ill,births), col = "blue")   #observed corr

# & find the 95% CI using bootstrap
quantile( cor.boot, c(.025, .975)) #95% confidence interval for correlations

# b) using a permuation test, find if illiteracy rates and birth rates
# are independent
n<- length(ill)
N <- 10^4; cor.boot <- numeric(N); 
for (i in 1:N){
  index <- sample(1:n,n, replace = FALSE); index 
  ill.boot <- ill
  births.boot <- births[index] #scrample on axis
  cor.boot[i] <- cor(ill.boot,births.boot)
}
mean(cor.boot)
hist(cor.boot,xlim=c(-1,1))
abline(v = cor(ill,births), col = "blue")   #observed corr
pVal <- (sum(cor.boot >= cor(ill,births))+1)/(N + 1); pVal
print("out of 10^4 permutations, none were as or more extreme than the
    observed cor. Illiteracy and birth rates are not independent.")

# ### Part 5 ### 

# Exercise 34 on pages 299-300 – very similar to the last section problem.
Titanic <- read.csv('Titanic.csv'); head(Titanic)
survived <- Titanic$Survived
age <- Titanic$Age

plot(age,survived)
b <- sum( (age-mean(age)) * (survived-mean(survived)) / sum((age-mean(age))^2));b #equation 9.4
a <- mean(survived) - b*mean(age);a   #equation 9.5
abline(a, b, col = "red") 

# a) Find the logistic equation modeling the log-odds of a male passenger
# surving against age
library(stats4)
MLL<- function(alpha, beta) -sum( log( exp(alpha+beta*age)/(1+exp(alpha+beta*age)) )*survived+ log(1/(1+exp(alpha+beta*age)))*(1-survived) )
results<-mle(MLL,start = list(alpha = -0.1, beta = -0.02))
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)

# b) compare the odds of survival for a 30-year-old male owith a 40 year 30-year-old 
# male.

# using observed values:
prob_30 <- sum((Titanic$Age == 30)*Titanic$Survived)/sum((Titanic$Age== 30)); prob_30
prob_40 <- sum((Titanic$Age == 40)*Titanic$Survived)/sum((Titanic$Age== 40)); prob_40
# you could also do this with linear or logistic predictions

# c) Find a 95% bootstrap percentile interval for the slope.

# test linear slope
n <- length(survived)
N <- 5000; cor.boot <- numeric(N); alpha.boot <- numeric(N)
beta.boot <- numeric(N);
for (i in 1:N){
  index <- sample(1:n,n, replace = TRUE); index 
  age.resamp <- age[index]
  survived.resamp <- survived[index]
  beta.boot[i] <- sum( (age.resamp-mean(age.resamp)) * (survived.resamp-mean(survived.resamp)) / sum((age.resamp-mean(age.resamp))^2))
  alpha.boot[i] <- mean(survived.resamp) - beta.boot[i]*mean(age.resamp)  #equation 9.5
}
hist(beta.boot)   #resembles Figure 9.17a
quantile( beta.boot, c(.025, .975)) #95% confidence interval for correlations

#We can also draw 50 of the regression lines
plot(age, survived)
for (i in 1:50) {
  abline(alpha.boot[i], beta.boot[i], col = "red")   #from the resampled data
}
abline(a, b, lwd = 4)   #from the origonal data

# d) estimate the probibility of a 69-year-old male surviving, and finda 
# 95% bootstrap percentile interval for the probability
sum((Titanic$Age== 69))
print("there were no data for 69 year old men recorded so we will have to infer")

#predict 69-year-old male survival rate with bootstraped equations
prob_69 <- numeric(N)
for (i in 1:N){
    prob_69[i] <- 69*beta.boot[i] + alpha.boot[i]
}
predict_orig <- (69*b + a); predict_orig
mean(prob_69)
print("the predicted probability from origonal linear eqn is very close to the
    mean predicted value from the bootstraped eqn") 
hist(prob_69)
quantile( predict_orig, c(.025, .975)) #95% confidence interval for percent survival
