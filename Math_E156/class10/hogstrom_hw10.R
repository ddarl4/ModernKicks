# Larson Hogstrom - HW10
# MATH_E156 - 4/18/2014

### Part 1 ###

# 1. You are playing a computer game where you can battle the AI by having 
# a hero from your army fight one-on-one against a randomly chosen unit 
# from the opposing army. Your probability of winning depends on what type 
# of opposing unit is chosen, as follows:
# • If it is a dragon, your probability of winning is 0.1. • If it is an 
# orc, your probability of winning is 0.3.
# • If it is a goblin, your probability of winning is 0.6. • If it is an 
# elf, your probability of winning is 0.7.
# • If it is a fairy, your probability of winning is 0.8.
# You know that the opposing army consists of one dragon, two orcs, three 
# goblins, three elves, and one fairy, and for your Bayesian prior you 
# assume that each of these ten units is equally likely to be chosen.
theta <- c(.1,.3,.6,.7,.8)
prior <- c(.1,.2,.3,.3,.1)
plot(theta, prior, type = "b", ylim = c(0, 0.90), ylab = "Probability")

# (a) You fight five battles, winning two and losing three. Find the posterior
#  distribution for the probability of selecting each type of unit.

# two wins, three losses
likelihood <- theta^2*(1-theta)^3; likelihood

#To use Bayes's Theorem, we need the probability of one win and two losses.
#This will be much less than 1, since we are better than most opponents.
P2W3L<-sum(prior* likelihood); P2W3L

#Bayes says P(theta|2W3L) = P(theta)P(2W3L|theta)/P(2W3L)
#Probability of theta, conditioned on observing one win, two losses
posterior <-prior * likelihood/ P2W3L; posterior
sum(posterior) #normalization check - new distribution must sum to 1

sum(theta*prior) #prior mean
sum(theta*posterior) #posterior mean

#Add the new "posterior" distribution to the plot
lines(theta, posterior, type="b", col = "red")


# (b) You fight ten more battles, winning three and losing seven. What is now
#  your estimate of the probability that the AI selects a dragon? An orc? A 
#  fairy?

prior.new <- posterior
# 3 wins, 7 losses
likelihood <- theta^3*(1-theta)^7; likelihood
P3W7L<-sum(prior.new* likelihood); P3W7L
posterior.new <-prior.new * likelihood/ P3W7L; posterior
sum(posterior.new) #normalization check - new distribution must sum to 1
sum(theta*posterior.new) #posterior mean
lines(theta, posterior.new, type="b", col = "green")

# (c) Show that if you group together the result of the fifteen battles, you 
# get the answers to (b) in a single step.
# 5 wins, 10 losses
likelihood <- theta^5*(1-theta)^10; likelihood
P5W10L<-sum(prior * likelihood); P5W10L
posterior.new2 <-prior * likelihood/ P5W10L; posterior
sum(posterior.new2) #normalization check - new distribution must sum to 1
sum(theta*posterior.new2) #posterior mean
lines(theta, posterior.new2, type="b", col = "blue", lty = 2)

# check that the posteiror calulated two ways gives a the same answer
round(posterior.new,5) == round(posterior.new2,5)

### Part 2 ###

# Page 324, Exercise 6. For part (a), see the comment on the second section problem.

# "Do you think of your dog as a member of your family"
# Pew Study - of the 178 people who were between 18 and 29 years old
# 160 responded years

# a) Find a 95% confidence interval for the true proportion of 19-29 years
# old who responded yes.

# bootstrap confidence interval
pet_is_family <-c(rep(1,160),rep(0,18)) 
N<-10^4; family.boot <- numeric(N)
for (i in 1:N) {
  fam.sample <- sample(pet_is_family, replace = TRUE)
  family.boot[i] <- sum(fam.sample)
}
hist(family.boot, breaks = "FD", main= "Bootstrap distribution pet is family responses")
#Extract a 95% bootstrap percentile confidence interval - proportion who responded yes
quantile(family.boot, c(.025, .975))/178

# Note that part (a) is fre- quentist, not Bayesian. You can get a good 
# approximate answer in one line by using qbinom().
obs.prob <- 160/178
qbinom(c(.025, .975),178,obs.prob)/178 # proportion who responded yes
print("binomial quantile gives aproximately the same quantile as bootstrap")

# b) 
# Find the estimates for theata (posterior means) and 95% credible intervals

# Statistician 1 - Prior Beta - mean=.85, variance=.0015
alpha.posterior.stat1 <- (.85+160)
beta.posterior.stat1 <- (.0015 + 178-160)
# Expected theta - E[X] = alpha/(alpha + Beta)
theta.expected <- alpha.posterior.stat1/(alpha.posterior.stat1 + beta.posterior.stat1); theta.expected
#95% credible intervals
qbeta(c(.025, .975),alpha.posterior.stat1, beta.posterior.stat1)
print("this is very close to the binomial estimate")

# Statistician 2 - flat prior - beta(1,1)
alpha.posterior.stat2 <- (1+160)
beta.posterior.stat2 <- (1 + 178-160)
# Expected theta - E[X] = alpha/(alpha + Beta)
theta.expected <- alpha.posterior.stat2/(alpha.posterior.stat2 + beta.posterior.stat2); theta.expected
#95% credible intervals
qbeta(c(.025, .975),alpha.posterior.stat2, beta.posterior.stat2)
print("this is very close to the binomial estimate")

# Statistician 3 - prior Beta(6,4)
alpha.posterior.stat3 <- (6+160)
beta.posterior.stat3 <- (4 + 178-160)
# Expected theta - E[X] = alpha/(alpha + Beta)
theta.expected <- alpha.posterior.stat3/(alpha.posterior.stat3 + beta.posterior.stat3); theta.expected
#95% credible intervals
qbeta(c(.025, .975),alpha.posterior.stat3, beta.posterior.stat3)
print("this is very close to the binomial estimate")

# c) For each statistician plot their prior and posterior distributions on 
# one graph

# Statistician 1 - flat prior
curve(dbeta(theta, .85, .0015), from = 0, to = 1, ylim = c(0,17), xname = "theta", ylab = "Beta density")
curve(dbeta(theta, alpha.posterior.stat1, beta.posterior.stat1), xname = "theta", col = "red", add = TRUE) 

# Statistician 2 - flat prior - beta(1,1)
curve(dbeta(theta, 1, 1), from = 0, to = 1, ylim = c(0,17), xname = "theta", ylab = "Beta density")
curve(dbeta(theta, alpha.posterior.stat2, beta.posterior.stat2), xname = "theta", col = "red", add = TRUE) 

# Statistician 3 - prior Beta(6,4)
curve(dbeta(theta, 6, 4), from = 0, to = 1, ylim = c(0,17), xname = "theta", ylab = "Beta density")
curve(dbeta(theta, alpha.posterior.stat3, beta.posterior.stat3), xname = "theta", col = "red", add = TRUE) 


# d) for each statistician, find the probibility, given the data, that theta > .90
1- pbeta(.9, alpha.posterior.stat1, beta.posterior.stat1)

# Statistician 2 - flat prior - beta(1,1)
1- pbeta(.9, alpha.posterior.stat2, beta.posterior.stat2)

# Statistician 3 - prior Beta(6,4)
1- pbeta(.9, alpha.posterior.stat3, beta.posterior.stat3)

### Part 3 ###

# Page 324, Exercise 8. Feel free to reuse code from script 10C.

# -You seek to find the mean of SAT scores in a town.
# -You assume distribution of scores as the same as the national
# average, sd = 116
# -prior: u ~ N(600,25^2)
# If a sample of size 60 yields a mean score of 538:
# a) find the posterior distribution of u
curve(dnorm(x,600,25), xlim = c(300,800))

#An ichthyologist observes a sample of 15 fish
#The mean length is 45 cm, with a variance assumed to be 8

#The formula on page 317 (skip the derivation!) gives the posterior variance
A<- 60/25^2 # 15 samples with variance assumed to be 8
A0<- 1/25^2 # inverse variance for prior
A1<- A0 + A # inverse variance for posterior
sqrt(1/A1)  #the standard deviation of the mean (posterior)

#Calculation of the posterior mean
M<- 538 # mean length for sample of 15 fish
M0 <- 600 #mean for prior
M1 <- (A0*M0 + A*M)/(A0 + A) #mean for posterior

curve(dnorm(x, M1, 1/sqrt(A1)), ,col = "red", add = TRUE) #posterior

# b) find a 95% credible interval for the true u
qnorm(c(.025, .975),M1, 1/sqrt(A1))

# c) find the probibility that the posterior mean math SAT score is
# greater than 600.
1 - pnorm(600,M1, 1/sqrt(A1))
print("the posterior distribution suggests that there is no chance the
    mean SAT score is greater than 600.")


### Part 4 ###
# Page 326, Exercise 16. This is another cute conjugate-prior problem like 
#  the third section problem. Add part (f):
# You estimate that the prior distribution has mean 8 and variance 4, 
# and you assume that it is a gamma distribution because otherwise you 
# cannot calculate the posterior density. For the same observed values 
# as in part (d), find the posterior density. Plot the prior and posterior 
# densities, and mark, using vertical lines of different colors, the 95% 
# credible interval for each.

# Let X1, X2, ... Xn be a random sample from the Poisson dist. with pdf
# f(x) = (theta^x e^-theta)/x! , x=0,1,2,3

# a) write down the likelihood function f(theta)

print("f(theta|x) = (theta^x e^-theta)/x!")

# b) suppose the prior for theta is the gamma distribution with parameter r, theta.
# Let pi(theta) denote the pdf for the prior. Find the posterior density f(theta)pi(theta)

print("prior = pi(theta)*Poisson(theta)/ integrate(pi(theta)*Poisson(theta))")


print("when I multiply the likelihood function by the gamma following:
    f(theta)pi(theta) = 1/(gamma(r)*x!) * theta^(r+x) *x^r-1 * e^-theta(x+1)
 ")

# c) recognize this posterior density as the pdf for what known distribution with
# what parameters?

print("I do not how this posterior can be represented as a common distribution
    function")


# d) Suppose you observed the values, 6,7,9,9,16 and you believe the 
# prior is gamma with parameters r=15, lambda=3. Find the posterior density.

print("I've tried a bunch to find a closed form solution to the posterior, but 
    I haven't come up with the right answer - I am not sure how to proceed in 
    this problem ")

# e) 95% credible interval for theta

# f) You estimate that the prior distribution has mean 8 and variance 4, 
# and you assume that it is a gamma distribution because otherwise you 
# cannot calculate the posterior density. For the same observed values 
# as in part (d), find the posterior density. Plot the prior and posterior 
# densities, and mark, using vertical lines of different colors, the 95% 
# credible interval for each.


### Part 5 ###

# Among players aged 25 or less, here are the top six hitters in major 
# league baseball for 2013:
# • Freddie Freeman, 176 hits in 551 at bats. • Paul Goldschmidt, 182 
# hits in 602 at bats. • Eric Hosmer, 188 hits in 623 at bats.
# • Salvador Perez, 145 hits in 496 at bats.
# • Jean Segura, 173 hits in 588 at bats.
# • Mike Trout, 190 hits in 589 at bats.
# You are a Yankees general manager whose motto is ”Great hitters 
# are bought, not made,” and you have 50 million dollars to spend 
# on hiring one of these folks when he becomes a free agent. Using 
# the “Bayes multi- arm” approach, identify which (if any) of these 
# players has a probability of less than 5 percent of being the top 
# hitter in the group and is therefore not worth considering. 

n<-c(551, 602, 623, 496, 588, 589) 
X<-c(176, 182, 188, 145, 173, 190)

alpha<- X # parameters for posterior beta distributions
beta<- n-X # parameters for posterior beta distributions

#Now we simulate 10000 random selections of the parameter values
N <- 10^5  #replications
theta<- matrix(0,0, nrow = N, ncol = 6) #10000 rows of 6 zeroes
for (j in 1:6) {
  theta[ ,j] <- rbeta(N, alpha[j], beta[j]) #fill in column j
}
head(theta)

probBest <- numeric(6) #vector for results
best <- apply(theta, 1, max) # 1 means apply max over rows
for (j in 1:6) {
  probBest[j] = mean(theta[ ,j] == best)
}
probBest 
print("Salvador Perez and Jean Segura are least likely to be consistent hitters")


# After doing the problem by Bayesian methods, repeat the analysis by 
# simply simulating 10000 seasons, using the 2013 batting average 
# as the parameter for a binomial distribution. You should reach 
# the same conclusion.

FF <- 176/551
PG <- 182/602
EH <- 188/623
SP <- 145/496
JS <- 173/588
MT <- 190/589

hit.prob <- c(176/551, 182/602, 188/623, 145/496, 173/588, 190/589)

# bootstrap confidence interval
binorm.boot<- matrix(0,0, nrow = N, ncol = 6) #10000 rows of 6 zeroes
for (j in 1:6) {
  binorm.boot[ ,j] <- rbinom(N,n[j],hit.prob[j])
}

probBest <- numeric(6) #vector for results
best <- apply(binorm.boot, 1, max) # 1 means apply max over rows
for (j in 1:6) {
  probBest[j] = mean(binorm.boot[ ,j] == best)
}
probBest 

print("Binomial findings are consistent with the multi-arm approach - 
    Salvador Perez and Jean Segura are least likely to be consistent hitters")
