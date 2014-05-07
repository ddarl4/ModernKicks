
#bayesian analysis
# binomial distribution, with vector of priors 
# win = 1, loose = 0
# after priors were set, 3 observations of WLL
theta <- seq(0,1, by=.1) #expected chance of wining
prior <- c( 0, .02, .03, .05, .1, .15, .2, .25, .15, .05, 0) 
likelihood <- theta*(1-theta)^2 # p^W*(1-p)^(n-W)
constant <- sum(prior*likelihood)
posterior <- prior*likelihood/constant

# expected value of prior
sum(theta*prior)
# expected of posterior
sum(theta*posterior)



# what if you play 5 more times so your record is
# WLLWWLLL
likelihood2 <- theta^3*(1-theta)^5
constant2 <- sum(prior*likelihood2)
posterior2 <- prior*likelihood2/constant2
# expected of posterior
sum(theta*posterior2)

# use your first posterior distribution and update that with 5 new games
likelihood3 <- theta^2*(1-theta)^3
constant3 <- sum(posterior*likelihood3)
posterior3 <- posterior*likelihood3/constant3
#compare both ways of calculating updated posterior distribution
posterior2 == posterior3
round(posterior2,8) == round(posterior3,8)

# plot prior, first posterior, and update posterior distribution
plot(theta,prior,col='red', type='b', ylim=c(0,.3),ylab='probability')
lines(theta,posterior,type='b',lty=2)
lines(theta,posterior2,type='b',lty=3)



