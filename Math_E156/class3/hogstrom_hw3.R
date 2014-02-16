# Larson Hogstrom - HW2
# MATH_E156 - 2/16/2014

#### Part 1 ###

# 1. Problem 9 on page 70 (permutation test for a numeric variable)
# a) compare the empirical distribuion functions of the number of strikeouts
# per game (StrikeOuts) for games played at home and games played away (Locastion)
phil <-read.csv("Phillies2009.csv"); 
hist(phil[phil$Location == 'Home',]$StrikeOuts, breaks ="FD",col=rgb(1,0,0,0.5))
hist(phil[phil$Location == 'Away',]$StrikeOuts, breaks ="FD",col=rgb(0,0,1,0.5),add=TRUE)
legend("topright", c("Away", "Home"), fill=c("blue", "red"))
# b) Find the mean number of strikeouts per game for the games played at
# home and games played away from home
index <- which(phil$Location == 'Home'); index    #row numbers for the home games
SOhome <- mean(phil$StrikeOuts[index]); SOhome
SOAway <- mean(phil$StrikeOuts[-index]); SOAway
homefield <- mean(phil$StrikeOuts[index])-mean(phil$StrikeOuts[-index]); homefield

# c) perform a permutation test to see if the differences in means is statstically 
# significant
N=10^5-1; result<-numeric(N)  #99999 random subsets should be enough
for (i in 1:N) {
  index = sample(162, size=81, replace = FALSE) #random subset
  result[i]=mean(phil$StrikeOuts[index])-mean(phil$StrikeOuts[-index])
}
hist(result, breaks = "FD")
abline(v = homefield, col = "red")
#calculate p-value
soMore<-which(result >= homefield)
pVal<-(length(soMore)+1)/(length(result)+1); pVal # one tailed
# testing the hypothesis that Home games would result in higher strike outs
#(I'm not sure if these are offensive or definsive strikeouts, so not sure about
    # directionality)

#### Part 2 ###

# 2. Problem 12 on page 71 (permutation and chi square test for a 2 x 2 con- tingency table)
# a) create a table to summarize the relationship between age of target
# consumer and shelf location

# b) Conduct a chi-square test using chisq.test command

# c) R returns a warning message. Compute the expected counts for each 
# cell to see why

# d) Conduct a permuatation test for independence, adapting the code on
# page 56

#### Part 3 ###

# 3. Problem 7 on page 69 (flight delays, including comparison of variances)


#### Part 4 ###

# 4. In the 2013 regular season, the San Diego Chargers played 16 games, 
# win- ning 9 and losing 7, in the sequence LWLWLWWLLLWLWWWW.
# It is generally felt that they “finished strong” in winning four of their 
# last six games.
# (a) Carry out an exact permutation test to determine the probability 
# that is the wins and losses were scrambled, the last six games would 
# include four or more wins.
# (b) Replicate your answer by using the multinomial distribution.
# (c) Do a binomial approximation to find the probability that if the 
# Charg- ers play six games with a probability p = 9/16 of winning each, 
# they will win four or more of the six games.
# (d) Rerun the permutation test by using 10,000 randomly chosen samples of 6 games.

#### Part 5 ###

# 5. Problem 10a on page 70. First carry out the built-in chi-square test 
# in R (The hard part may be getting the data into the right format), then 
# repeat the test by creatting a data frame with two columns and 286 rows 
# and carrying out a permutation test using the chi square statistic. You 
# should get good but not perfect agreement.


