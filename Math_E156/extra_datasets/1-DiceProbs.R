#1-DiceProbs
#Populations and Samples
#Make a vector that lists all the outcomes of rolling one die
X <- 1:6; X
Omega <- data.frame(X); Omega 
#Now we have a sample space, and if we assume the die is fair we have a probability space
#X is a random variable
mu <-mean(Omega$X); mu  #expectation of X
sigmaSq <- mean((Omega$X - mu)^2); sigmaSq #variance
#We can specify an event( a subset of the sample space)
odd <- which(X %% 2 == 1); odd
#We can calculate the probability of this event and the conditional expectation
length(odd)/length(X)    # probabilty of an odd roll
mean(odd)         #conditional expectation
#For a pair of dice, one red, one green, we need two columns
Red2 <- rep(X, each = 6); Red2
Green2 <- rep(X, 6); Green2
Total <- Red2 + Green2       #sum of random variables
Omega2<- data.frame(Red2, Green2, Total); Omega2
CrapsWin <- which(Total == 7 | Total == 11)
length(CrapsWin)/nrow(Omega2)  #probability of winning
mean((Red2 - mu)*(Green2-mu))    #confirms that the die rolls are uncorrelated
#Since each row has the same probability, we can sample from this population
Omega2[4,] #extract one row
selection <- sample(1:36, 36, replace = TRUE)
sample36 <-Omega2[selection,]  ; sample36
sample36[1]
var(sample36[1], sample36[2])   #values close to zero suggest independence
#Try again with a much larger sample
#It is not hard to add a third die
Red3 <- rep(Red2, each = 6)
Green3 <- rep(Green2, each = 6)
White3 <- rep(Green2, 6)
Omega3 <- data.frame(Red3, Green3, White3); Omega3
mu3 <- mean(Red3 + Green3 + White3); mu3
sigmaSq3 <- mean((Red3 + Green3 + White3 -mu3)^2); sigmaSq3
probs <-table(Red3 + Green3 + White3)
barplot(probs)
#Making a nice-looking histogram is slightly tricky
hist(Red3+ Green3+ White3, breaks = seq(from = 2.5,to = 18.5))
#The number of sixes is a random variable
nSix <- (Red3 ==6) + (Green3 == 6) + (White3 ==6)
tbl <- table(nSix)
sum(tbl)
names(tbl)      #these are now character strings but can be coerced
as.numeric(names(tbl))
sum(as.numeric(names(tbl))*tbl)   #expected number of sixes
#Let's save this data frame
write.csv(Omega3,"Dice3.csv")   #you can open this in Excel
#We can also get samples
#Rolling one 1 die three times
sample(X, 3, replace = TRUE)
#Repeat this experiment 10000 times and save the results
N <- 10000; rolls<-numeric(N)   #empty vector
for (i in 1:N) {
  rolls[i]<-sum(sample(X, 3, replace = TRUE))
}
hist(rolls, breaks = seq(from = 2.5,to = 18.5))
#Now we are looking at a sample, not a population
x.bar <-mean(rolls); x.bar   #not exactly 10.5
mean((rolls-x.bar)^2)        #not exactly 8.75

#In 2008 Mars Candy Company revealed the probabilities for colors of M&Ms (see example B.2)
#For Peanut M&Ms
colors <-c("Blue", "Orange", "Green", "Yellow", "Red", "Brown") 
proportions<-c(23, 23, 15, 15, 12, 12)
MM <- rep(colors, proportions); MM    #vectors in action!
table(MM)
barplot(table(MM))
sum(MM == "Yellow")/length(MM)    #this is a probability
#A specific 1.74 ounce bag contained the following:
counts<-c(3, 7, 1,  6, 2, 0)
MMbag <- rep(colors, counts)       #this is sampling data
table(MMbag)
barplot(table(MMbag))
#Question: is this sample consistent with the 2008 probabilities?
#It is easy to generate more samples
sample(MM, 18)
#R has functions that do this sort of thing
rmultinom(1, 18, prob = proportions)





