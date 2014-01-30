#Math E-156, script 1A-DiceProbs
#Populations and Samples
#Make a vector that lists all the outcomes of rolling one die
X <- 1:6; X
Omega <- data.frame(X); Omega #convert to a data frame 
#Now we have a sample space, and if we assume the die is fair we have a probability space
#X is a random variable
mu <-mean(Omega$X); mu  #expectation of X
sigmaSq <- mean((Omega$X - mu)^2); sigmaSq #variance
#We can specify an event( a subset of the sample space)
odd <- which(X %% 2 == 1); odd    # a set of row indices
#We can calculate the probability of this event and the conditional expectation
length(odd)/length(X)    # probabilty of an odd roll
mean(odd)         #conditional expectation
#For a pair of dice, one red, one green, we need two columns
Red2 <- rep(X, each = 6); Red2
Green2 <- rep(X, 6); Green2
Total <- Red2 + Green2       #sum of random variables
Omega2<- data.frame(Red2, Green2, Total); Omega2  #all 36 outcomes
CrapsWin <- which(Total == 7 | Total == 11)     #event "win at Craps"
length(CrapsWin)/nrow(Omega2)  #probability of winning
attach(Omega2)   #now we can use just the column names - a bit risky
mean((Red2 - mu)*(Green2-mu))    #confirms that the die rolls are uncorrelated
cor (Red2, Green2)               #built-in function
#Since each row has the same probability, we can sample from this population
Omega2[4,] #extract one row - note that row index comes first

#Repeat the following block several times to see different samples
selection <- sample(1:36, 36, replace = TRUE); selection #may have duplicates
sample36 <-Omega2[selection,]  ; head(sample36)  #a data frame
sample36$Red2     #extract the first column
mean(sample36$Red2)  #a sample mean, probaly not 3.5
var(sample36$Red2, sample36$Green2)   #values close to zero suggest independence
cor(sample36$Red2, sample36$Green2)   #correlation lies in range [-1,1]

#Try again with a much larger sample
#It is not hard to add a third die
Red3 <- rep(Red2, each = 6)
Green3 <- rep(Green2, each = 6)
White3 <- rep(Green2, 6)
Omega3 <- data.frame(Red3, Green3, White3); Omega3  #216 rows, equally likely
mu3 <- mean(Red3 + Green3 + White3); mu3     #expectation of sum for 3 dice
sigmaSq3 <- mean((Red3 + Green3 + White3 -mu3)^2); sigmaSq3 #variance for 3 dice
probs <-table(Red3 + Green3 + White3); probs  #tally the sums
barplot(probs)      #this does not view the sums as numbers

#Making a nice-looking histogram is slightly tricky - break on half integers
hist(Red3+ Green3+ White3, breaks = seq(from = 2.5,to = 18.5))
#The number of sixes is a random variable
nSix <- (Red3 ==6) + (Green3 == 6) + (White3 ==6) #sum of three indicator functions
tbl <- table(nSix); tbl    #frequencies for the carnival game Chuck-A-Luck
sum(tbl)
names(tbl)      #these are now character strings but can be coerced
as.numeric(names(tbl))
sum(as.numeric(names(tbl))*tbl)   #expected number of sixes is 216/2

#Let's save this data frame for reuse
write.csv(Omega3,"Dice3.csv")   #you can open this in Excel

#We can also get samples
#Rolling one 1 die three times - remember that X is 1:6
sample(X, 3, replace = TRUE)   #allows repetition
#Repeat this experiment 10000 times and save the results
N <- 10000; rolls<-numeric(N)   #empty vector
#This is one of the few times that you need to use a loop in R
for (i in 1:N) {
  rolls[i]<-sum(sample(X, 3, replace = TRUE))
}
hist(rolls, breaks = seq(from = 2.5,to = 18.5))
#Now we are looking at a sample, not a population
x.bar <-mean(rolls); x.bar   #not exactly 10.5
mean((rolls-x.bar)^2)        #not exactly 8.75

#One way to deal with unequal probabilities
#In 2008 Mars Candy Company revealed the probabilities for colors of M&Ms (see example B.2)
#For Peanut M&Ms
colors <-c("Blue", "Orange", "Green", "Yellow", "Red", "Brown") #concatenate "c()"
proportions<-c(23, 23, 15, 15, 12, 12)   #percentages
MM <- rep(colors, proportions); MM    #vectors in action - makes a vector of 100 colors
table(MM)   #this vector specifies probabilities!
barplot(table(MM))
sum(MM == "Yellow")/length(MM)    #this is a probability
#A specific 1.74 ounce bag contained the following:
counts<-c(3, 7, 1,  6, 2, 0)
MMbag <- rep(colors, counts)       #this is sampling data
table(MMbag)
barplot(table(MMbag))
#Question: is this sample consistent with the 2008 probabilities?
#It is easy to generate more samples
sample(MM, 18)   #sample of what might be in a bag of M&Ms
#R has functions that do this sort of thing
rmultinom(1, 18, prob = proportions)  #generates contents of a bag
#It is not obvious how to decide whether our bag held an "unlikely" sample.





