#Funny dice.R

#You have been given two fair dice. 
#The faces of the red die show 1,2,2,3,3,4
#The faces of the green die show 1,3,4,5,6,8
#Question: can these sustitute for a pair of standard dice?

#We will create a data frame with one row for each possible outcome of a die roll
#First do it by hand using the data editor.

Fundice = data.frame(Red=numeric(),Green=numeric()) #empty data frame
Fundice = edit(Fundice) #bring up the editor
#Inspect the result
Fundice

#An alternative: create each column as a vector

Red<-c(rep(1,6),rep(2,12),rep(3,12),rep(4,6))
Red
Green<-rep(c(1,3,4,5,6,8),6)
Green

#Now combine the columns into a data frame
FunnyDice<-data.frame(Red,Green) #the types are known
FunnyDice

Create a vector with the total roll (add random variables)
total<-FunnyDice$Red+FunnyDice$Green
total

#Make a table of the frequency of each total
tbl <-table(total)
tbl
barplot(tbl)

#If the dice are fair, each row has probability 1/36. The mass function si
mass <- tbl/36 #the table behaves like a vector!
barplot(mass)

#Since each possible roll of the dice is equally likely, we can simulate rolling the dice by sampling
#sample() requires a vector as its argument
sample(total, 10, replace= TRUE)
sample(total, 10, replace= TRUE) #different result
#Since a column is a vector, we can do the same for one die
sample(FunnyDice$Red, 10, replace= TRUE)
sample(FunnyDice$Red, 10, replace= TRUE) #different result
#If we try sample directly from a data frame, we get samples for each row!
sample(FunnyDice, 5, replace= TRUE)
#However, it is easy to extract one or more rows from a data frame
FunnyDice[14,]
FunnyDice[c(1,4,6),]
#So we sample from the index values and get a new data frame
indices<-sample(1:36, 10, replace= TRUE)
indices
FunnyDice[indices,]
is.data.frame(FunnyDice[indices,])
#A data frame with 36 rows is now a genuine random sample
sample36<-FunnyDice[sample(1:36, 36, replace= TRUE),]
sample36
barplot(table(sample36$Red+sample36$Green)) #looks quite different
#With lots of samples the histogram looks like the mass function
sample3600<-FunnyDice[sample(1:36, 3600, replace= TRUE),]
barplot(table(sample3600$Red+sample3600$Green))



