#Math E-156 Script 3A-PermutationTest.R
#Based on Sections 3.1 and 3.2, pages 35-44

#Topic 1 - a tiny permutation test
#We generate a small data frame with the data from section 3.1
Time<-c(30,25,20,18,21,22)   #times for six mice in a maze
Group<-c(rep("Drug",3),rep("Control",3))  #first three got the slowdown drug
#Create a data frame
MazeTimes<-data.frame(Time,Group); MazeTimes
#Calculate the mean time for each of the two groups
tapply(MazeTimes$Time,MazeTimes$Group,mean) #drug slows them down

#Here is a clever way to compute the time difference.
index <-which(Group=="Drug"); index   #rows with drugged mice
slowdown <-mean(MazeTimes$Time[index]-MazeTimes$Time[-index]); slowdown

#Question: is this difference in times significant?
#What would have happened if we selected a random subset of the three mice?
#Here is a way to generate all possible subsets of three of the six mice
AllSubsets<-combn(1:6,3); AllSubsets; 
AllSubsets[,1] #each subset is a column
N<-ncol(AllSubsets); timediff <-numeric(N); create empty vector of the right length.
#Now deal with each subset the same way that we dealt with the three drugged mice
for (i in 1:N) {
  index=AllSubsets[,i]
  timediff[i]=mean(MazeTimes$Time[index]-MazeTimes$Time[-index])
}
timediff   #look at all 20 possible time differences  
#We have replicated the last column of Table 3.1 from the textbook
#The rows came up in a different order, however.

#Now display a histogram and highlight the value for the drugged mice.
hist(timediff, breaks = 20)     #avoids grouping together different values
abline(v = slowdown, col = "red")

#Which random subsets of three mice were slowed down as much as the drugged subset
slower<-which(timediff >= slowdown); slower   #three of them, including the drugged subset

#The P-value is the probability that the observed slowdown could have arisen by chance
pVal<-length(slower)/length(timediff); pVal   #15 percent
#Most reasonable people would now regard this drug test as inconclusive.
#It should be repeated with a larger sample.

#Topic 2 - a permutation test with a larger data set
#Do the Patriots have a home field advantage when playing the Buffalo Bills?
#Look at the data for the seasons from 2013 back to 2009
ScoreDiff<-c(14,2,6,24,28,-3,31,8,7,1)   #Pats score - Bills score -- usually positive
Venue<-c("H","A","H","A","H","A","A","H","A","H") #5 home games, 5 away

#Convert these data to a data frame
Bills<-data.frame(ScoreDiff,Venue); Bills
index=which(Venue=="H"); index    #row numbers for the home games
homefield=mean(Bills$ScoreDiff[index]-mean(Bills$ScoreDiff[-index])); homefield

#Surprise -- the "home field advantage" is slightly negative, but is the number significant?
AllSubsets<-combn(1:10,5) #generate all possible subsets of 5 of the 10 games
N <-ncol(AllSubsets); N; scorediff <- numeric(N)  #now we have 252 subsets

#Run the permutation test -- calculate the score difference for all 5 subsets
for (i in 1:N) {
  index=AllSubsets[,i]     #extract column i
  scorediff[i]=mean(Bills$ScoreDiff[index]-mean(Bills$ScoreDiff[-index]))
}
head(scorediff)  #usually a few points one way or the other
hist(scorediff, breaks ="FD")
abline(v = homefield, col = "red")  #highlight the actual result
#Clearly this small difference could easily have arisen by chance

#Topic 3 - data from an actual experiment
#This is very much like the previous two examples.

#Let's try 16 spruce trees, 10 of which were fertilized
Spruce<-read.csv("Spruce.csv"); head(Spruce)
#Extract the first 16 rows and the two columns of interest.
Spruce<-Spruce[c(1:16), c("Fertilizer","Ht.change")]; Spruce
#Notice that a couple of the non-fertilized trees did pretty well!
index <-which(Spruce$Fertilizer=="NF"); index  #use the smaller subset of 6

#Compare mean height change for the fertilized and non-fertilized subsets
benefit <-mean(Spruce$Ht.change[-index])-mean(Spruce$Ht.change[index]); benefit
#So fertilizer helps. Is it significant?
AllSubsets<-combn(1:16,6)   #there were 6 non-fertilized trees
N <-ncol(AllSubsets); N ; gain<-numeric(N) #plenty of subsets this time!

#Run the permutation test
for (i in 1:N) {
  index=AllSubsets[,i]
  gain[i]=mean(Spruce$Ht.change[-index])-mean(Spruce$Ht.change[index])
}
hist(gain, breaks = "FD")
abline(v = benefit, col = "red")

#The observed gain appears unlikely to arise by chance -- how unlikely?
better<-which(gain >= benefit)   #random subsets that matched or exceeded the observed benefit
pVal<-length(better)/length(gain)
pVal #probability that observed benefit of fertilizer can arise by chance
#More data would be nice, but there are already a lot of subsets!
#The histogram is also looking suspiciously "normal."

#Topic 4 - generating the subsets by random sampling
#With more data, trying all the subsets takes too much computation
Beerwings<-read.csv("Beerwings.csv")
tapply(Beerwings$Hotwings, Beerwings$Gender, mean)  #men eat more hot wings

#Calculate the difference in a way that will work for random subsets
index=which(Beerwings$Gender=="M")
observed=mean(Beerwings$Hotwings[index])-mean(Beerwings$Hotwings[-index]); observed
N=10^5-1; result<-numeric(N)  #99999 random subsets should be enough
for (i in 1:N) {
  index = sample(30, size=15, replace = FALSE) #random subset
  result[i]=mean(Beerwings$Hotwings[index])-mean(Beerwings$Hotwings[-index])
}
hist(result, breaks = "FD")
abline(v = observed, col = "red")
#The observed difference appears extremely unlikely to arise by chance
#The P-value will specify exactly how unlikely
eatMore<-which(result >= observed)
pVal<-(length(eatMore)+1)/(length(result)+1) #include the actual sample
pVal #will not be exactly the same number as in the textbook

#Other statistics can be used  -- repeat the calculation, using the median instead
tapply(Beerwings$Hotwings, Beerwings$Gender, median) #men eat more
index=which(Beerwings$Gender=="M")
observed=median(Beerwings$Hotwings[index])-median(Beerwings$Hotwings[-index]); observed
N=10^5-1; result<-numeric(N) 
for (i in 1:N) {
  index = sample(30, size=15, replace = FALSE) #random subset
  result[i]=median(Beerwings$Hotwings[index])-median(Beerwings$Hotwings[-index])
}
hist(result, breaks = "FD")  #difference of medians must be an integer
abline(v = observed, col = "red")
#The observed difference is extremely unlikely to arise by chance
eatMore<-which(result >= observed)
pVal<-(length(eatMore)+1)/(length(result)+1); pVal 
#By including the observation along with the random samples we can avoid a P-value of zero