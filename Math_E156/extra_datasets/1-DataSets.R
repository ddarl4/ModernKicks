#1-DataSets.R
#Based on pages 13-24 of the textbook
#This data set includes all flights out of LaGuardia on American and United during May and June 2009.
#It is the source of many examples in the textbook and the accompanying R tutorial
FlightDelays<-read.csv("FlightDelays.csv") #assumes "Starts In" was set up in shortcut
head(FlightDelays) #shows all columns and a bit of data
ncol(FlightDelays)
nrow(FlightDelays)
dim(FlightDelays)
length(FlightDelays)        #because it is a list of columns (variables)
length(FlightDelays$Month)  #each column is a vector of observations
#List all the values that appear in a "Categorical" column, with their frequencies
table(FlightDelays$Destination)
barplot(table(FlightDelays$Destination)) #table contains the labels for the bars
table(FlightDelays$Destination, FlightDelays$Carrier) #see who flies where -- only O'Hare in common
table(FlightDelays$Carrier, FlightDelays$FlightNo) #both have a flight 745
#Exploring a numeric variable
delay<- FlightDelays$Delay #extract a vector from a data frame
median(delay) 
mean(delay) #positive --  bad news for travelers
mean(delay, trim = .10) #get rid of the worst ten percent at each end
summary(delay) #combines lots of informative statistics
boxplot(delay) # shows the summary and all the outliers
#Now condition on a categorical variable
Spl<-split(delay, FlightDelays$Carrier)   #splits into a list of vectors
mean(Spl$AA); mean(Spl$UA); mode(Spl)     #split produces a list
lapply(Spl, mean)                         #list of conditional means
lapply(split(delay, FlightDelays$Day),mean) #all in one line
#It is even possible to split on multiple factors
lapply(split(delay, list(FlightDelays$Day, FlightDelays$Carrier)),mean) #all in one line
#The boxplot() function automates this process
boxplot(delay ~ Carrier, data = FlightDelays)
boxplot(delay ~ Day, data = FlightDelays)
boxplot(delay ~ DepartTime, data = FlightDelays)
#Make a new data frame with just the data for O'Hare, the only common destination
FlightsORD<-subset(FlightDelays, subset = Destination == "ORD")
table(FlightsORD$Day, FlightsORD$Carrier)
#Use tapply() to find conditional expectations
tapply(FlightsORD$Delay, FlightsORD$Carrier, mean)
tapply(FlightsORD$Delay, FlightsORD$Day, mean)
barplot(tapply(FlightsORD$Delay, FlightsORD$Day, mean))

#Section 2.5 - experimental cumulative distribution functions
#Start with a probability example
Dice<-read.csv("Dice3.csv"); head(Dice)
total<-Dice$Red3+Dice$Green3+Dice$White3
plot(table(total)/sum(total), ylab = "Probability Density") #density function
plot.ecdf(total)     #the cumulative distibution function
#Same approach works with real data
Beerwings <-read.csv("Beerwings.csv");head(Beerwings)
#Split up the beer consumption by gender
tapply(Beerwings$Beer, Beerwings$Gender, mean)
tapply(Beerwings$Beer, Beerwings$Gender, median)
lapply(split(Beerwings$Beer, Beerwings$Gender),mean)  #alternative
#Every random variable has a distribution function, which R will compute for us
#select chooses the column to turn into a vector; subset specifies which rows to keep
beerMale<-subset(Beerwings, select = Beer, subset = (Gender == "M"), drop = TRUE); beerMale # a vector
split(Beerwings$Beer, Beerwings$Gender)$M  #shorter alternative
plot(table(beerMale)/sum(beerMale),ylab = "Probability Density") #density
plot.ecdf(beerMale, xlab= "ounces")  #distribution
#It is easy to add the female data to the plot
beerFemale<-split(Beerwings$Beer, Beerwings$Gender)$F
plot.ecdf(beerFemale, col = "blue", pch = 2, add = T)
#Let's rescale by the ratio of the means
beerRescale <- beerFemale*mean(beerMale)/mean(beerFemale)
plot.ecdf(beerRescale, col = "red", pch = 3, add = T)

