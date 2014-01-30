#Math E-156 Script 1D-Case Studies.R
#Shows the data sets that are introduced in Chapter 1.
#You need to download these into your Data subfolder and set the start foplder for R correctly.
#Follow the instructions on the Web site.

#Flight delays out of La Guardia
Delays<-read.csv("FlightDelays.csv")
dim(Delays)   #lots of observations and 10 columns
nrow(Delays)  #how many observations
length(Delays) # a data frame is a list of columns with this length
head(Delays)   #sneak a peek
barplot(table(Delays$Destination))  # factor variable
table(Delays$Day, Delays$Delayed30) #a contingency table from two factors
mean(Delays$Delay)     #mean of a numeric vector columnon course-related stuff,
median(Delays$Delay)   #negative - the data must be skewed
hist(Delays$Delay, breaks = "FD")   #long tail
#You may need to move the cursor back into this script!

#Birth Weights of babies in North Carolina
NCB<- read.csv("NCBirths2004.csv"); head(NCB)
hist(NCB$Weight, breaks = "FD", prob = TRUE)       #not badly skewed; "FD" gives more bars than the default

#Compare our sample with a probability model for the population
curve(dnorm(x, mean(NCB$Weight), sd(NCB$Weight)), col = "red", add=TRUE) #overlay a normal distribution
#The fit is pretty good, but it's hard to credit the Central Limit Theorem!

#There are lots of ways to split up these data
byGender<-split(NCB$Weight, NCB$Gender) #splits Weight into a list of vectors, one for each gender
lapply(byGender, mean)   #applies mean() to each item in the list
hist(byGender$Male, breaks = "FD", main = "Histogram by Gender")
hist(byGender$Female, breaks = "FD", border = "green", add = TRUE)
#We will learn better ways to overlay two histograms.

#We might guess at relationships from a scatter plot
plot(NCB$Gestation, NCB$Weight)
Or we can split the data and plot the means
byGest<-split(NCB$Weight, NCB$Gestation) 
meanByGest <-lapply(byGest, mean); meanByGest
#R treatedthe number of weeks of gestation as a factor, not a number.
plot(as.numeric(names(meanByGest)), meanByGest, type = "b", xlab = "Gestation", ylab = "Mean Weight")

#Verizon Repair Times
Verizon<- read.csv("Verizon.csv"); head(Verizon)
#Here is another way to separate the data by factor
Verizon$Time[which(Verizon$Group == "CLEC")]  #small subset of non-Verizon customers
mean(Verizon$Time[which(Verizon$Group == "CLEC")]) #average 16 hour delay
mean(Verizon$Time[which(Verizon$Group == "ILEC")])  #significantly less for Verizon customers?

#General Social Survey
GSS<-read.csv("GSS2002.csv"); head(GSS)  #lots of columns
table(GSS$Politics)    #look at just one factor
table(GSS$Politics, GSS$OwnGun) #contigency table for two factors
#No numeric variables; lots of contingency tables and proportions

#Beer and Hot Wings - data from a bar in Minneapolis
BW <- read.csv("Beerwings.csv"); head(BW)
mean(BW$Beer[which(BW$Gender=="F")])
mean(BW$Beer[-which(BW$Gender=="F")])
#Do men drink significantly more beer?

#Black Spruce Seedlings - the result of a controlled experiment
BS <- read.csv("Spruce.csv"); head(BS)
plot(BS$Height0, BS$Height5)  #possible linear relationship?
cor(BS$Height0, BS$Height5)   #is this correlation significant?

#We can apply mean() to one column separately for each factor in a second column
tapply(BS$Ht.change, BS$Fertilizer, mean)  #does fertilizer help?
tapply(BS$Ht.change,  BS$Competition, mean)  #does competition hurt?
#We can even consider a pair of factors and make a table
tapply(BS$Ht.change, list(BS$Fertilizer, BS$Competition), mean)  #combination of factors