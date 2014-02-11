#1-Case Studies
#Shows the data sets that are introduced in Chapter 1.
#Flight delays out of La Guardia
Delays<-read.csv("FlightDelays.csv")
dim(Delays)   #lots of observations
head(Delays)   #sneak a peek
barplot(table(Delays$Destination))  # factor variable
table(Delays$Day, Delays$Delayed30) #a contingency table from two factors
mean(Delays$Delay)
median(Delays$Delay)   #must be skewed
hist(Delays$Delay, breaks = "FD")   #long tail

#Birth Weights of babies
NCB<- read.csv("NCBirths2004.csv");head(NCB)
hist(NCB$Weight, breaks = "FD", prob = TRUE)       #not badly skewed
#Compare our sample with a probability model for the population
curve(dnorm(x, mean(NCB$Weight), sd(NCB$Weight)), col = "red", add=TRUE) #overlay a normal distribution
#There are lots of ways to split up these data
byGender<-split(NCB$Weight, NCB$Gender) #creates a list of two vectors
lapply(byGender, mean)
hist(byGender$Male, breaks = "FD", main = "Histogram by Gender")
hist(byGender$Female, breaks = "FD", border = "green", add = TRUE)
#We might guess at relationships from a scatter plot
plot(NCB$Gestation, NCB$Weight)
Or we can split the data and plot the means
byGest<-split(NCB$Weight, NCB$Gestation) 
meanByGest <-lapply(byGest, mean); meanByGest
plot(as.numeric(names(meanByGest)), meanByGest, type = "b", xlab = "Gestation", ylab = "Mean Weight")

#Verizon Repair Times
Verizon<- read.csv("Verizon.csv"); head(Verizon)
#Here is another way to separate the data by factor
Verizon$Time[which(Verizon$Group == "CLEC")]  #small subset
mean(Verizon$Time[which(Verizon$Group == "CLEC")])
mean(Verizon$Time[which(Verizon$Group == "ILEC")])  #significant?

#General Social Survey
GSS<-read.csv("GSS2002.csv"); head(GSS)
table(GSS$Politics)
table(GSS$Politics, GSS$OwnGun)
#No numeric variables; lots of contingency tables and proportions

#Beer and Hot Wings
BW <- read.csv("Beerwings.csv"); head(BW)
mean(BW$Beer[which(BW$Gender=="F")])
mean(BW$Beer[-which(BW$Gender=="F")])
#Do men drink significantly more beer?

#Black Spruce Seedlings
BS <- read.csv("Spruce.csv"); head(BS)
plot(BS$Height0, BS$Height5)  #possible linear relationship?
cor(BS$Height0, BS$Height5)   #is this correlation significant?
tapply(BS$Ht.change, BS$Fertilizer, mean)  #does fertilizer help
tapply(BS$Ht.change,  BS$Competition, mean)  #does competition hurt?
tapply(BS$Ht.change, list(BS$Fertilizer, BS$Competition), mean)  #combination of factors