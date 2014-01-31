#Math E-156 Script 0A-StartTutorial.R
#This script accompanies the "Getting Started with R" Tutorial from the textbook's Web site
#It will save you the trouble of typing the individual commands

#Simple arithmetic
4*9 
3+2*(8                 #incomplete expression
-4)                    #completion
dog <- 1:5
dog
dog + 10
3*dog
sum(dog) # 1+2+3+4+5
?sum		           #brings up online help

#Importing Data
#This assumes that when you installed R, you set up the shotcut to start in the Data subfolder of your Math E-156 folder
#See page 6 of the tutorial
FlightDelays <- read.csv("FlightDelays.csv")  #complete path is not needed
names(FlightDelays)
head(FlightDelays)
dim(FlightDelays)
table(FlightDelays$Carrier)
barplot(table(FlightDelays$Carrier))  #have to close the graphic or move the cursor back into the script
hist(FlightDelays$Delay)
delay <- FlightDelays$Delay
mean(delay)
median(delay)
mean(delay, trim = .25)

max(delay)
min(delay)
range(delay)
var(delay) #sample variance (unbiased estimate of population variance)
sd(delay) #standard deviation
quantile(delay) #quartiles
#If you need the population variance (that is, denominator of 1/n instead of 1/(n-1)),
n <- length(delay)
(n-1)/n*var(delay)  #population variance

tapply(delay, FlightDelays$Carrier, mean)
tapply(delay, FlightDelays$DepartTime, median)

summary(delay) #numeric summary
boxplot(delay)
#To compare the distribution of a numeric variable across the levels of a factor variable
tapply(delay, FlightDelays$Day, summary)
boxplot(Delay ~ Day, data = FlightDelays)
tapply(delay, FlightDelays$DepartTime, summary)
boxplot(Delay ~ DepartTime, data = FlightDelays)

fish25 <- 10:35; fish25 #two commands on one line is OK
whale
whale <- 200
objects()
rm(whale)
objects()
 In general, R is space-insensitive.
3 +4
3+ 4
mean(3+5)
mean ( 3 + 5 )
q()     #choose Cancel
#You should already have done the next section (Workspace Management)

1:10
5:-3

seq(0, 3, by = .2)
seq(0, 3, length = 15)
quantile(delay, seq(0, 1, by = .1)) #deciles of delay
#To create vectors with no particular pattern, use the c() function (c for "combine" or "concatenate").
c(1, 4, 8, 2, 9)
x <- c(2, 0, -4)
x
c(x, 0:5, x)

c("a", "b", "c", "d")
#logical values (note that there are no double quotes):
c(T, F, F, T, T, F)
#The rep() command for repeating values:
rep("a", 5)
rep(c("a", "b"), 5)
rep(c("a", "b"), c(5, 2))
#The \class" attribute
#Use data.class to determine the class attribute of an object.
state.name
data.class(state.name)
state.name == "Idaho"
data.class(state.name == "Idaho")
head(FlightDelays$Carrier)
data.class(FlightDelays$Carrier)
#Basic Arithmetic
x <- 1:5
x - 3
x*10
x/10
x^2
2^x
log(x)

w <- 6:10
w
x*w

#Logical expressions
x < 3
x == 4

z <- c(8, 3, 0, 9, 9, 2, 1, 3)
z[4]
z[c(1, 3, 4)]
z[-c(1, 3, 4)]
which(z < 4) # which positions are z values < 4?
index <- which(z < 4) # store in index
z[index] # return z[c(2, 3, 6, 7)]

delay <- subset(FlightDelays, select = Delay, drop = TRUE)
subset(FlightDelays, select = Delay, drop = TRUE) # a vector

subset(FlightDelays, select = Delay) # a data frame

delay2 <- subset(FlightDelays, select = Delay, subset = Day != "Mon",drop = TRUE)
mean(delay2)

delay3 <- subset(FlightDelays, select = Delay,
subset = (Day == "Sat" | Day == "Sun"), drop = TRUE)
mean(delay3)

index <- which(delay > mean(delay))
head(index)

length(z) # number of elements in z
sum(z) # add elements in z
sort(z) # sort in increasing order
sample(10)
sample(10, 4)
sample(10, 4, replace = TRUE)  #may include repeated values
state.name
sample(state.name, 20)
sample(state.name, 20, replace=TRUE)

index <- sample(50, 20)
index
tempA <- state.name[index]
tempB <- state.name[-index]
tempA
tempB

#Data Frames in R
data.class(FlightDelays)
FlightDelays[5, 3]
FlightDelays[1:10, c(1, 3)]
FlightDelays[-(1:10), c(1, 3)]
FlightDelays[, c(1, 3)]
FlightDelays[1:100, ]
DelaysTue <- subset(FlightDelays, subset = Day == "Tue")
head(DelaysTue)
DelaysTue <- subset(FlightDelays, Day == "Tue", select = c(1, 6, 7))
head(DelaysTue)


