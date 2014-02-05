### Tutorial 1
fd <- read.csv("/Users/hogstrom/Documents/code/ModernKicks/Math_E156/extra_datasets/Data/FlightDelays.csv")
names(fd)
head(fd)
#The columns are the variables. There are two types of variables: 
#numeric, for example, FlightLength and Delay and factor 
#(also called categorical) (for example Carrier and DepartTime). The rows are called 
#observations or cases.
dim(fd)

#summary of variable 'Carrier'
table(fd$Carrier)
table(fd$FlightLength)
barplot(table(fd$Carrier))

#compare two categorical variables
table(fd$Carrier, fd$Delayed30)
hist(fd$Delay)

# numeric summaries
delay <- fd$Delay
mean(delay)
median(delay)
mean(delay,trim=.25)

# check that sd = sqrt(var)
sd(delay) == sqrt(var(delay))

#If you need the population variance 
#(that is, denominator of 1/n instead of 1/(n-1))
n <- length(delay)
(n-1)/n*var(delay)


tapply(delay,fd$Carrier,mean)
tapply(delay,fd$DepartTime,median)
boxplot(Delay ~ Day, data=fd)
tapply(delay, fd$DepartTime, summary)
boxplot(Delay ~ DepartTime, data = fd)
seq(0,3,by=.2)

x <- c(2, 0, -4) # combine ints into a sequence
w <- 6:10

which(x < 4)
index <- which(x < 4)
x[index]


delay <- subset(FlightDelays, select = Delay, drop = TRUE)
delay2 <- subset(fd,select=Delay,subset=Day != "Mon",drop=TRUE)

#obtain random sample without replacement:
sample(10,4)
sample(10,4,replace=TRUE)