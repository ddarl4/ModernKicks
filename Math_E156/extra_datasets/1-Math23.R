#This data set was created by deleting irrelevant columns and rows from an Excel file, then saving it in .csv format.
Math23<-read.csv("Math23.csv")
head(Math23)
#Yard is a categorical(factor) variable. Explore its levels:
table(Math23$Yard) 
#It can be viewed as a partition of the sample space into three disjoint events, Y1, Y2, Y3
#Explore Section the same way -- a six-way partition
table(Math23$Section) #the counts are a vector that can be plotted
barplot(table(Math23$Section))
#With two categorical variables we can make a contingency table
table(Math23$Yard,Math23$Section)
#The entries in this table refer to the intersection of two events like Y1 and S2
#Variable HW is a numeric variable. We can have a look at it
hist(Math23$HW,breaks="fd") #fd computes number of bins
hist(Math23$HW,breaks="scott") #different algorithm for number of bins
#Compute various statistics, but first extract a vector with a shorter name
hw<-Math23$HW
mean(hw)
median(hw)
max(hw)
min(hw)
range(hw)
var(hw) #variance
sd(hw) #standard deviation
#To combine these statistice with a categorical variable, use tapply
tapply(hw, Math23$Yard, mean) #is Ivy really better?
tapply(hw, Math23$Section, median)
#View the experiment as "choose one of these students at random."
#Extract events (subsets) involving random variable HW
subset(Math23, subset = HW < 37) #this is a data frame
#Compute frequency of various events involving random variable HW
#Careful -- length gives the number of rows only if we pass it a vector
length(subset(Math23,select= HW, subset = HW < 37, drop = TRUE))
#Function nrow works on a data frame
nrow(subset(Math23, HW < 37)) #default secondargument is subset

#A shorter alternative
which(hw < 37) #get a vector of row numbers
length(which(hw < 37))/length(hw) # a probability

#Forming the union of two events using |
subset(Math23,Final > 70 | Section == "Dennis")
#Forming the intersection of two events using &
subset(Math23, Final < 60 & HW > 38)

#Calculating a conditional probability P(Final > 65 | Proofs > 23)
nrow(subset(Math23,Final > 65 & Proofs > 23))/nrow(subset(Math23, Proofs > 23))

#Finding the top student
max(Math23$Final) #get the top exam score
which.max(Math23$Final)#get the row number for the top score

Math23[which.max(Math23$Final),] #display the row


#Scatter plots to compare numeric variables
attach(Math23) #now we can just use column names
plot(HW, Proofs, xlab = "Homework", ylab = "Proofs")
plot(HW, Final, xlab = "Homework", ylab = "Final Exam")
plot(Proofs, Final, xlab = "Proofs", ylab = "Final Exam")
detach(Math23) #in case we want to reuse a column name
