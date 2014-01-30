#Math E-156 Script 1E-Math23.R
#This data set was created by deleting irrelevant columns and rows from an Excel file, then saving it in .csv format.
Math23<-read.csv("Math23.csv"); head(Math23)

#Yard is a categorical(factor) variable. Explore its levels:
table(Math23$Yard) 
#It can be viewed as a partition of the sample space into three disjoint events, Y1, Y2, Y3

#Explore Section the same way -- a six-way partition
table(Math23$Section) #the counts are a vector that can be plotted
barplot(table(Math23$Section))  #appropriate for factors

#With two categorical variables we can make a contingency table
table(Math23$Yard,Math23$Section)
#The entries in this table refer to the intersection of two events like Y1 and S2

#Variable HW is a numeric variable. We can have a look at it
hist(Math23$HW, breaks = "FD") 

#Compute various statistics, but first extract a vector with a shorter name.
hw<-Math23$HW   #a numeric vector, not a data frame
max(hw)
min(hw)
range(hw)   #get min and max both at once
mean(hw)    #this is a sample mean, not an expectation
median(hw)  #if data were not skewed, would be close to mean

var(hw) #estimate of population variance
sd(hw); sd(hw)^2  #standard deviation; square root of variance

#To break down one of these statistics over a categorical variable, use tapply()
tapply(hw, Math23$Yard, mean) #but is Crimson significantly worse?
tapply(hw, Math23$Section, median) #perhaps Weijun is a more lenient grader

#Now we will treat this as if it were a probability data frame!
#The experiment is "choose one of these students at random; all equally likely."
#Extract events (subsets) involving random variable HW
subset(Math23, subset = HW < 37) #this produces another data frame but keeps the original row numbers

#Compute frequency of various events involving random variable HW.
#Careful -- length() gives the number of rows only if we pass it a vector, not a data frame
length(subset(Math23, select= HW, subset = HW < 37, drop = TRUE)) #drop = TRUE gives us a vector

#Function nrow works on a data frame
nrow(subset(Math23, HW < 37)) #default second argument is subset

#Another way to find the probabiliity of an event
which(hw < 37) #create the subset as a vector of row numbers
length(which(hw < 37))/length(hw) #probability of the event

#Forming the union of two events, using "vectorzed or" (operator |)
subset(Math23, Final > 70 | Section == "Dennis")

#Forming the intersection of two events, using "vectorzed and" (operator &)
subset(Math23, Final < 60 & HW > 38)

#Calculating a conditional probability, P(Final > 65 | Proofs > 23)
nrow(subset(Math23,Final > 65 & Proofs > 23))/nrow(subset(Math23, Proofs > 23))

#Finding the top student
max(Math23$Final) #get the top exam score
Math23[which.max(Math23$Final),] #display the row that contains the top score

#Scatter plots to compare numeric variables
attach(Math23) #now we can just use column names
plot(HW, Proofs, xlab = "Homework", ylab = "Proofs")
plot(HW, Final, xlab = "Homework", ylab = "Final Exam")
plot(Proofs, Final, xlab = "Proofs", ylab = "Final Exam")
detach(Math23) #in case we want to reuse a column name
