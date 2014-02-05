

#### Part 1 ###
states <- read.csv('/Users/hogstrom/Documents/code/ModernKicks/Math_E156/class2/States03.csv')

# make a scatter plot of percentage of high school graduates against teacher 
# pay, with a solid red vertical line at mean teacher pay and 
# a dashed blue horizontal line at mean percentage of high school graduates.
plot(states$TeachersPay,states$HSGrad, ylab="HSGrad", xlab="TeachersPay")
abline(v = mean(states$TeachersPay), lty= 1, col = "red")  #red vertical line at mean teacher pay
abline(mean(states$HSGrad),0, lty=2, col="blue") # a dashed blue horizontal line at mean percentage of high school graduates.
title("US - States Data from 2003")

### Part 2 ###

#import Black Spruce

a) compute the numeric summaries for the height change Ht.Change of the seedlings

b) make histogram and normal quantile plot for the height changes
of the seedlings. Is the  distribution appprox. normal?

c) make boxplot - change in diameter (Di.change) groupbed by plot fertilize status

d) use tapply to find the numeric summaries of the diameter changes for 
the two levels of fertilization

e)





