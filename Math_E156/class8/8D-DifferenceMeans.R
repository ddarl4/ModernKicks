#10-DifferenceMeans
#Example 7.8 from the textbook
Reading <- read.csv("Reading.csv"); head(Reading)
#Extract vectors for the two groups
treated <- subset(Reading, select = Response, subset = (Treatment == "Treated"), drop = TRUE)
control <- subset(Reading, select = Response, subset = (Treatment == "Control"), drop = TRUE)
M1<-mean(treated); M2<-mean(control); M1; M2 #is this difference significant?
n1 <- length(treated); n2 <- length(control); n1; n2 #not the same size
#We do not know the variances but can calculate the sample standard deviations
S1<- sd(treated); S2<- sd(control); S1; S2
#If the groups are independent and the distributons are normal, we know the standard error of the differnce of means
S <- sqrt(S1^2/n1 + S2^2/n2); S
#Welch's approcmation gives a reasonable value for degrees of freedom
n <- (S1^2/n1 + S2^2/n2)^2/(S1^4/(n1^2*(n1-1)) + S2^4/(n2^2*(n2-1))); n
#Get the quantiles for the t distribution with this many degrees of freedom
tqnt<- qt(c(.025, .975),n); tqnt
#Multiply by the sample standard deviation to create a 95% confidence interval
interval<- (M1-M2)+ S*tqnt; interval
#Of course, this has all been automated
t.test(treated, control)