#Math E-156 Script 12A--OneSampleMeansTest

#Topic 1 -- hypothesis test for a mean

#Example 8.1 in the textbook -- SAT scores
#Null hypothesis - we have a sample from a population with N(mu, sigma)
#Alternative: our sample is from a population with a different mean

#Assume a normal distribution with the national mean and standard deviation
mu <- 515; sigma <- 116   

#For our sample, we know only the mean and the sample SD
xbar <- 555; s <- 120; n = 25        #data for our sample
#First approach, assuming sigma is correct for our sample
#Create a z statistic whose distribution is N(0,1)
#In the days of printed statistical tables, this was a good idea.
z = (xbar-mu)/(sigma/sqrt(n)); z 

#Calculate the probability of a sample mean as extreme as what we observed
PValue <- pnorm(z, lower.tail = FALSE); PValue

#With R, there is no longer need for explicit normalization
PValue <- pnorm(xbar, mean = mu, sd = sigma/sqrt(n), lower.tail = FALSE); PValue

#We can illustrate our conclusion with a graph of the normal density
#Here is a heavy-handed way to shade some of the area
xpoints <- c(xbar,seq(xbar,650,1),650) #define polygon to shade
ypoints <- c(0,dnorm(seq(xbar,650,1), mu, sigma/sqrt(n)),0) 
curve(dnorm(x, mu, sigma/sqrt(n)), from = 400, to = 650)
abline(v = xbar, col = "red")
polygon(xpoints,ypoints,col="skyblue")

#Alternatively, we can assume that we do not know sigma
#If we want to use our sample standard deviation, we have a t statistic
#Standardize the data, using s instead of the national sigma
t = (xbar-mu)/(s/sqrt(n)); t 
PValue <- pt(t, df = n-1, lower.tail = FALSE); PValue

#We can illustrate this approach with a graph of the t density
xpoints <- c(xbar,seq(xbar,650,1),650) #define polygon to shade
ypoints <- c(0, dt((seq(xbar,650,1)- mu)/ (s/sqrt(n)), df = n-1),0) 
curve(dt((x-mu)/(s/sqrt(n)), df = n-1), from = 400, to = 650)
abline(v = xbar, col = "red")
polygon(xpoints,ypoints,col="skyblue")

#Topic 2 -- hypothesis test for a proportion, using a binomial distribution
#Example 8.3 from the textbook
#Population model: 13% of the population are lefties
#Sample data: 36 of 200 scientists (18%) are left-handed
#Test the probability of such an extreme result under the null hypothesis

#This is most easily done using the distribution function
pbinom(35, 200, 0.13, lower.tail = FALSE)  #evaluates P(x > 35)

#Alternative: use the mass function, as on page 214
pvalue <-sum(dbinom(36:200, 200, 0.13)); pvalue 

#For this example we could also use a bootstrap approach
#Simulate the population with 200 rows
data <- c(rep("L", 26), rep ("R", 174)); data
counter <- 0; N <- 10000;  lefties <- numeric(N)

#Check our sampling methodology before doing the loop
x <-sample( data, 200, replace = TRUE)
sum(x == "L")  #number of lefties in the bootstrap sample

for (i in 1:N) {
  x <- sample(data, 200, replace = TRUE)
  lefties[i] <- sum(x == "L")
  if (lefties[i] >= 36) counter <- counter+1
}
hist(lefties, breaks = seq(0.5, 50.5, by = 1))
abline(v = 35.5, col = "red")
counter/N; pvalue     #should agree closely with the thoeretical value

#Topic 3 -- hypothesis test for a proportion, using a binomial distribution
#The old-fashioned way (see remark on page 214)
#Calculating the binomial distribution function used to be a lot of work.
#We can try approximating by a normal distribution.
p = 0.13; sigma = sqrt(p*(1-p)/200); sigma
z = (35.5/200 - p)/sigma; z    #standardize; 35.5 does a continuity correction
pnorm(z, lower.tail = FALSE)   #quite different from binomial or bootstrap

#This inferior version is what has been automated
prop.test(36, 200, 0.13, alternative = "greater")
#Same number, but why does chi-square get mentioned?
#Answer: z is N(0,1); so z^2 is chi-squared with 1 degree of freedom
pchisq(z^2, df =1, lower.tail = FALSE)/2   #divide by 2 to do a one-sided test

