# Larson Hogstrom - Section 8
# MATH_E156 - 4/6/2014

# p. 202 - #9
Spruce <- read.csv('Spruce.csv'); head(Spruce)
hc <- Spruce$Ht.change
hist(hc)
qqnorm(hc) # discrete, normal
qqline(hc) # discrete, normal

# create exploratory plot to chekc the distribution of Ht.change
# find 95% t confidence interval for the mean height change ofver 
# the 5-year period of study and give a sentence interpreting your 
# interval
t.test(hc, conf.level=.95)
mean(hc)

## bootstrap t confidence interval
counter <- 0
plot(x =c(27,35), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
U.count <- numeric(72)
L.count <- numeric(72)
for (i in 1:1000) {
  x <-sample(hc,replace=TRUE)
  L <- mean(x) + qt(0.025, 71) * sd(x)/sqrt(72) #usually less than the true mean
  L.count[i] <- L
  U <- mean(x) + qt(0.975, 71) * sd(x)/sqrt(72) #usually greater than the true mean
  U.count[i] <- U
  if (L < 25 && U > 25) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = mean(hc), col = "red") #vertical line at true mean
counter/1000 #what fraction of the time did our confidence interval include the true mean?
mean(L.count)
mean(U.count)
hist( L.count, col=rgb(0,0,1,1/4),xlim=c(21,40))  # first histogram
hist( U.count, col=rgb(1,0,0,1/4),  add=T)  # second
