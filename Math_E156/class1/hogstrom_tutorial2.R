### Tutorial 1
states <- read.csv('/Users/hogstrom/Documents/code/ModernKicks/Math_E156/class2/States03.csv')
States03 <- states

barplot(table(states$Region))
hist(states$Poverty)

plot(states$Unemp,states$Poverty, xlab="Unemployment")
plot(Poverty ~ Unemp, data=states)

hist(states$Poverty, main='Poverty',xlim = c(0, 24), ylim = c(0, 20))
plot(1:19, 1:19, pch=1:19, xlab='x',ylab='y')

plot(Poverty ~ Unemp, data = States03, xlab = "Unemployment", ylab = "Poverty")
abline(v = mean(States03$Unemp), lty = 2)  #vertical line at mean unemployment rate,
text(30, 18, "mean unemployment rate")
title("Data from 2003")

plot(Poverty ~ ColGrad, data = States03, col = "blue", pch = 19, xlab = "College grad (%)",
   ylab = "Poverty (%)")
points(Uninsured ~ ColGrad, data = States03, col = "red", pch = 19)
mtext("Percent uninsured", side = 4)
legend("bottomleft", legend = c("Y: Poverty","Y: Uninsured"), col = c("blue","red"),
pch = c(16, 16))


range(States03$Poverty)
range(States03$ColGrad)
plot(Poverty ~ ColGrad, data = States03, pch=16, subset =Region=="West", xlim = c(15,40), ylim = c(5, 20))
points(Poverty ~ ColGrad, data = States03, pch=16, col = "red", subset = Region=="South")
points(Poverty ~ ColGrad, data = States03, pch=16, col = "green", subset = Region=="Northeast")
points(Poverty ~ ColGrad, data = States03,pch = 16, col = "blue", subset = Region=="Midwest")
legend("topright", legend=c("West", "South", "Northeast", "Midwest"),
   pch = rep(16,4), col = c("black", "red", "green", "blue"))


curve(cos(x), from = 0, to = 2*pi)
curve(sin(x), add = TRUE, col = "blue", lty = 2)    

# par command
par(mfrow = c(2, 2))   #2x2 layout
curve(3*x^2)
curve(cos(x))
hist(States03$Population)
qqnorm(States03$Population)
qqline(States03$Population)
par(mfrow = c(1, 1))

### save plot 
postscript(file = "MyPlot.eps")    #open graphics device
hist(States03$Births, main = "Number of births")  #create graph
dev.off() 


