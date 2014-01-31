#Math E-156 Script 0B-PlotTutorial.R
#This script accompanies the "Plots in R" Tutorial from the textbook's Web site
#It will save you the trouble of typing the individual commands

#High-Level Plot Functions
States03 <- read.csv("States03.csv")
barplot(table(States03$Region)) #close graphic or move the cursor to the next line
hist(States03$Poverty)

#To create a scatter plot, there are two approaches:
plot(States03$Unemp, States03$Poverty, xlab = "Unemployment", ylab = "Poverty")
#Move the cursor back into the script
plot(Poverty ~ Unemp, data = States03, xlab = "Unemployment", ylab = "Poverty")
hist(States03$Poverty, main = "Poverty", xlab = "percent",
xlim = c(0, 24), ylim = c(0, 20))
plot(1:19, 1:19, pch = 1:19, xlab = "x", ylab = "y")
pie(rep(1, 8), col = 1:8)

#Curves
curve(x^2, from = 0, to = 2)
curve(cos(x), from = 0, to = pi)
curve(cos(x), from = 0, to = pi, lty = 4, col = "red")

#Low-level functions
#First, create a high-level plot.
plot(Poverty ~ Unemp, data = States03, xlab = "Unemployment", ylab = "Poverty")
abline(v = mean(States03$Unemp), lty = 2) #vertical line at mean unemployment rate,
text(30, 18, "mean unemployment rate") #text at (30, 18)
title("Data from 2003")

#Another low-level example, but start with a high-level plot
plot(Poverty ~ ColGrad, data = States03, col = "blue", pch = 19, xlab = "College grad (%)",
ylab = "Poverty (%)")
points(Uninsured ~ ColGrad, data = States03, col = "red", pch = 19)
mtext("Percent uninsured", side = 4)
legend("bottomleft", legend = c("Y: Poverty","Y: Uninsured"), col = c("blue","red"),
pch = c(16, 16))

#Plotting symbols
range(States03$Poverty)
range(States03$ColGrad)
plot(Poverty ~ ColGrad, data = States03, pch=16, subset =Region=="West",xlim = c(15,40), ylim = c(5, 20))
points(Poverty ~ ColGrad, data = States03, pch=16, col = "red", subset = Region=="South")
points(Poverty ~ ColGrad, data = States03, pch=16, col = "green", subset = Region=="Northeast")
points(Poverty ~ ColGrad, data = States03,pch = 16, col = "blue", subset = Region=="Midwest")
legend("topright", legend=c("West", "South", "Northeast", "Midwest"),
pch = rep(16,4), col = c("black", "red", "green", "blue"))

#Overlaying a curve on a plot
curve(cos(x), from = 0, to = 2*pi)
curve(sin(x), add = TRUE, col = "blue", lty = 2)

#The par command
par(mfrow = c(2, 2)) #2x2 layout
curve(3*x^2)
curve(cos(x))
hist(States03$Population)
qqnorm(States03$Population)
qqline(States03$Population)
par(mfrow = c(1, 1)) #reset to default layout