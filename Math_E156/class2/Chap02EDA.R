#Chap 2: Exploratory Data Analysis


#Chapter 2.4
 x <- c(21.7, 22.6, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
 qqnorm(x)   # plot points
 qqline(x)   # add straight line
 
 qqnorm(NCBirths$Weight)
 qqline(NCBirths$Weight)

#Chapter 2.5 
#R Note
x <- c(3, 6, 15, 15, 17, 19, 24)
 plot.ecdf(x)
 x <- rnorm(25)              # random sample of size 25 from N(0,1)
 plot.ecdf(x, xlim = c(-4, 4))             # adjust x range
 curve(pnorm(x), col = "blue", add = TRUE) # impose normal cdf
 
 beerM <- subset(Beerwings, select = Beer, subset = Gender == "M",
            drop = T)
 beerF <- subset(Beerwings, select = Beer, subset = Gender == "F",
           drop = T)

 plot.ecdf(beerM, xlab = "ounces")
 plot.ecdf(beerF, col = "blue", pch = 2, add = TRUE)
 abline(v = 25, lty = 2)
 legend(5, .8, legend = c("Males", "Females"),
    col = c("black", "blue"), pch = c(19, 2))

#--------------------------
