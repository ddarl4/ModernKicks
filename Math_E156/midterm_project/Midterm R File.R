setwd("/Users/hogstrom/Documents/code/ModernKicks/Math_E156/midterm_project")
File2 <- read.csv("File2.csv")
NumberOfColumns <- length(colnames(File2)) - 1
ColumnIDs <- colnames(File2)
# NewColumnNames <- c("id", 1:NumberOfColumns)
# colnames(File2) <- NewColumnNames
Mean = 0
Variance = 1
randSample <- sample(ColumnIDs,1)
sigVec <- File2[,randSample]
sortVec <- sort(sigVec)
hist(sigVec, main = "", breaks = 100, xlim = c(-4, 4), probability = TRUE)
curve(dnorm(x, 0, 1), from = -4, to = 4, add = TRUE)
qqnorm(sigVec)
qqline(sigVec)

