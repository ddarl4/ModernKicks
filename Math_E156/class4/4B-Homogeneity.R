#Math E-156 Script 4B-Homogeneity.R
#Based on the example of Section 3.6 of the textbook

#Topic 1 -- creating a contingency table and doing a chi-square test
#We can create contingency table 3.7 in the textbook directly from the counts
candy.mat<-rbind(c(42,20,38),c(33,27,50))    #enter the counts in the cells
rownames(candy.mat) <- c("Boys", "Girls")    #add row names
colnames(candy.mat) <- c("Flavor 1", "Flavor 2", "Flavor 3")   #add column names
candy.mat        #looks like table 3.7
chisq.test(candy.mat)    #P-value is same as bottom of page 62
#We can get the same chi square value from our usual chi square function
chisq <-function(Obs){
  Expected <- outer(rowSums(Obs), colSums(Obs)/sum(Obs))
  sum((Obs-Expected)^2/Expected)
}
observed <- chisq(candy.mat); observed

#To simulate the actual survey, create a data frame with 210 rows, each specifying preference and gender
Preference<-c(rep("Flavor1",75),rep("Flavor2",47),rep("Flavor3",88))
Gender<-c(rep("Boys",42),rep("Girls",33),rep("Boys",20),rep("Girls",27),rep("Boys",38),rep("Girls",50))
Candy<-data.frame(Preference,Gender);head(Candy)
Table3.7<-table(Candy$Gender,Candy$Preference);Table3.7 #creates Table 3.7 from the textbook
chisq.test(Table3.7)   #P-value is 0.193


#Topic 2 -- doing a permutation test with a chi-square statistic
#Let's do a simulation to determine whether or not the gender discrepancy could arise by chance.
#If we permute the gender column, the row sums and column sums will not change.
Gender.perm<-sample(Candy$Gender);head(Gender.perm)  #default sample size is the length of the vector
#As usual, first lookat the result of a single permutation
table(Gender.perm,Candy$Preference)   #this table could have arisen by chance
chisq(table(Gender.perm,Candy$Preference))  #we can inspect the chi square statistic
#Now just generate lots of permutations and tally the results
N = 10^4 -1; result <- numeric(N)
for (i in 1:N) {
  Gender.perm<-sample(Candy$Gender)
  result[i]<-chisq(table(Gender.perm,Candy$Preference))
}
hist(result)
abline(v = observed, col = "red")
pValue = (sum(result >= observed)+1)/(N+1);pValue    #the built-in test gave 0.193
#Why are the results so close? -- overlay the chi square density function
hist(result,25,probability = TRUE)
curve(dchisq(x, 2), add = TRUE, col = "red") 

