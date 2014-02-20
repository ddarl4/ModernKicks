#Math E-156 4X-Hypergeometric.R

Topic 1 - some fabricated examples witth small numbers
#A bag contains 6 Munchkins, 2 chocolate,4 plain
#Lisa extracts 3 and puts them in a bag for Thomas
#What are the probabilities for 0, 1, 2 chocolate Munchkins?
dhyper(0,2,4,3)
dhyper(1,2,4,3)
dhyper(2,2,4,3)
dhyper(0:2,2,4,3)
#A bag contains 30 Munchkins, 10 chocolate,20 plain
#Lisa extracts 15 and puts them in a bag for Thomas
#What are the probabilities for 0:10 chocolate Munchkins?
chocs<-dhyper(0:10,10,20,15);barplot(chocs)
#Suppose Thomas gets 8 chocolate: is that an unequal distribution?
phyper(7,10,20,15,lower.tail = FALSE) #probability of > 7
phyper(2,10,20,15,lower.tail = TRUE) #probability of <=2
#So the probability of such an unequal division is about 5%
#We can make the entire contingency table
Munchkins.mat<-rbind(c(8,7),c(2,13));Munchkins.mat
chisq.test(Munchkins.mat)# pretty good agreement
fisher.test(Munchkins.mat,alternative = "greater") #does the hypergeometric calculation
#Now we can simulate the distribution of Munchkins
Flavor <- c(rep("Chocolate",10),rep("Plain",20))
Child <- c(rep("Thomas",8),rep("Catherine",2),rep("Thomas",7),rep("Catherine",13))
Munchkin<- data.frame(Flavor,Child); head(Munchkin)
table(Munchkin$Flavor,Munchkin$Child)
#Now do the usual 10000 samples
#As a statistic, use the number of chocolate Munchkins that Thomas gets
N = 10^4-1
result = numeric(N)
for (i in 1:N) {
  Flavor.perm = sample(Munchkin$Flavor)
  result[i] = sum(Flavor.perm == "Chocolate" & Munchkin$Child == "Thomas")
}
barplot(table(result)) #looks mighty hypergeometric
Pvalue <- (sum(result >=8)+1)/(N+1); Pvalue #should be close to 0.025
#This is a one-sided test: did Thomas get too many chocolate Munchkins?


#Topic 2 -- some real data: do Bush voters own guns?
GSS<-read.csv("GSS2002.csv")
head(GSS)
#Extract the subset who respond about gun ownership and voted for Bush or Gore
#Use factor() to get rid of the unwanted levels
Gun<-factor(subset(GSS,select = OwnGun,subset = ((OwnGun=="Yes")|(OwnGun=="No")) & ((Pres00=="Bush")|(Pres00=="Gore")),drop=TRUE))
Pres<-factor(subset(GSS,select = Pres00,subset = ((OwnGun=="Yes")|(OwnGun=="No")) & ((Pres00=="Bush")|(Pres00=="Gore")),drop=TRUE))
addmargins(table(Gun,Pres)) #include the row and column sums
#Now we can find the probability that the 297 Bush voters would own at least 141 of the 210 guns
#Bush is like Thomas, Yes is like Chocolate, No is like Plain
phyper(140, 210,347,297,lower.tail = FALSE)
#This test is built ito R: it tests the first column, which is "No"
fisher.test(table(Gun,Pres),alternative = "less")
#We could also do a chi square test to see if the distribution of guns is unequal
chisq.test(table(Gun,Pres)) #p-value should be roughly twice as large

#Alternatively we can simulate the distribution of guns
GunData<- data.frame(Gun,Pres); head(GunData)
#Now do the usual 10000 samples
#As a statistic, use the number of guns in the hands of Bush voters
N = 10^4-1
result = numeric(N)
for (i in 1:N) {
  Gun.perm = sample(GunData$Gun)
  result[i] = sum(Gun.perm == "Yes" & GunData$Pres == "Bush")
}
barplot(table(result)) #looks mighty hypergeometric
Pvalue <- (sum(result >=141)+1)/(N+1); Pvalue #likely to be 1 in 10000
fisher.test(table(Gun,Pres),alternative = "less")
