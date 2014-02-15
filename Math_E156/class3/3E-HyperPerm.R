#Math E-156 Script 3E-HyperPerm.R   Appendix B.5, page 378
#Uses the hypergeometric distribution to get the result of a permutation test
choose(18,6)   #a manageable number of permutations
M <-6 #six women
N <-12 #12 men
n <-6  #choose committee of 6
Gender <- c(rep("Woman",M),rep("Man",N))
Member <- c("On", rep("Off",M+N-n), rep("On", n-1))
Students <- data.frame(Gender,Member); Students
table(Gender,Member)

#Approach 1 - an exact permutation test
AllSubsets<-combn(1:18,6)   #all ways to select 6 of 18
NPerm<-ncol(AllSubsets); NPerm;  members <-numeric(NPerm); #create empty vector of the right length.
#Now deal with each subset the same way that we dealt with the three drugged mice
for (i in 1:NPerm) {
  index=AllSubsets[,i]
  members[i]=sum(Gender[index] == "Woman")
}
barplot(table(members))
pValue = mean(members <= 1); pValue

#Approach 2 - the hypergeometric distribution
barplot(dhyper(0:6,6, 12, 6),names.arg = 0:6)
pValue = phyper(1,6,12,6); pValue

#Approach 3 - the Fisher exact test
#Probability of getting 1 or fewer women by chance
fisher.test(table(Gender,Member),alternative = "less")$p.value
#Probability that the numer of women will differ from expectation by at least 1
fisher.test(table(Gender,Member),alternative = "two.sided")$p.value

#Approach 4 - the chi-square test
#Since this test squares the difference, it can only do a 2-sided test
chisq.test(table(Gender,Member))$p.value