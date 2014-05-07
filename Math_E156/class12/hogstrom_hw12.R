# Larson Hogstrom - HW12
# MATH_E156 - 5/7/2014

# 1. Exercise 1 on page 240.
# calcium levels are normally distributed, mean=9.5 mg/day. SD unknown
# 20 new women surveyed x_bar =9.2, sample SD, s=1.1

# expected value
mu=9.5
# observed in 20 Women
xbar=9.2
s=1.1
n=20
t = (xbar-mu)/(s/sqrt(n)); t 
PValue <- pt(t, df = n-1); PValue

print("There is not enough evidence to support the claim that these women 
    have a different mean calcium level from the population")

# 2. Exercise 11 on page 241.
# Regional differences in the support for same-sex marraige
# 38% of 505 people form the midwest support same-sex mairrage compared to
# 31% of 773 from the South

n1 = 550
n2 = 773
X1 = n1 * .38
X2 = round(n2 * .31)
Location = c(rep("Midwest", n1), rep("South", n2))
Support = c(rep(TRUE, X1), rep(FALSE,n1-X1), rep(TRUE,X2), rep(F, n2-X2))
table(Location, Support) #display results as a contingency table
# use fisher's exact test
fisher.test(table(Location, Support))

print("Evidence supports a differnce in same-sex marriage support by location")

# 3. Exercise 21 on page 243. “Critical region” is defined on page 224.

# let X1,X2,...,X50 be a random sample for a Bernoulli distribution with
# unknown success probibility p. 
# Null Hypothesis p=.6
# alpha = .05
# Find the critical region for this test
qbinom(c(.025,.975),50,.6)
print("Values less than 24 or greater than 28 would be more extreme 
    at the alpha level of significance.")

# 4. Exercise 23 on page 243. Carry out a simulation to confirm your answers.
# a single measurment from distribution with pdf:
# f(x) = ae^(-ax) , x>0
# null hypotheis is that a=1
# null hypothesis is rejected if x>= 3.2

# a) calcluate the probability of commiting a Type I error
p_typeI <- 1-pexp(3.2,1); p_typeI

N<-10^4
false_pos_count <- 0
for (i in 1:N) {
    x <- rexp(1,1)
    if (x >= 3.2) false_pos_count <- false_pos_count +1
}
type_I_error <- false_pos_count/N; type_I_error

# b) calcluate the probability of commiting a Type II error if, in fact, a=1/5
1-pexp(3.2,.2,lower.tail = TRUE)

N<-10^4
false_neg_count <- 0
for (i in 1:N) {
    x <- rexp(1,.2)
    if (x < 3.2) false_neg_count <- false_neg_count +1
}
type_II_error <- false_neg_count/N; type_II_error

# 52.27