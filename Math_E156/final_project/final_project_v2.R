##  Musings on the final

red = rgb(1,0,0,1/3)
green = rgb(0,1,0,1/3)
blue = rgb(0,0,1,1/3)
##  Just instantiating my favorite colors to graph with.


##  First I import the data I wish to use.
data.temp = read.csv("finaldata.csv",stringsAsFactors = FALSE)
##  Needed the additional command so numbers wouldn't become
##  factors; it's easier to convert strings to numerics.

names(data.temp)
##  Checking out my headers.

##  With the data imported, I subset to get the columns I want,
##  in a format I want to deal with.

data = subset(data.temp,select = c("cell.ID","pharm_id","subgroup",
      "Conc.","Exposed.time","cc","ss","is_gold","deltaSS","deltaCC",
      "missing.6","deltaConc","SSConc"))

##  I want a data file with drug concentration fixed at 10
##  micromolar (by far the mode)
index = (data$Conc. == 10)
data.10 = data[index,]

##  I want also created two fields that were differences in
##  observations when an experiment ran for both 6 and 24
##  hour durations.  Anything with a numeric instead of a
##  FALSE value represents that data.
index = (data$deltaSS != 0)
delta = data[index,]

nrow(delta)
##  Confirming I've shrunk the dataset.

index = (delta$Exposed.time ==6)
delta.6 = delta[index,]
delta.24 = delta[-index,]

nrow(delta.6)  ##  Should be half the rows in delta.

head(delta)

##  More subsetting to split the delta file into
##  10 and not-10 micromolar experiments

index = (delta.6$Conc. == 10)
delta.6.10 = delta.6[index,]

index = (delta.6$Conc. != 10)
delta.6.n10 = delta[index,]

##  And now to begin :-)


##  I had initially graphed things wrong.  I had done the
##  whole data set in red and the 10mu in blue.  Not
##  Surprisingly, almost all the data intersected, but I
##  came up with of dealing with that without making two
##  different graphs.  Just shift one of the data sets by
##  a bit and give a second axis.

par(mfrow = c(1,1))
plot(delta.6.10$deltaSS~delta.6.10$ss,xlim = c(.5,19),pch = 16,
     col = red,ylim = c(-6,14),xlab = "6 hour SS",
     ylab = "Delta SS (to 24)",main = "SS from 6 to 24 hours")
temp = delta.6.n10$ss + 8
points(delta.6.n10$deltaSS+6~temp,xlim = c(0,20),pch = 16,col = blue)
abline(h = c(0,6),col = c("red","blue"))
abline(v = c(0,8),col = c("red","blue"))

##  Anyway, it seems like a useful way to compare two similar data
##  sets without having to squish the data on two very short
##  graphs.

##  Using the right data sets, overlaid data:

par(mfrow = c(1,1))
plot(delta.6.10$deltaSS~delta.6.10$ss,xlim = c(0,20),pch = 16,
     col = red,ylim = c(-6,14),xlab = "6 hour SS",
     ylab = "Delta SS (to 24)",main = "SS from 6 to 24 hours")
points(delta.6.n10$deltaSS~delta.6.n10$ss,xlim = c(0,20),
       pch = 16,col = blue)
abline(h = 0)
abline(v = 0)

legend("topright",c("10","Other"),col = c(red,blue),pch = 16,
       title = "Concentrations")


cor(delta.6.10$ss,delta.6.10$deltaSS)
##  like no correlation.

cor(delta.6.n10$ss,delta.6.n10$deltaSS)
##  but there is a moderate amount of correlation when we
##  look at the things that aren't 10 micromolar

abline(v = c(1,1.8,2))
##  I'm guessing that someone has drawn the line bounding the
##  the top of the data, and the line bounding the bottom
##  of the data, and at their nexus near the x-axis, it's
##  been decided that the change in SS over 18 subsequent
##  won't be worth testing


par(mfrow = c(1,1))
plot(delta.6.10$deltaSS~delta.6.10$cc,pch = 16,
     col = red,xlab = "6 hour CC",
     ylab = "Delta SS (to 24)",main = "SS from 6 to 24 hours")

points(delta.6.n10$deltaSS~delta.6.n10$cc,xlim = c(0,20),
       pch = 16,col = blue)
abline(h = 0)
legend("topright",c("10","Other"),col = c(red,blue),pch = 16,
       title = "Concentrations")

cor(delta.6.10$cc,delta.6.10$deltaSS)
##  Nothing.

cor(delta.6.10$ss,delta.6.10$deltaCC)
##  Nope.


##


cor(delta.6.10$cc,delta.6.10$deltaCC)
##  Interesting
par(mfrow = c(1,1))
plot(delta.6.10$deltaCC~delta.6.10$cc,pch = 16,
     col = red,xlab = "6 hour CC",
     ylab = "Delta CC (to 24)",main = "CC from 6 to 24 hours")
points(delta.6.n10$deltaCC~delta.6.n10$cc,xlim = c(0,20),
       pch = 16,col = blue)
abline(h = 0)
legend("topright",c("10","Other"),col = c(red,blue),pch = 16,
       title = "Concentrations")


##  Run a linear model and test it with the data.  There may be a
##  band of values, but maybe there's correlation mixed in there.

##  wait a second ...
curve(1-x,add = TRUE)

##  Correlation is bounded above by 1, so it should
##  be below the preceding two black line and the
##  y-axis.  This correlation looks like artifact of the var-
##  iable and the constraints that exist on it.

##  It's hard to tell if there's something meaningful here, or if
##  it is artifact of CC having to hit a .2 cutoff.  I also
##  wonder if those blue points outside the bounds are actually
##  negative correlations that have been absoluted.


##  Get my bearings and keep going.
names(data)
plot(data[,3],data[,7])
##  Meaningless graph.

gold = subset(data,is_gold == TRUE)
not.gold = subset(data,is_gold == FALSE)

plot(data$cc,data$ss,col = rgb(0,0,0,0),)
points(gold$cc,gold$ss,col = red,pch = 16)
points(not.gold$cc,not.gold$ss,col = blue,pch = 16)
legend("topleft",c("is gold","is not"),pch = 16,col = c(red,blue))
##  So "is_gold" is just those values for which CC > .2 and for
##  which the underlying drug wasn't DSMO or DMSO whatever.


##  regress on 6 and 24 hour exposure to see if cc predicts 24 cc.

par(mfrow = c(1,1))
plot(delta.6.10$deltaSS+delta.6.10$ss~delta.6.10$ss,pch = 16,
     col = red,xlab = "6 hour SS",
     ylab = "24 hour SS",main = "SS from 6 to 24 hours")
cor(delta.6.10$deltaSS+delta.6.10$ss,delta.6.10$ss)
##  Decent correlation between the 6 and 24 hour SS's

plot(delta.6.10$deltaCC+delta.6.10$cc~delta.6.10$cc,pch = 16,
     col = red,xlab = "6 hour CC",
     ylab = "24 hour CC",main = "CC from 6 to 24 hours")
cor(delta.6.10$deltaCC+delta.6.10$cc,delta.6.10$cc)
##  Not as great, but still decent correlation between
##  the 6 and 24 hour CC's

abline(v = .2)
abline(h = .2)

##  Although, are we throwing out the 24 CC values under .2
##  because they don't count as "gold"?  Let's actually look
##  instead of talk this time.

?lm
lm.1 = lm(delta.6.10$deltaSS+delta.6.10$ss~delta.6.10$ss)
plot(lm.1,col = blue,pch = 16,lwd = 3)
##  Not an impressive linear fit.  What about using both variable
##  dependencies?

lm.2 = lm(delta.6.10$deltaSS+delta.6.10$ss~delta.6.10$ss+delta.6.10$cc)
plot(lm.2,col = blue,pch = 16,lwd = 3)
##  Nope, not really much better.






##  Time to start asking different questions



##  Can we regress?  Use a linear model.  Then try regressing on
##  the 6 hour CC and SS simultaneously.




##  compare 6 hour and 24 hour is_gold percentages.  Is it worth
##  doing 24 hour tests.  Is one duration notably better?


head(data)
nrow(data)

sixes = subset(data,Exposed.time == 6,select = is_gold)
X6 = sum(sixes$is_gold);X6
N6 = nrow(sixes);N6
X6/N6

twofour = subset(data,Exposed.time == 24,select = is_gold)
X24 = sum(twofour$is_gold);X24
N24 = nrow(twofour);N24
X24/N24

X6/N6 - X24/N24

##  I suspect something fishy is going on here.  Namely that the 6 hour
##  is gold times are being taken out.  You can look at 24 hour tests
##  that are missing 6 hour counterparts and see that 679 non-gold
##  tests were likely taken from the data set.  So let's add those 679
##  tests back in. ()

N6 = N6 + 678

X6/N6
X24/N24

##  Now we've got something interesting on our hands.  Are these
##  proportions different?  If they're not, maybe it's not worth doing
##  24 hour tests.  Time for a bootstrap T interval.

##  Reinstantiating sixes and twofour:
sixes = c(rep(1,X6),rep(0,N6-X6))
twofour = c(rep(1,X24),rep(0,N24-X24))

var(sixes)
P6*(1-P6)

############

P6 = X6/N6; P6  #sample mean

P24 = X24/N24  #sample mean

thetahat = mean(sixes) - mean(twofour);thetahat
SE = sqrt(var(sixes)/N6 + var(twofour)/N24);SE

N = 10^4; Tstar = numeric(N) #vector of t statistics
for (i in 1:N) {
  six.boot = sample(sixes, size = N6, replace = TRUE)
  twofour.boot = sample(twofour, size = N24, replace = TRUE)
  Tstar[i] = (mean(six.boot)-mean(twofour.boot)-thetahat)/
    sqrt(var(six.boot)/N6 + var(twofour.boot)/N24)
}

q = quantile(Tstar, c(.025, .975), names = FALSE); q
L = thetahat - q[2]*SE; U = thetahat - q[1]*SE; L; U

##  And done classically with the prop.test?
prop.test(c(X6,X24),c(N6,N24))

##  The bootstrap t-confidence interval and the built-in
##  classical test yield the same result.

##  And regardless, it appears that the 24 hour is-gold
##  really does beat out the 6 hour is-gold.  Why not
##  expose everything for a longer period of time?

#############

##  Maybe repeat this idea, but with concentrations instead of times ...
##  nope, don't have concentrations for non-gold, but could do cell lines.

##  And then could compare the cell lines like the baseball players ...

data.n10 = subset(data,subset = (Conc. != 10),select =
  c("cell.ID","pharm_id","subgroup","Conc.",
    "Exposed.time","cc","ss","is_gold"))

for (i in 1:9) {
  plot(data$ss[data$cell.ID == i],data$Conc.[data$cell.ID == i],
       xlab = paste("SS for cell line",i),
       ylab = paste("Conc. for cell line",i))
}
##  Increasing your concentration doesn't seem to assure an
##  high SS response.


for (i in 1:9) {
  plot(data.n10$ss[data$cell.ID == i],data.n10$Conc.[data$cell.ID == i],
       xlab = paste("SS for cell line",i),
       ylab = paste("Conc. for cell line",i))
}
##  Same graphs with the 10 micromolar data removed.  Same story.
##  You either have an inherently high SS, or you don't.

##  But let's take a look at the data and see if the 10 micromolar stuff
##  just sees more of its distribution because of higher sampling.

index = (data$deltaConc != 0)
names(data)
conc = cbind(data$deltaConc[index],data$SSConc[index],data$ss[index],
             data$SSConc[index]+data$ss[index],data$cc[index])
head(conc)
colnames(conc) = c("deltaConc","deltaSS","originalSS","finalSS","CC")
head(conc)

##  change in concentration, change in SS, original SS

plot(conc[,3],conc[,4],xlab = "Original SS",
     ylab = "Final SS",main = "Before and after SS")
lm.1 = lm(conc[,4]~conc[,3])
abline(a = lm.1$coefficients[[1]],b = lm.1$coefficients[[2]],
       col = "red",lwd = 3)
r = cor(conc[,3],conc[,4]); r
##  some correlation!

r^2
##  So about 44% of the variation in final SS is determined by
##  the original SS.  Can we identify other sources?

cor(lm.1$residuals,conc[,5])
cor(lm.1$residuals,conc[,1])

##  sadly, our other "independent variables" seem to be
##  independent of our data :-(

##  so we can construct a model where a large amount of the
##  variance is determined by the starting position, but to
##  determine what else is influencing the model.



##  On average, increasing the concentration buys you 1.13 more
##  SS, but there's no obvious correlation between the variables

##  Again with the nothing.
##  It looks like pretty much everything is uncorrelated here.


data.10 = subset(data,subset = (Conc. != 10),select =
   c("cell.ID","pharm_id","subgroup","Conc.","Exposed.time",
     "cc","ss","is_gold"))

for (i in 1:9) {
  par(mfrow = c(2,1))
  hist(data.10$ss[data$cell.ID == i],xlab = paste("SS for cell line",i),
       main = paste("Histogram of SS for cell line",i,"(10 micromolar)"),
       xlim = c(1,13))
  hist(data.10$cc[data$cell.ID == i],xlab = paste("CC for cell line",i),
       main = paste("Histogram of CC for cell line",i,"(10 micromolar)"),
       xlim = c(0,1))  ##  xlim keeps the histograms on the same scale.
}

##  So, I really wanted to build a Bayesian calculator.
##  I looked at the emprical cdf gene response for each
##  drug group and used that to evaluate the likelihood
##  that an unknown drug was exhibiting the properties
##  of an already known drug class.

##  The function works off of an unknown prior and takes
##  a list of genes a drug has been tested on and what
##  the reactivity data was for each of those genes.

##  The function returns the posterior likelihoods that
##  a particular drug class was used.

data2 = read.csv("finaldata2.csv")
##  Data file with known gene responses to varied drug
##  groups.

##  Columns are - Cell.Line, Duration, Drug.number,
##  and then gene-# ...

prior = rep(1/363,363)

##  need a function that takes a drug number and ss and turns it
##  into a vector of likelihoods  It should look at the ECDF for each
##  known drug, for a specific gene, and turn that into a
##  a probability for each drug

##  given gene data and ss data (vectors of), what is the posterior?

DrugPred = function(gene,ss) {
  ##  Take a vector of genes and associated ss's
  posterior = rep(1/363,363)
  ##  Non-asusmptive prior.
  likelihood = numeric(363)
  for (j in 1:length(gene)) {
    ##  will update the posterior once for every gene passed
    ##  to the function.
    prior = posterior
    gene.t = gene[j]
    ss.t = ss[j]
    ##  grabbing values to be used by the next loop
    for (i in 1:363) {
      ##  calculating likelihoods for all 363 known drug classes
      index = (data2$Drug.number == i)
      quant = ecdf(data2[,gene.t + 3][index])
      likelihood[i] = .999 * (1 - 2*abs( .5-quant(ss.t)) ) + .001
    }
    posterior = prior * likelihood / (sum(prior*likelihood))
    ##  using the calculated likelihoods against the repeatedly
    ##  updated priors to calculate a more refined prior.
  }
  return(posterior)
  ##  return the final prior to user
}

post = DrugPred(1:50,rnorm(50))
##  Given 50 genes and their associated responses (randomly
##  generated here), what do the posterior likelihoods look
##  like for the different drug groups that could be 
##  represented.

plot(1,type="n", xlab = "Drugs",ylab = "Posteriors",
     xlim = c(0,364),ylim = c(0,70),
     main = "Negative log likelihoods (lower is better)")
points(1:363,-log(post),pch = 16,col = rgb(1,0,0,post^.05))
abline(h = 10,col = blue)

which(post>exp(-10))
##  The best posteriors, with lower being better.

round(post[which(post>exp(-10))],3)
##  Easy to discern which of the candidates is the best.

##  Testing to see if the function does its job.
k = 180
##  select a dummy drug.
index = (data2$Drug.number == k)
ktable = numeric(47)
for (i in 1:47) {
  ktable[i] = mean(data2[index,i+3]) + rnorm(1,0,.5)
}
##  build a table of values that should favor the dummy drug,
##  but throw some noise in the mix to make sure we're not
##  being ridiculous.

post = DrugPred(1:47,ktable)
##  Finding the Bayesian posteriors

plot(1,type="n", xlab = "Drugs",ylab = "Posteriors",
     xlim = c(0,364),ylim = c(0,70),
     main = "Negative log likelihoods (lower is better)")
points(1:363,-log(post),pch = 16,col = rgb(1,0,0,post^.05))
abline(h = 10,col = blue)

which(post>exp(-10))
##  The best posteriors, with lower being better.

round(post[which(post>exp(-10))],3)
##  Easy to discern which of the candidates is the best.

##  Notice that the calculator favors the desired drug,
##  even when a little noise is thrown in the mix.  I've
##  tested with varied choices of k, some are more robust
##  than others.





#####

##  I didn't like the step function from ecdf, so I modified
##  the function slightly to give a linear interpolation of
##  the ecdf.

DrugPred2 = function(gene,ss) {
  ##  Take a vector of genes and associated ss's
  posterior = rep(1/363,363)
  ##  Non-asusmptive prior.
  likelihood = numeric(363)
  for (j in 1:length(gene)) {
    ##  will update the posterior once for every gene passed
    ##  to the function.
    prior = posterior
    gene.t = gene[j]
    ss.t = ss[j]
    ##  grabbing values to be used by the next loop
    for (i in 1:363) {
      ##  calculating likelihoods for all 363 known drug classes
      index = (data2$Drug.number == i)
      values = as.numeric(data2[,gene.t + 3][index])
      values.plus = c(-100,min(values)-1,values,max(values+1),100)
      quant = ecdf(values)
      new.quant = approxfun(values.plus,quant(values.plus))
      ##  A slightly smoothed ecdf, derived from the original.
      likelihood[i] = .999 * (1 - 2*abs( .5-new.quant(ss.t)) ) + .001
      ##  created the funny sum to deal with 0 occuring because
      ##  of discrete distributions.
    }
    posterior = prior * likelihood / (sum(prior*likelihood))
    ##  using the calculated likelihoods against the repeatedly
    ##  updated priors to calculate a more refined prior.
  }
  return(posterior)
  ##  return the final prior to user
}


prior = rep(1/363,363)
##  no information prior.

post = DrugPred2(1:50,rnorm(50))
##  Given 50 genes and their associated responses (randomly
##  generated here), what do the posterior likelihoods look
##  like for the different drug groups that could be 
##  represented.

plot(1,type="n", xlab = "Drugs",ylab = "Posteriors",
     xlim = c(0,364),ylim = c(0,70),
     main = "Negative log likelihoods (lower is better)")
points(1:363,-log(post),pch = 16,col = rgb(1,0,0,post^.05))
abline(h = 10,col = blue)

which(post>exp(-10))
##  The best posteriors, with lower being better.

round(post[which(post>exp(-10))],3)
##  Easy to discern which of the candidates is the best.

##  Testing to see if the function does its job.
k = 180
##  select a dummy drug.
index = (data2$Drug.number == k)
ktable = numeric(47)
for (i in 1:47) {
  ktable[i] = mean(data2[index,i+3]) + rnorm(1,0,.8)
}
##  build a table of values that should favor the dummy drug,
##  but throw some noise in the mix to make sure we're not
##  being ridiculous.

post = DrugPred2(1:47,ktable)
##  Finding the Bayesian posteriors

plot(1,type="n", xlab = "Drugs",ylab = "Posteriors",
     xlim = c(0,364),ylim = c(0,70),
     main = "Negative log likelihoods (lower is better)")
points(1:363,-log(post),pch = 16,col = rgb(1,0,0,post^.05))
abline(h = 10,col = blue)

which(post>exp(-10))
##  The best posteriors, with lower being better.

round(post[which(post>exp(-10))],3)
##  Easy to discern which of the candidates is the best.

##  Notice that the calculator favors the desired drug,
##  even when a little noise is thrown in the mix.  I've
##  tested with varied choices of k, some are more robust
##  than others.





