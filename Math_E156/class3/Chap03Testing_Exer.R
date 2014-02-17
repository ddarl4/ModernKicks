#Chapter 3 Testing
#R Code for exercise

#-----------------
#Exercise 5

N<-10^4-1

UA.Delay <- subset(FlightDelays, select=Delay, Carrier=="UA", drop=T)
AA.Delay <- subset(FlightDelays, select=Delay, Carrier=="AA",drop=T)
observedSumUA<-sum(UA.Delay)
observedmeanUA<-mean(UA.Delay)
observedmeanDiff<-mean(UA.Delay)-mean(AA.Delay)
m<-length(UA.Delay)  #number of UA observations

sumUA<-numeric(N)
meanUA<-numeric(N)
meanDiff<-numeric(N)

set.seed(0)
for (i in 1:N)
{
  index<-sample(4029,  m, replace = FALSE)
  sumUA[i]<-sum(FlightDelays$Delay[index])
  meanUA[i]<-mean(FlightDelays$Delay[index])
  meanDiff[i]<-mean(FlightDelays$Delay[index])-mean(FlightDelays$Delay[-index])

}

(sum(sumUA >= observedSumUA)+1)/(N + 1)  #P-value

(sum(meanUA >= observedmeanUA)+1)/(N + 1)  #P-value

(sum(meanDiff >= observedmeanDiff)+1)/(N + 1)  #P-value

#-------------------------------
