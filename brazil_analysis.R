#### Analysis for Brazil paper

data <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/compliance/compliance.csv")

library(Zelig)
library(stargazer)

data$caprat = (data$cap_1/data$cap_2)
data$lead1 = ifelse(data$lead1 > 0, 1, 0)
data$lead1[is.na(data$lead)] = 0
data$lagtc1 = ifelse(data$lagtc1 > 0, 1, 0)
data$lagtc1[is.na(data$lagtc1)] = 0


### Table 1: MIDs 1,5,10,20 years after trasnfer
m1 = zelig(lead1 ~ agreement + conflict + contlose + caprat, data=data, model="logit")
summary(m1)
m2 = zelig(lead1 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="logit")
summary(m2)
m3 = zelig(window5 ~ agreement + conflict + contlose + caprat, data=data, model="logit")
summary(m3)
m4 = zelig(window5 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="logit")
summary(m4)
m5 = zelig(window10 ~ agreement + conflict + contlose + caprat, data=data, model="logit")
summary(m5)
m6 = zelig(window10 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="logit")
summary(m6)
m7 = zelig(window20 ~ agreement + contlose + conflict + caprat, data=data, model="logit")
summary(m7)
m8 = zelig(window20 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, model="logit")
summary(m8)

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,title="Logit Models of the Effect of Legalized Territorial Transfers on Territorial MIDs",style="apsr")

#### Claims

m9 = zelig(lagtc1 ~ agreement + conflict + contlose + caprat, data=data, model="logit")
summary(m9)
m10 = zelig(lagtc1 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="logit")
summary(m10)
m11 = zelig(outstanding.claim5 ~ agreement + conflict + contlose + caprat, data=data, model="logit")
summary(m11)
m12 = zelig(outstanding.claim5 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, model="logit")
summary(m12)

### Predicted Probabilities

mid1.no.agreement = setx(m2, agreement=0)
mid1.agreement = setx(m2, agreement = 1)
mid1.prob = sim(m2, x=mid1.no.agreement, x1=mid1.agreement)