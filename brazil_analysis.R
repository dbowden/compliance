#### Analysis for Brazil paper

data <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/compliance/compliance.csv")

library(Zelig)
library(stargazer)

data$caprat = (data$cap_1/data$cap_2)
data$lead1 = ifelse(data$lead1 > 0, 1, 0)
data$lead1[is.na(data$lead)] = 0
data$lagtc1 = ifelse(data$lagtc1 > 0, 1, 0)
data$lagtc1[is.na(data$lagtc1)] = 0

write.csv(data, "C:/Users/David/Desktop/stata.csv")


### Table 1: MIDs 1,5,10,20 years after trasnfer
m1 = glm(lead1 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m1)
m2 = glm(lead1 ~ arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m2)
m3 = glm(window5 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m3)
m4 = glm(window5 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m4)
m5 = glm(window10 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m5)
m6 = glm(window10 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit))
summary(m6)
m7 = glm(window20 ~ agreement + contlose + conflict + caprat, data=data, family=binomial(logit))
summary(m7)
m8 = glm(window20 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, family=binomial(logit))
summary(m8)

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,title="Logit Models of the Effect of Legalized Territorial Transfers on Territorial MIDs",style="apsr")

# m1.5 = zelig(lead1 ~ agreement + conflict + contlose, data=data, model="relogit")
# summary(m1.5)
# m2.5 = zelig(lead1 ~ agreement + arbadj2 + conflict + contlose , data=data, model="relogit")
# summary(m2.5)
# m3.5 = zelig(window5 ~ agreement + conflict + contlose, data=data, model="relogit")
# summary(m3.5)
# m4.5 = zelig(window5 ~ agreement + arbadj2 + conflict + contlose, data=data, model="relogit")
# summary(m4.5)
# m5.5 = zelig(window10 ~ agreement + conflict + contlose, data=data, model="relogit")
# summary(m5.5)
# m6.5 = zelig(window10 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="relogit")
# summary(m6.5)
# m7.5 = zelig(window20 ~ agreement + contlose + conflict + caprat, data=data, model="relogit")
# summary(m7.5)
# m8.5 = zelig(window20 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, model="relogit")
# summary(m8.5)
# 
# stargazer(m1.5,m2.5,m3.5,m4.5,m5.5,m6.5,m7.5,m8.5,title="With Rare-events Correction",style="apsr")

#### Claims

m9 = zelig(lagtc1 ~ agreement + conflict + contlose + caprat, data=data, model="relogit")
summary(m9)
m10 = zelig(lagtc1 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, model="relogit")
summary(m10)
m12 = zelig(outstanding.claim5 ~ arbadj2 + contlose + conflict + caprat, data=data, model="relogit")
summary(m12)

stargazer(m9,m10,m12,title="With rare-events correction",style="apsr")

########### Interactions

i1 = zelig(lead1 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i1)
i2 = zelig(window5 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i2)
i3 = zelig(window10 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i3)
i4 = zelig(window20 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i4)
i5 = zelig(lagtc1 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i5)
i6 = zelig(outstanding.claim5 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i6)

stargazer(i1,i2,i3,i4,i5,i6,title="hi",style="apsr")