#### Analysis for Brazil paper
#updated 9.15.13

#data <- read.csv("compliance.csv")
data <- read.csv("9_2013.csv")

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

########### Look at post-WWII

h1 = glm(lead1 ~ agreement + conflict + contlose, data=data, family=binomial(logit), subset=year > 1945)
summary(h1)
h2 = glm(lead1 ~ agreement + arbadj2 + conflict + contlose , data=data, family=binomial(logit), subset=year > 1945)
summary(h2)
h3 = glm(window5 ~ agreement + conflict + contlose, data=data, family=binomial(logit), subset=year > 1945)
summary(h3)
h4 = glm(window5 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year > 1945)
summary(h4)
h5 = glm(window10 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year > 1945)
summary(h5)
h6 = glm(window10 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year > 1945)
summary(h6)
h7 = glm(window20 ~ agreement + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year > 1945)
summary(h7)
h8 = glm(window20 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year > 1945)
summary(h8)
h9 = glm(lagtc1 ~ agreement + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year>1945)
summary(h9)
h10 = glm(lagtc1 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year>1945)
summary(h10)

stargazer(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,title="Post-WWII",style="apsr")

#### Pre-WWII

j1 = glm(lead1 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j1)
j2 = glm(lead1 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j2)
j3 = glm(window5 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j3)
j4 = glm(window5 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j4)
j5 = glm(window10 ~ agreement + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j5)
j6 = glm(window10 ~ agreement + arbadj2 + conflict + contlose + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j6)
j7 = glm(window20 ~ agreement + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j7)
j8 = glm(window20 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j8)
j9 = glm(lagtc1 ~ agreement + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j9)
j10 = glm(lagtc1 ~ agreement + arbadj2 + contlose + conflict + caprat, data=data, family=binomial(logit), subset=year < 1945)
summary(j10)

stargazer(j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,title="Pre-WWII",style="apsr")

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

m9 = zelig(lagtc1 ~ agreement + conflict + contlose + caprat + polity21, data=data, model="relogit")
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
i6 = glm(outstanding.claim5 ~ agreement*conflict + contlose + caprat, data=data, family=binomial(logit))
summary(i6)

stargazer(i1,i2,i3,i4,i5,i6,title="hi",style="apsr")
