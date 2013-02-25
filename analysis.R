m1 = glm(ongoing ~ non.pleb, data=data,family=binomial(logit))
summary(m1)
m2 = glm(ongoing ~ non.pleb + contlose + conflict,data=data,family=binomial(logit))
summary(m2)
m2.1 = glm(ongoing ~ contlose + conflict*non.pleb,data=data,family=binomial(logit))
summary(m2.1)
m3 = glm(ongoing ~ agreement, data=data,family=binomial(logit))
summary(m3)
m4 = glm(ongoing ~ agreement + contlose,data=data,family=binomial(logit))
summary(m4)

m5 = glm(lag(ter, -1) ~ non.pleb, data=data, family=binomial(logit))
summary(m5)
m6 = glm(lag(ter, -1) ~ non.pleb + contlose + conflict, data=data, family=binomial(logit))
summary(m6)
m7 = glm(lag(ter, -1) ~ agreement, data=data, family=binomial(logit))
summary(m7)
m8 = glm(lag(ter, -1) ~ agreement + contlose, data=data, family=binomial(logit))
summary(m8)

#make some latex tables
library(stargazer)
stargazer(m1, m2, m2.1, title="The Effect of Treaties on Territorial Claims", style="apsr")

stargazer(m3, m4, title="The Effect of Non-Violent Agreements on Territorial Claims", style="apsr")

stargazer(m5, m6, m7, m8, title="The Effect of Agreements on Territorial MIDs", style="apsr")



summary(glm(w20 ~ agreement, data=data,family=binomial(logit)))
summary(glm(w10 ~ agreement, data=data,family=binomial(logit)))
