#### Analysis for Brazil paper

data <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/compliance/compliance.csv")

library(Zelig)
library(stargazer)

data$caprat = (data$cap_1/data$cap_2)

##################

m1 = zelig(window5 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
summary(m1)
