############ Analyis for Bowden and Diehl (2013)
library(Zelig)
library(stargazer)

data$caprat = (data$cap_1/data$cap_2)

###### mids within 5 years

m1 = zelig(window5 ~ agreement, data=data, model="logit")
summary(m1)
m2 = zelig(window5 ~ agreement + conflict, data=data, model="logit")
summary(m2)
m3 = zelig(window5 ~ agreement + contlose, data=data, model="logit")
summary(m3)
m4 = zelig(window5 ~ agreement + log(area), data=data, model="logit")
summary(m4)
m5 = zelig(window5 ~ agreement + caprat, data= data, model="logit")
summary(m5)
m6 = zelig(window5 ~ agreement + conflict + contlose, data=data, model="logit")
summary(m6)
m7 = zelig(window5 ~ agreement + conflict + caprat, data=data, model="logit")
summary(m7)
m8 = zelig(window5 ~ agreement + contlose + caprat, data=data, model="logit")
summary(m8)
m9 = zelig(window5 ~ agreement + contlose + caprat + conflict, data=data, model="logit")
summary(m9)

stargazer(m1,m2,m3,m4,m5,m6,m7,m8,m9,title="Effect of Agreements on MIDs within 5 Years",style="apsr")

#######################

m10 = zelig(window5 ~ arbadj2, data=data, model="logit")
summary(m10)
m11 = zelig(window5 ~ arbadj2 + conflict, data=data, model="logit")
summary(m11)
m12 = zelig(window5 ~ arbadj2 + contlose, data=data, model="logit")
summary(m12)
m13 = zelig(window5 ~ arbadj2 + log(area), data=data, model="logit")
summary(m13)
m14 = zelig(window5 ~ arbadj2 + caprat, data= data, model="logit")
summary(m14)
m15 = zelig(window5 ~ arbadj2 + conflict + contlose, data=data, model="logit")
summary(m15)
m16 = zelig(window5 ~ arbadj2 + conflict + caprat, data=data, model="logit")
summary(m16)
m17 = zelig(window5 ~ arbadj2 + contlose + caprat, data=data, model="logit")
summary(m17)
m18 = zelig(window5 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")
summary(m18)

stargazer(m10,m11,m12,m13,m14,m15,m16,m17,m18,title="Effect of Adjudication on MIDs",style="apsr")

#################################

m19 = zelig(window5 ~ arbadj2 + agreement, data=data, model="logit")
summary(m19)
m20 = zelig(window5 ~ arbadj2 + agreement + conflict, data=data, model="logit")
summary(m20)
m21 = zelig(window5 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m22 = zelig(window5 ~ arbadj2 + agreement + log(area), data=data, model="logit")
m23 = zelig(window5 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m24 = zelig(window5 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
m25 = zelig(window5 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
m26 = zelig(window5 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
m27 = zelig(window5 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m19,m20,m21,m22,m23,m24,m25,m26,m26,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m28 = zelig(window5 ~ arbadj2*agreement, data=data, model="logit")
m29 = zelig(window5 ~ arbadj2*agreement + conflict, data=data, model="logit")
m30 = zelig(window5 ~ arbadj2*agreement + contlose, data=data, model="logit")
m31 = zelig(window5 ~ arbadj2*agreement + log(area), data=data, model="logit")
m32 = zelig(window5 ~ arbadj2*agreement + caprat, data= data, model="logit")
m33 = zelig(window5 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
m34 = zelig(window5 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
m35 = zelig(window5 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
m36 = zelig(window5 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m28,m29,m30,m31,m32,m33,m34,m35,m36,title="Interactive Effects of Agreements and Adjudication",style="apsr")

########################## 10 Years

m37 = zelig(window10 ~ agreement, data=data, model="logit")
m38 = zelig(window10 ~ agreement + conflict, data=data, model="logit")
m39 = zelig(window10 ~ agreement + contlose, data=data, model="logit")
m40 = zelig(window10 ~ agreement + log(area), data=data, model="logit")
m41 = zelig(window10 ~ agreement + caprat, data= data, model="logit")
m42 = zelig(window10 ~ agreement + conflict + contlose, data=data, model="logit")
m43 = zelig(window10 ~ agreement + conflict + caprat, data=data, model="logit")
m44 = zelig(window10 ~ agreement + contlose + caprat, data=data, model="logit")
m45 = zelig(window10 ~ agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m37,m38,m39,m40,m41,m42,m43,m44,m45,title="Effect of Agreements on MIDs",style="apsr")

#######################

m46 = zelig(window10 ~ arbadj2, data=data, model="logit")
m47 = zelig(window10 ~ arbadj2 + conflict, data=data, model="logit")
m48 = zelig(window10 ~ arbadj2 + contlose, data=data, model="logit")
m49 = zelig(window10 ~ arbadj2 + log(area), data=data, model="logit")
m50 = zelig(window10 ~ arbadj2 + caprat, data= data, model="logit")
m51 = zelig(window10 ~ arbadj2 + conflict + contlose, data=data, model="logit")
m52 = zelig(window10 ~ arbadj2 + conflict + caprat, data=data, model="logit")
m53 = zelig(window10 ~ arbadj2 + contlose + caprat, data=data, model="logit")
m54 = zelig(window10 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")

stargazer(m46,m47,m48,m49,m50,m51,m52,m53,m54,title="Effect of Adjudication on MIDs",style="apsr")

#################################

m55 = zelig(window10 ~ arbadj2 + agreement, data=data, model="logit")
m56 = zelig(window10 ~ arbadj2 + agreement + conflict, data=data, model="logit")
m57 = zelig(window10 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m58 = zelig(window10 ~ arbadj2 + agreement + log(area), data=data, model="logit")
m59 = zelig(window10 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m60 = zelig(window10 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
m61 = zelig(window10 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
m62 = zelig(window10 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
m63 = zelig(window10 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m55,m56,m57,m58,m59,m60,m61,m62,m63,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m64 = zelig(window10 ~ arbadj2*agreement, data=data, model="logit")
m65 = zelig(window10 ~ arbadj2*agreement + conflict, data=data, model="logit")
m66 = zelig(window10 ~ arbadj2*agreement + contlose, data=data, model="logit")
m67 = zelig(window10 ~ arbadj2*agreement + log(area), data=data, model="logit")
m68 = zelig(window10 ~ arbadj2*agreement + caprat, data= data, model="logit")
m69 = zelig(window10 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
m70 = zelig(window10 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
m71 = zelig(window10 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
m72 = zelig(window10 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m64,m65,m66,m67,m68,m69,m70,m71,m72,title="Interactive Effects of Agreements and Adjudication",style="apsr")

############### 20 Years ################

m73 = zelig(window20 ~ agreement, data=data, model="logit")
m74 = zelig(window20 ~ agreement + conflict, data=data, model="logit")
m75 = zelig(window20 ~ agreement + contlose, data=data, model="logit")
m76 = zelig(window20 ~ agreement + log(area), data=data, model="logit")
m77 = zelig(window20 ~ agreement + caprat, data= data, model="logit")
m78 = zelig(window20 ~ agreement + conflict + contlose, data=data, model="logit")
m79 = zelig(window20 ~ agreement + conflict + caprat, data=data, model="logit")
m80 = zelig(window20 ~ agreement + contlose + caprat, data=data, model="logit")
m81 = zelig(window20 ~ agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m73,m74,m75,m76,m77,m78,m79,m80,m81,title="Effect of Agreements on MIDs within 20 Years",style="apsr")

#######################

m82 = zelig(window20 ~ arbadj2, data=data, model="logit")
m83 = zelig(window20 ~ arbadj2 + conflict, data=data, model="logit")
m84 = zelig(window20 ~ arbadj2 + contlose, data=data, model="logit")
m85 = zelig(window20 ~ arbadj2 + log(area), data=data, model="logit")
m86 = zelig(window20 ~ arbadj2 + caprat, data= data, model="logit")
m87 = zelig(window20 ~ arbadj2 + conflict + contlose, data=data, model="logit")
m88 = zelig(window20 ~ arbadj2 + conflict + caprat, data=data, model="logit")
m89 = zelig(window20 ~ arbadj2 + contlose + caprat, data=data, model="logit")
m90 = zelig(window20 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")

stargazer(m82,m83,m84,m85,m86,m87,m88,m89,m90,title="Effect of Adjudication on MIDs within 20 Years",style="apsr")

#################################

m91 = zelig(window20 ~ arbadj2 + agreement, data=data, model="logit")
m92 = zelig(window20 ~ arbadj2 + agreement + conflict, data=data, model="logit")
m93 = zelig(window20 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m94 = zelig(window20 ~ arbadj2 + agreement + log(area), data=data, model="logit")
m95 = zelig(window20 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m96 = zelig(window20 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
m97 = zelig(window20 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
m98 = zelig(window20 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
m99 = zelig(window20 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m91,m92,m93,m94,m95,m96,m97,m98,m99,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m100 = zelig(window20 ~ arbadj2*agreement, data=data, model="logit")
m101 = zelig(window20 ~ arbadj2*agreement + conflict, data=data, model="logit")
m102 = zelig(window20 ~ arbadj2*agreement + contlose, data=data, model="logit")
m103 = zelig(window20 ~ arbadj2*agreement + log(area), data=data, model="logit")
m104 = zelig(window20 ~ arbadj2*agreement + caprat, data= data, model="logit")
m105 = zelig(window20 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
m106 = zelig(window20 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
m107 = zelig(window20 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
m108 = zelig(window20 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m100,m101,m102,m103,m104,m105,m106,m107,m108,title="Interactive Effects of Agreements and Adjudication",style="apsr")

###################################
###############################################
#########################################################
#################################################################
#########################################################
###############################################
###################################

#claims time

m109 = zelig(outstanding.claim5 ~ agreement, data=data, model="logit")
m110 = zelig(outstanding.claim5 ~ agreement + conflict, data=data, model="logit")
m111 = zelig(outstanding.claim5 ~ agreement + contlose, data=data, model="logit")
m112 = zelig(outstanding.claim5 ~ agreement + log(area), data=data, model="logit")
#m113 = zelig(outstanding.claim5 ~ agreement + caprat, data= data, model="logit")
m114 = zelig(outstanding.claim5 ~ agreement + conflict + contlose, data=data, model="logit")
#m115 = zelig(outstanding.claim5 ~ agreement + conflict + caprat, data=data, model="logit")
#m116 = zelig(outstanding.claim5 ~ agreement + contlose + caprat, data=data, model="logit")
#m117 = zelig(outstanding.claim5 ~ agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m109,m110,m111,m112,m114,title="Effect of Agreements on Claims within 5 Years",style="apsr")

#######################

m118 = zelig(outstanding.claim5 ~ arbadj2, data=data, model="logit")
m119 = zelig(outstanding.claim5 ~ arbadj2 + conflict, data=data, model="logit")
m120 = zelig(outstanding.claim5 ~ arbadj2 + contlose, data=data, model="logit")
m121 = zelig(outstanding.claim5 ~ arbadj2 + log(area), data=data, model="logit")
#m122 = zelig(outstanding.claim5 ~ arbadj2 + caprat, data= data, model="logit")
m123 = zelig(outstanding.claim5 ~ arbadj2 + conflict + contlose, data=data, model="logit")
#m124 = zelig(outstanding.claim5 ~ arbadj2 + conflict + caprat, data=data, model="logit")
#m125 = zelig(outstanding.claim5 ~ arbadj2 + contlose + caprat, data=data, model="logit")
#m126 = zelig(outstanding.claim5 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")

stargazer(m118,m119,m120,m121,m123,title="Effect of Adjudication on Claims",style="apsr")

#################################

m127 = zelig(outstanding.claim5 ~ arbadj2 + agreement, data=data, model="logit")
m128 = zelig(outstanding.claim5 ~ arbadj2 + agreement + conflict, data=data, model="logit")
m129 = zelig(outstanding.claim5 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m130 = zelig(outstanding.claim5 ~ arbadj2 + agreement + log(area), data=data, model="logit")
#m131 = zelig(outstanding.claim5 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m132 = zelig(outstanding.claim5 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
#m133 = zelig(outstanding.claim5 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
#m134 = zelig(outstanding.claim5 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
#m135 = zelig(outstanding.claim5 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m127,m128,m129,m130,m132,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m136 = zelig(outstanding.claim5 ~ arbadj2*agreement, data=data, model="logit")
m137 = zelig(outstanding.claim5 ~ arbadj2*agreement + conflict, data=data, model="logit")
m138 = zelig(outstanding.claim5 ~ arbadj2*agreement + contlose, data=data, model="logit")
m139 = zelig(outstanding.claim5 ~ arbadj2*agreement + log(area), data=data, model="logit")
#m140\\ = zelig(outstanding.claim5 ~ arbadj2*agreement + caprat, data= data, model="logit")
m141 = zelig(outstanding.claim5 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
#m34 = zelig(outstanding.claim5 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
#m35 = zelig(outstanding.claim5 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
#m36 = zelig(outstanding.claim5 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m136,m137,m138,m139,m141,title="Interactive Effects of Agreements and Adjudication",style="apsr")

########################## 10 Years

m37 = zelig(window10 ~ agreement, data=data, model="logit")
m38 = zelig(window10 ~ agreement + conflict, data=data, model="logit")
m39 = zelig(window10 ~ agreement + contlose, data=data, model="logit")
m40 = zelig(window10 ~ agreement + log(area), data=data, model="logit")
m41 = zelig(window10 ~ agreement + caprat, data= data, model="logit")
m42 = zelig(window10 ~ agreement + conflict + contlose, data=data, model="logit")
m43 = zelig(window10 ~ agreement + conflict + caprat, data=data, model="logit")
m44 = zelig(window10 ~ agreement + contlose + caprat, data=data, model="logit")
m45 = zelig(window10 ~ agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m37,m38,m39,m40,m41,m42,m43,m44,m45,title="Effect of Agreements on MIDs",style="apsr")

#######################

m46 = zelig(window10 ~ arbadj2, data=data, model="logit")
m47 = zelig(window10 ~ arbadj2 + conflict, data=data, model="logit")
m48 = zelig(window10 ~ arbadj2 + contlose, data=data, model="logit")
m49 = zelig(window10 ~ arbadj2 + log(area), data=data, model="logit")
m50 = zelig(window10 ~ arbadj2 + caprat, data= data, model="logit")
m51 = zelig(window10 ~ arbadj2 + conflict + contlose, data=data, model="logit")
m52 = zelig(window10 ~ arbadj2 + conflict + caprat, data=data, model="logit")
m53 = zelig(window10 ~ arbadj2 + contlose + caprat, data=data, model="logit")
m54 = zelig(window10 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")

stargazer(m46,m47,m48,m49,m50,m51,m52,m53,m54,title="Effect of Adjudication on MIDs",style="apsr")

#################################

m55 = zelig(window10 ~ arbadj2 + agreement, data=data, model="logit")
m56 = zelig(window10 ~ arbadj2 + agreement + conflict, data=data, model="logit")
m57 = zelig(window10 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m58 = zelig(window10 ~ arbadj2 + agreement + log(area), data=data, model="logit")
m59 = zelig(window10 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m60 = zelig(window10 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
m61 = zelig(window10 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
m62 = zelig(window10 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
m63 = zelig(window10 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m55,m56,m57,m58,m59,m60,m61,m62,m63,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m64 = zelig(window10 ~ arbadj2*agreement, data=data, model="logit")
m65 = zelig(window10 ~ arbadj2*agreement + conflict, data=data, model="logit")
m66 = zelig(window10 ~ arbadj2*agreement + contlose, data=data, model="logit")
m67 = zelig(window10 ~ arbadj2*agreement + log(area), data=data, model="logit")
m68 = zelig(window10 ~ arbadj2*agreement + caprat, data= data, model="logit")
m69 = zelig(window10 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
m70 = zelig(window10 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
m71 = zelig(window10 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
m72 = zelig(window10 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m64,m65,m66,m67,m68,m69,m70,m71,m72,title="Interactive Effects of Agreements and Adjudication",style="apsr")

############### 20 Years ################

m73 = zelig(window20 ~ agreement, data=data, model="logit")
m74 = zelig(window20 ~ agreement + conflict, data=data, model="logit")
m75 = zelig(window20 ~ agreement + contlose, data=data, model="logit")
m76 = zelig(window20 ~ agreement + log(area), data=data, model="logit")
m77 = zelig(window20 ~ agreement + caprat, data= data, model="logit")
m78 = zelig(window20 ~ agreement + conflict + contlose, data=data, model="logit")
m79 = zelig(window20 ~ agreement + conflict + caprat, data=data, model="logit")
m80 = zelig(window20 ~ agreement + contlose + caprat, data=data, model="logit")
m81 = zelig(window20 ~ agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m73,m74,m75,m76,m77,m78,m79,m80,m81,title="Effect of Agreements on MIDs within 20 Years",style="apsr")

#######################

m82 = zelig(window20 ~ arbadj2, data=data, model="logit")
m83 = zelig(window20 ~ arbadj2 + conflict, data=data, model="logit")
m84 = zelig(window20 ~ arbadj2 + contlose, data=data, model="logit")
m85 = zelig(window20 ~ arbadj2 + log(area), data=data, model="logit")
m86 = zelig(window20 ~ arbadj2 + caprat, data= data, model="logit")
m87 = zelig(window20 ~ arbadj2 + conflict + contlose, data=data, model="logit")
m88 = zelig(window20 ~ arbadj2 + conflict + caprat, data=data, model="logit")
m89 = zelig(window20 ~ arbadj2 + contlose + caprat, data=data, model="logit")
m90 = zelig(window20 ~ arbadj2 + contlose + caprat + conflict, data=data, model="logit")

stargazer(m82,m83,m84,m85,m86,m87,m88,m89,m90,title="Effect of Adjudication on MIDs within 20 Years",style="apsr")

#################################

m91 = zelig(window20 ~ arbadj2 + agreement, data=data, model="logit")
m92 = zelig(window20 ~ arbadj2 + agreement + conflict, data=data, model="logit")
m93 = zelig(window20 ~ arbadj2 + agreement + contlose, data=data, model="logit")
m94 = zelig(window20 ~ arbadj2 + agreement + log(area), data=data, model="logit")
m95 = zelig(window20 ~ arbadj2 + agreement + caprat, data= data, model="logit")
m96 = zelig(window20 ~ arbadj2 + agreement + conflict + contlose, data=data, model="logit")
m97 = zelig(window20 ~ arbadj2 + agreement + conflict + caprat, data=data, model="logit")
m98 = zelig(window20 ~ arbadj2 + agreement + contlose + caprat, data=data, model="logit")
m99 = zelig(window20 ~ arbadj2 + agreement + contlose + caprat + conflict, data=data, model="logit")

stargazer(m91,m92,m93,m94,m95,m96,m97,m98,m99,title="Additive Effects of Agreements and Adjudication",style="apsr")

##############################

m100 = zelig(window20 ~ arbadj2*agreement, data=data, model="logit")
m101 = zelig(window20 ~ arbadj2*agreement + conflict, data=data, model="logit")
m102 = zelig(window20 ~ arbadj2*agreement + contlose, data=data, model="logit")
m103 = zelig(window20 ~ arbadj2*agreement + log(area), data=data, model="logit")
m104 = zelig(window20 ~ arbadj2*agreement + caprat, data= data, model="logit")
m105 = zelig(window20 ~ arbadj2*agreement + conflict + contlose, data=data, model="logit")
m106 = zelig(window20 ~ arbadj2*agreement + conflict + caprat, data=data, model="logit")
m107 = zelig(window20 ~ arbadj2*agreement + contlose + caprat, data=data, model="logit")
m108 = zelig(window20 ~ arbadj2*agreement + contlose + caprat + conflict, data=data, model="logit")