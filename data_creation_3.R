### Continue in R after creating propoer reverse lags in Stata

#import back from stata
data <- read.csv("post_stata_slim.csv")

#calculate indicators for windows
data$window5 <- ifelse(data$mid1 > 0 | data$mid2 > 0 | data$mid3 > 0 | data$mid4 > 0 | data$mid5 > 0, 1, 0)
data$window5[is.na(data$window5)] <- 0

data$window10 <- ifelse(data$mid1 > 0 | data$mid2 > 0 | data$mid3 > 0 | data$mid4 > 0 | data$mid5 > 0 | data$mid6 > 0 | data$mid7 > 0 | data$mid8 > 0 | data$mid9 > 0 | data$mid10 > 0, 1, 0)
data$window10[is.na(data$window10)] <- 0

data$window20 <- ifelse(data$mid1 > 0 | data$mid2 > 0 | data$mid3 > 0 | data$mid4 > 0 | data$mid5 > 0 | data$mid6 > 0 | data$mid7 > 0 | data$mid8 > 0 | data$mid9 > 0 | data$mid10 > 0 | data$mid11 > 0 | data$mid12 > 0 | data$mid13 > 0 | data$mid14 > 0 | data$mid15 > 0 | data$mid16 > 0 | data$mid18 > 0 | data$mid19 > 0 | data$mid20 > 0, 1, 0)
data$window20[is.na(data$window20)] <- 0

data$outstanding.claim.5 <- ifelse(data$claim1 > 0 | data$claim2 > 0 | data$claim3 > 0 | data$claim4 > 0 | data$claim5 > 0, 1, 0)
data$outstanding.claim.5[is.na(data$outstanding.claim.5)] <- 0

data$outstanding.claim.10 <- ifelse(data$claim1 > 0 | data$claim2 > 0 | data$claim3 > 0 | data$claim4 > 0 | data$claim5 > 0 | data$claim6 > 0 | data$claim7 > 0 | data$claim8 > 0 | data$claim9 > 0 | data$claim10 > 0, 1, 0)
data$outstanding.claim.10[is.na(data$outstanding.claim.10)] <- 0

data$outstanding.claim.20 <- ifelse(data$claim1 > 0 | data$claim2 > 0 | data$claim3 > 0 | data$claim4 > 0 | data$claim5 > 0 | data$claim6 > 0 | data$claim7 > 0 | data$claim8 > 0 | data$claim9 > 0 | data$claim10 > 0 | data$claim11 > 0 | data$claim12 > 0 | data$claim13 > 0 | data$claim14 > 0 | data$claim15 > 0 | data$claim16 > 0 | data$claim18 > 0 | data$claim19 > 0 | data$claim20 > 0, 1, 0)
data$outstanding.claim.20[is.na(data$outstanding.claim.20)] <- 0

#get rid of lags, just keep indicators
data <- subset(data, select=c(year,dyad,cwkeynum,claimdy,mid1,window5,window10,window20,claim1,outstanding.claim.5,outstanding.claim.10,outstanding.claim.20))

#merge back together with the rest of the data
x <- read.csv("1_14.csv")
data <- merge(data,x,all=T)
rm(x)

#and finally, cut it back down to the territorial change as the unit of analysis
data <- subset(data, !is.na(data$version))

#create a few more variables
data$caprat = (data$cap_1/data$cap_2)
data$mid1 = ifelse(data$mid1 > 0, 1, 0)
data$mid1[is.na(data$mid1)] = 0
data$claim1 = ifelse(data$claim1 > 0, 1, 0)
data$claim1[is.na(data$claim1)] = 0
data$rivals <- ifelse(data$rivtyp2=="RIVALRY", 1, 0)
data$rivals[is.na(data$rivals)] <- 0
data$joint.dem <- ifelse(data$polity21 >= 6 & data$polity22 >= 6, 1, 0)

#I want to get the beginning date of claims to control for pre-existing ones
icow <- read.csv("http://dl.dropboxusercontent.com/u/4115584/ICOWprov10.csv")
icow <- subset(icow, select=c(claimdy,begclaim))
icow$begclaim <- as.numeric(substr(icow$begclaim,1,4))
data <- merge(data,icow,by="claimdy",all.x=T,all.y=F)
rm(icow)
data$begclaim.x <- ifelse(is.na(data$begclaim.x)==T, data$begclaim.y, data$begclaim.x)
data <- subset(data, select=-begclaim.y)
colnames(data)[77] <- "begclaim"
data$existing.claim <- ifelse(data$year > data$begclaim, 1, 0)
data$existing.claim[is.na(data$existing.claim)] <- 0

#create a separate dv for new claims
data$new.claim.1 <- ifelse(data$claim1==1& data$begclaim>=data$year, 1, 0)
data$new.claim.1[is.na(data$new.claim.1)] <- 0
data$new.claim.5 <- ifelse(data$outstanding.claim.5==1 & data$begclaim>=data$year,1,0)
data$new.claim.5[is.na(data$new.claim.5)] <- 0
data$new.claim.10 <- ifelse(data$outstanding.claim.10==1 & data$begclaim>=data$year,1,0)
data$new.claim.10[is.na(data$new.claim.10)] <- 0
data$new.claim.20 <- ifelse(data$outstanding.claim.20==1 & data$begclaim>=data$year,1,0)
data$new.claim.20[is.na(data$new.claim.20)] <- 0

#and write the data
write.csv(data,"tc_1_14.csv")