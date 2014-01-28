#### Code to create data for Bowden and Diehl (2013) paper to be presented at Brazil Compliance Conference ###

############ load and clean data ##########
tc <- read.csv("http://dl.dropbox.com/u/4115584/tc2008.csv")
frame <- read.csv("http://dl.dropboxusercontent.com/u/4115584/frame2.csv")
courts <- read.delim("http://dl.dropbox.com/u/4115584/huthallee")

#standardize NA codes
tc[tc == "."] <- NA
tc[tc == -9] <- NA
#drop <- c("polity21","polity22")
#frame <- frame[!names(frame) %in% drop]
#rm(drop)
frame[frame < -10] <- NA
frame[,1:32][frame[,1:32] < 0] <- NA

#narrow to state-to-state transfers
tc <- subset(tc, indep == 0) #remove territories that became independent
tc <- subset(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- subset(tc, loser > 0) #remove cases with NA for losing state

#remove plebiscites
tc$agreement <- ifelse(tc$procedur == 3, 1, 0)

# recode cessions that are plebscites as determined by COW code sheets and Beigbeder (1994). Limiting to state-to-state gets rid of most of them.
tc[289,21] <- 0
tc[295,21] <- 0
tc[277,21] <- 0
tc[377,21] <- 0
tc[93,21] <- 0
tc[407,21] <- 0
tc[416,21] <- 0
tc[417,21] <- 0

#remove violence
tc$nonviolent.agreement <- ifelse(tc$conflict == 0 & tc$agreement == 1, 1, 0)

#limit to territorial MIDs
frame$ter <- ifelse(frame$cwrevt11 == 1 | frame$cwrevt21 == 1 | frame$cwrevt21 == 1 | frame$cwrevt22 == 1 | frame$cwrevt12 == 1, 1, 0)

#standardize country codes and years for merging
colnames(frame)[1] <- "loser"
colnames(frame)[2] <- "gainer"


############ Merge data #############

#clean up huth and allee
courts <- subset(courts, arbadj2 == 1)
colnames(courts)[1] <- "gainer"
colnames(courts)[2] <- "loser"
trncint <- function(x, left=0,length=0)  (x %% 10^(left) ) %/%
  10^(left-length)
courts$year.2 <- trncint(courts$edatepog, 2, 2)
keeps <- c("gainer","loser","year","arbadj2")
courts <- courts[names(courts) %in% keeps]
rm(keeps)


#merge
tc$year.2 <- trncint(tc$year, 2, 2)
tc <- merge(tc, courts, all.x=T, all.y=F)
rm(courts,trncint)

#make the NAs zeroes
tc$arbadj2[is.na(tc$arbadj2)] <- 0

#code more recent cases
tc[367,24] <- 1
tc[368,24] <- 1
tc[370,24] <- 1

#drop non-states
tc <- subset(tc, gainer > 1)
tc <- subset(tc, loser > 1)

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)
gc()

############# create claim windows #################################

# first icow has to be merged in twice, by beg and end of claims

#merge in icow using end year to capture ongoing claims using leads
icow <- read.csv("http://dl.dropboxusercontent.com/u/4115584/ICOWprov10.csv")
icow <- subset(icow, select = -version)
icow$endclaim[icow$endclaim == -9] <- 201312
icow[icow == -9] <- NA
colnames(icow)[4] <- "loser"
colnames(icow)[5] <- "gainer" #change challenger and target to tc terminology
icow$begclaim <- substr(icow$begclaim, 6, 4) #6 is position of first digit in 6 digit year+mo, keep 4 digits
icow$endclaim <- substr(icow$endclaim, 6, 4)
icow$year <- icow$endclaim
icow$year[is.na(icow$year)] <- 2030
gc()
data <- merge(data, icow, by=c("loser","gainer","year"), all=T)
rm(icow)

#merge in icow using beg year to capture new claims by lagging tc att
icow <- read.csv("http://dl.dropboxusercontent.com/u/4115584/ICOWprov10.csv")
icow <- subset(icow, select = -version)
icow$endclaim[icow$endclaim == -9] <- 201312
icow[icow == -9] <- NA
colnames(icow)[4] <- "loser"
colnames(icow)[5] <- "gainer" #change challenger and target to tc terminology
icow$begclaim <- substr(icow$begclaim, 6, 4) #6 is position of first digit in 6 digit year+mo, keep 4 digits
icow$endclaim <- substr(icow$endclaim, 6, 4)
icow$year <- icow$begclaim
gc()
data <- merge(data, icow, all=T)
rm(icow)

# ######## create a bunch of leads ########
#
# library(plm)
#
# data$dyad <- paste(data$gainer,data$loser,sep="")
#
# version <- data$version
#
# data <- pdata.frame(data, c("dyad","year"), drop = FALSE)
#
# #lead MID # into future
# data$lead1 <- lag(data$cwkeynum, k=1, na.pad=T)
# data$lead2 <- lag(data$cwkeynum, k=2, na.pad=T)
# data$lead3 <- lag(data$cwkeynum, k=3, na.pad=T)
# data$lead4 <- lag(data$cwkeynum, k=4, na.pad=T)
# data$lead5 <- lag(data$cwkeynum, k=5, na.pad=T)
# data$lead6 <- lag(data$cwkeynum, k=6, na.pad=T)
# data$lead7 <- lag(data$cwkeynum, k=7, na.pad=T)
# data$lead8 <- lag(data$cwkeynum, k=8, na.pad=T)
# data$lead9 <- lag(data$cwkeynum, k=9, na.pad=T)
# data$lead10 <- lag(data$cwkeynum, k=10, na.pad=T)
# data$lead11 <- lag(data$cwkeynum, k=11, na.pad=T)
# data$lead12 <- lag(data$cwkeynum, k=12, na.pad=T)
# data$lead13 <- lag(data$cwkeynum, k=13, na.pad=T)
# data$lead14 <- lag(data$cwkeynum, k=14, na.pad=T)
# data$lead15 <- lag(data$cwkeynum, k=15, na.pad=T)
# data$lead16 <- lag(data$cwkeynum, k=16, na.pad=T)
# data$lead17 <- lag(data$cwkeynum, k=17, na.pad=T)
# data$lead18 <- lag(data$cwkeynum, k=18, na.pad=T)
# data$lead19 <- lag(data$cwkeynum, k=19, na.pad=T)
# data$lead20 <- lag(data$cwkeynum, k=20, na.pad=T)
#
# data <- as.data.frame(data)
#
# #lag tc entity
# data$lagtc1 <- lag(data$entity, k=-1, na.pad=T)
# data$lagtc2 <- lag(data$entity, k=-2, na.pad=T)

#R is having memory problems, so let's just export to stata

write.csv(data,"preStata.csv")
rm(data)
gc()

data <- read.csv("postStatasmall.csv")

########## Create indicators for conflict/claim within window

data$window5 <- ifelse(data$lead1 > 0 | data$lead2 > 0 | data$lead3 > 0 | data$lead4 > 0 | data$lead5 > 0, 1, 0)
data$window5[is.na(data$window5)] <- 0

data$window10 <- ifelse(data$lead1 > 0 | data$lead2 > 0 | data$lead3 > 0 | data$lead4 > 0 | data$lead5 > 0 | data$lead6 > 0 | data$lead7 > 0 | data$lead8 > 0 | data$lead9 > 0 | data$lead10 > 0, 1, 0)
data$window10[is.na(data$window10)] <- 0

data$window20 <- ifelse(data$lead1 > 0 | data$lead2 > 0 | data$lead3 > 0 | data$lead4 > 0 | data$lead5 > 0 | data$lead6 > 0 | data$lead7 > 0 | data$lead8 > 0 | data$lead9 > 0 | data$lead10 > 0 | data$lead11 > 0 | data$lead12 > 0 | data$lead13 > 0 | data$lead14 > 0 | data$lead15 > 0 | data$lead16 > 0 | data$lead18 > 0 | data$lead19 > 0 | data$lead20 > 0, 1, 0)
data$window20[is.na(data$window20)] <- 0

drops=c("leaddescribe","lead2","lead3","lead4","lead5","lead6","lead7","lead8","lead9","lead10","lead11","lead12","lead13","lead14","lead15","lead16","lead17","lead18","lead19","lead20")
data <- data[!names(data) %in% drops]
rm(drops)

############### claims windows

data$outstanding.claim5 <- ifelse(!is.na(data$lagtc1) == T & !is.na(data$claim) == T | !is.na(data$lagtc2) == T & !is.na(data$claim) == T | !is.na(data$lagtc3) == T & !is.na(data$claim) == T |  !is.na(data$lagtc4) == T & !is.na(data$claim) == T | !is.na(data$lagtc5) == T & !is.na(data$claim) == T, 1, 0)

data$outstanding.claim10 <- ifelse(!is.na(data$lagtc1) == T & !is.na(data$claim) == T | !is.na(data$lagtc2) == T & !is.na(data$claim) == T | !is.na(data$lagtc3) == T & !is.na(data$claim) == T |  !is.na(data$lagtc4) == T & !is.na(data$claim) == T | !is.na(data$lagtc5) == T & !is.na(data$claim) == T | !is.na(data$lagtc6) == T & !is.na(data$claim) == T | !is.na(data$lagtc7) == T & !is.na(data$claim) == T | !is.na(data$lagtc8) == T & !is.na(data$claim) == T| !is.na(data$lagtc9) == T & !is.na(data$lagtc10) == T & !is.na(data$claim) == T, 1, 0)

data$outstanding.claim20 <- ifelse(!is.na(data$lagtc1) == T & !is.na(data$claim) == T | !is.na(data$lagtc2) == T & !is.na(data$claim) == T | !is.na(data$lagtc3) == T & !is.na(data$claim) == T |  !is.na(data$lagtc4) == T & !is.na(data$claim) == T | !is.na(data$lagtc5) == T & !is.na(data$claim) == T | !is.na(data$lagtc6) == T & !is.na(data$claim) == T | !is.na(data$lagtc7) == T & !is.na(data$claim) == T | !is.na(data$lagtc8) == T & !is.na(data$claim) == T| !is.na(data$lagtc9) == T & !is.na(data$lagtc10) == T & !is.na(data$claim) == T | !is.na(data$lagtc11) == T & !is.na(data$claim) == T | !is.na(data$lagtc12) == T & !is.na(data$claim) == T | !is.na(data$lagtc13) == T & !is.na(data$claim) == T | !is.na(data$lagtc14) == T & !is.na(data$claim) == T | !is.na(data$lagtc15) == T & !is.na(data$claim) == T | !is.na(data$lagtc16) == T & !is.na(data$claim) == T | !is.na(data$lagtc17) == T & !is.na(data$claim) == T | !is.na(data$lagtc18) == T & !is.na(data$claim) == T | !is.na(data$lagtc19) == T & !is.na(data$claim) == T | !is.na(data$lagtc20) == T & !is.na(data$claim) == T, 1, 0)

#clean up

drops <- c("lagtcdescribe","lagtc2","lagtc3","lagtc4","lagtc5","lagtc6","lagtc7","lagtc8","lagtc9","lagtc10","lagtc11","lagtc12","lagtc13","lagtc14","lagtc15","lagtc16","lagtc17","lagtc18","lagtc19","lagtc20")
data <- data[!names(data) %in% drops]
rm(drops)

####### Rivalry ##########

# First fill down MID attributes

library(plyr)
library(zoo)

data$dyad <- paste(data$gainer,data$loser,sep="")

# data <- ddply(data, .(dyad), transform, na.locf(data$cwinit))
# data <- ddply(data, .(dyad), transform, na.locf(data$cwkeynum))

#let's also do this in stata
write.csv(data, "roll.csv")

######### shrink back down to tc as unit of analysis #################

data <- subset(data, !is.na(data$version))

riv <- read.csv("http://dl.dropboxusercontent.com/u/4115584/riv5.10all.csv")

riv$beginr <- substr(riv$beginr, 1, 4)
riv$endr <- substr(riv$endr, 1, 4)

# riv$ab <- paste(riv$rivala,riv$rivalb,sep="")
# riv$ba <- paste(riv$rivalb,riv$rivala,sep="")
# data$ab <- paste(data$gainer,data$loser,sep="")
# data$ba <- paste(data$loser,data$gainer,sep="")

# condense <- function(x){
#   seq(x[5],x[6],by=1)
# }

riv$beginr <- as.numeric(riv$beginr)
riv$endr <- as.numeric(riv$endr)

# riv$all.years <- apply(riv, 1, seq(riv[5],riv[6],by=1))
# 
# riv$all.years <- dlply(riv, .(rivnumb), seq(riv[5],riv[6],by=1))
# 
# for (i in length(riv$rivnumb)){
#   riv$all.years <- list(seq(riv[i,5],riv[i,6],by=1))
# }

data$dyad2 <- ifelse(as.numeric(data$gainer) > as.numeric(data$loser), paste(data$loser,data$gainer,sep=""), paste(data$gainer,data$loser,sep=""))
riv$dyad2 <- ifelse(riv$rivala > riv$rivalb, paste(riv$rivalb,riv$rivala,sep=""), paste(riv$rivala,riv$rivalb,sep=""))

riv <- subset(riv, riv$rivtyp2 != "ISOLATED")

data <- merge(data,riv,by="dyad2",all.x=T,all.y=F)
rm(riv)

# data$rivals <- ifelse(!is.na(data$rivnumb)==T  & (data$year %in% data$all.years)==T, 1, 0)

data$rivals <- ifelse(!is.na(data$rivnumb)==T  & (data$year >= data$beginr)==T & (data$year <= data$endr)==T, 1, 0)

short <- ddply(data, .(gainer,loser,year,month,number), summarize, rivals=max(rivals))

drops <- cbind(data$gainer,data$loser,data$year,data$month,data$number,data$entity)
drops <- as.data.frame(drops)
drops$dup <- ifelse(duplicated(drops) == T, 1, 0)
data$dup <- drops$dup
rm(drops)

df <- data[data$dup==0,]
df$rivals <- short$rivals
rm(short)

#clean up a bit, it won't write the all.years variable
df <- subset(df,select=-c(1:3,76,78))

write.csv(df, "1_2014.csv")
