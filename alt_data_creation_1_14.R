###new approach that involves sql lookups instead of merging everything
tc <- read.csv("http://dl.dropbox.com/u/4115584/tc2008.csv")
courts <- read.delim("http://dl.dropbox.com/u/4115584/huthallee")

library(plyr)

###First clean up territorial change data and add courts variable
#standardize NA codes
tc[tc == "."] <- NA
tc[tc == -9] <- NA

#narrow to state-to-state transfers
tc <- subset(tc, indep == 0) #remove territories that became independent
tc <- subset(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- subset(tc, loser > 0) #remove cases with NA for losing state

#remove plebiscites
tc$agreement <- ifelse(tc$procedur == 3, 1, 0)

# recode cessions that are plebscites as determined by COW code sheets and Beigbeder (1994). Limiting to state-to-state gets rid of most of them.
tc[289,21] <- 0 #390 from 255 in 1920
tc[295,21] <- 0 #310 from 305 in 1921
tc[277,21] <- 0 #305 from 345 in 1920
tc[377,21] <- 0 #750 from 220 in 1950
tc[93,21] <- 0 #325 from 327 in 1870
tc[407,21] <- 0 #471 from 200 in 1961
tc[416,21] <- 0 #820 from 200 in 1963
tc[417,21] <- 0 #820 from 200 in 1963

#code more recent cases (our coding)
tc[485,21] <- 1 #471 from 475 in 2008
tc[482,21] <- 1 #471 from 475 in 2003
tc[483,21] <- 1 #475 from 471 in 2003

#remove violence
tc$nonviolent.agreement <- ifelse(tc$conflict == 0 & tc$agreement == 1, 1, 0)

#clean up huth and allee
courts <- subset(courts, arbadj2 == 1)
colnames(courts)[1] <- "gainer"
colnames(courts)[2] <- "loser"
courts$year.2 <- substr(courts$bdatepog, 2, 3)
courts$year.2 <- as.numeric(courts$year.2)
keeps <- c("gainer","loser","year.2","arbadj2")
courts <- courts[names(courts) %in% keeps]
rm(keeps)

#create arbadj variable in tc
tc$year.2 <- ifelse(tc$year >= 1919, substr(tc$year, 3, 4), NA)
tc$year.2 <- as.numeric(tc$year.2)
courts$year.1 <- courts$year.2 - 1
courts$year.0 <- courts$year.2 - 2
courts[3,5] <- 99
courts[3,6] <- 98
courts[29,5] <- 99
courts[29,6] <- 98
tc$arbadj2 <- ifelse((tc$year.2 %in% courts$year.2 | tc$year.2 %in% courts$year.1 | tc$year.2 %in% courts$year.0) & (tc$gainer %in% courts$gainer | tc$gainer %in% courts$loser) & (tc$loser %in% courts$loser | tc$loser %in% courts$gainer), 1, 0)
tc$arbadj0 <- ifelse(tc$year.2 %in% courts$year.2 & (tc$gainer %in% courts$gainer | tc$gainer %in% courts$loser) & (tc$loser %in% courts$loser | tc$loser %in% courts$gainer), 1, 0)
rm(courts)

#drop non-states (this should be near the top, but I realized too late that I missed some non-state actors and don't want to adjust the hand coding)
tc <- subset(tc, gainer > 1)
tc <- subset(tc, loser > 1)
#at this point there are 472 observations

#remove unneeded vars
tc <- subset(tc, select = -c(V21,year.2))

###Create rivalry variable
riv <- read.csv("http://dl.dropboxusercontent.com/u/4115584/riv5.10all.csv")

#standardize rivalry years
riv$beginr <- as.numeric(substr(riv$beginr, 1, 4))
riv$endr <- as.numeric(substr(riv$endr, 1, 4))

#convert rivalry to dyad years and merge
all.years <- ddply(riv, .(rivnumb), summarize, seq(beginr, endr))
riv <- merge(riv,all.years,all=T)
riv <- subset(riv, select = -c(beginr,endr))
colnames(riv)[8] <- "year"
tc$dyad <- ifelse(tc$gainer > tc$loser, paste(tc$loser,tc$gainer,sep=""), paste(tc$gainer,tc$loser,sep=""))
riv$dyad <- ifelse(riv$rivala > riv$rivalb, paste(riv$rivalb,riv$rivala,sep=""), paste(riv$rivala,riv$rivalb,sep=""))
riv <- subset(riv, select = -c(rivala,rivalb))
tc <- merge(tc,riv,all.x=T,all.y=F)
rm(riv,all.years)

###Clean up the MIDs data
frame <- read.csv("http://dl.dropboxusercontent.com/u/4115584/frame1_14.csv")

frame[frame < -10] <- NA
frame[,1:30][frame[,1:30] < 0] <- NA

#limit to territorial MIDs
frame$ter <- ifelse(frame$cwrevt11 == 1 | frame$cwrevt21 == 1 | frame$cwrevt21 == 1 | frame$cwrevt22 == 1 | frame$cwrevt12 == 1, 1, 0)

#standardize country codes and years for merging
frame$dyad <- ifelse(frame$ccode1 > frame$ccode2, paste(frame$ccode2,frame$ccode1,sep=""), paste(frame$ccode1,frame$ccode2,sep=""))
data <- merge(tc, frame,all=T)
rm(frame,tc)

#make indicator for MID initiator/side A
data$loser.init <- ifelse(data$loser==data$ccode1, 1, 0)
data <- subset(data, select=-c(ccode1,ccode2))

### Merge in Claims Data

#clean data
icow <- read.csv("http://dl.dropboxusercontent.com/u/4115584/ICOWprov10.csv")
icow <- subset(icow, select = -version)
icow$endclaim[icow$endclaim == -9] <- 201312
icow[icow == -9] <- NA
icow$begclaim <- as.numeric(substr(icow$begclaim, 1, 4))
icow$endclaim <- as.numeric(substr(icow$endclaim, 1, 4))
icow$dyad <- ifelse(icow$chal > icow$tgt, paste(icow$tgt,icow$chal,sep=""), paste(icow$chal,icow$tgt,sep=""))

#convert to dyad years
all.years <- ddply(icow, .(claimdy), summarize, seq(begclaim, endclaim))
colnames(all.years)[2] <- "year"
icow <- merge(icow,all.years,all=T)
rm(all.years)

#remove multiple claims per dyad year. we're not using any claim attributes, so we'll just keep the first one listed.
dupes <- subset(icow,select=c(dyad,year))
dupes$dupes <- duplicated(dupes)
icow$dupes <- dupes$dupes
rm(dupes)
icow <- subset(icow,dupes==FALSE)
icow <- subset(icow,select=-c(dupes,dyadnum))

#merge
data <- merge(data,icow,by=c("dyad","year"),all.x=T,all.y=F)
rm(icow)

### Make 1, 5, 10 and 20 year windows for MIDs

#my approach is to reverse lag the mid number

#I'm running into memory problems, so I'll trim the dataset down a bit temporarily
write.csv(data,"1_14.csv") #we'll merge this back in later
data <- subset(data, select=c(dyad,year,cwkeynum,claimdy))

#can't find a good way to do reverse lags in panel data in R, so we'll export to Stata for this step
write.csv(data,"pre_stata_slim.csv")
#see stata do file for commands
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
#data$contiguous <- ifelse(data$contig==1 | (data$contlose==0 & data$colcont==1), 1, 0)

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

#and let's see if we can fill in some missing values on the contiguity variable
contig <- read.csv("http://dl.dropboxusercontent.com/u/4115584/contdir.csv")
contig2 <- ifelse(data$dyad %in% contig$dyad, contig$conttype, NA)
data$contig2 <- contig2
data$cont <- ifelse(is.na(data$contig)==T, data$contig2, data$contig)
rm(contig,contig2)
data <- subset(data,select=-c(contig,contig2,X,X.2))

#and write the data
write.csv(data,"tc_1_14.csv")
