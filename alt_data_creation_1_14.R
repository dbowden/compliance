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
frame[,c(1:24,27:28)][frame[,c(1:24,27:28)] < 0] <- NA

#limit to territorial MIDs
frame$ter <- ifelse(frame$cwrevt11 == 1 | frame$cwrevt21 == 1 | frame$cwrevt21 == 1 | frame$cwrevt22 == 1 | frame$cwrevt12 == 1, 1, 0)

#standardize country codes and years for merging
frame$dyad <- ifelse(frame$ccode1 > frame$ccode2, paste(frame$ccode2,frame$ccode1,sep=""), paste(frame$ccode1,frame$ccode2,sep=""))
data <- merge(tc, frame, all=T)
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

#convert to yearly observations and merge
all.years <- ddply(icow, .(claimdy), summarize, seq(begclaim, endclaim))
icow <- merge(icow,all.years,all=T)
colnames(icow)[24] <- "year"
icow$dyad <- ifelse(icow$chal > icow$tgt, paste(icow$tgt,icow$chal,sep=""), paste(icow$chal,icow$tgt,sep=""))
data <- merge(data,icow,by=c("dyad","year"),all=T)
rm(icow,all.years)

### Make 1, 5, 10 and 20 year windows for MIDs
