#### Code to create data for Bowden and Diehl (2013) paper to be presented at Brazil Compliance Conference ###

############ load and clean data ##########
tc <- read.csv("http://dl.dropbox.com/u/4115584/tc2008.csv")
icow <- read.csv("http://dl.dropbox.com/u/4115584/ICOW-Diehl.csv")
frame <- read.csv("http://dl.dropbox.com/u/4115584/frame.csv")
courts <- read.delim("http://dl.dropbox.com/u/4115584/huthallee")

#standardize NA codes
tc[tc == "."] <- NA
tc[tc == -9] <- NA
icow[icow == -9] <- NA
frame[frame < -10] <- NA

#narrow to state-to-state transfers
tc <- subset(tc, indep == 0) #remove territories that became independent
tc <- subset(tc, exit == 0) #remove territories that were absorbed after state deaths
tc <- subset(tc, loser > 0) #remove cases with NA for losing state

#remove plebiscites
tc$non.pleb <- ifelse(tc$procedur == 3, 1, 0) 

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
tc$agreement <- ifelse(tc$conflict == 0 & tc$non.pleb == 1, 1, 0)

#limit to territorial MIDs
frame$ter <- ifelse(frame$cwrevt11 == 1 | frame$cwrevt21 == 1 | frame$cwrevt21 == 1 | frame$cwrevt22 == 1 | frame$cwrevt12 == 1, 1, 0)

#standardize country codes and years for merging
colnames(icow)[4] <- "loser"
colnames(icow)[5] <- "gainer" #change challenger and target to tc terminology
colnames(frame)[1] <- "loser"
colnames(frame)[2] <- "gainer"

#function to truncate numeric strings (icow has year + month as one value)
trncint <- function(x, left=0,length=0)  (x %% 10^(left) ) %/%   
  10^(left-length)
#note that here I am only using new claims
icow$begclaim <- trncint(icow$begclaim, 6, 4) #6 is position of first digit in 6 digit year+mo, keep 4 digits
icow$endclaim <- trncint(icow$endclaim, 6, 4)
#colnames(icow)[6] <- "year" #this would capture new disputes
colnames(icow)[7] <- "year" #this captures ongoing claims
#icow$year[is.na(icow$year)] <- 2030

############ Merge data #############

#clean up huth and allee
courts <- subset(courts, arbadj2 == 1)
colnames(courts)[1] <- "gainer"
colnames(courts)[2] <- "loser"
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

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)

# merge in claims
data <- merge(data, icow, all=T)
rm(icow)

################ Create 5, 10, and 20 year windows for wars ################

library(plm)

data$dyad <- paste(data$gainer,data$loser,sep="")

lags <- pdata.frame(data, index=c("dyad","year"))
lags <- lags[!is.na(lags$year),]

#lead years of terr mids
lags$mid0 <- ifelse(lags$ter == 1, lags$year, 0)
lags$mid1 <- ifelse(lags$ter == 1, lag(factor(lags$year, levels=1816:2029), 1), 0) 
lags$mid2 <- ifelse(lags$ter == 1, lag(factor(lags$year, levels=1816:2029), 2), 0)
lags$mid3 <- ifelse(lags$ter == 1, lag(factor(lags$year, levels=1816:2029), 3), 0)
lags$mid4 <- ifelse(lags$ter == 1, lag(factor(lags$year, levels=1816:2029), 4), 0)
lags$mid5 <- ifelse(lags$ter == 1, lag(factor(lags$year, levels=1816:2029), 5), 0)
lags$mid6 <- ifelse(lags$ter == 1, lag(lags$year, 6), 0)
lags$mid7 <- ifelse(lags$ter == 1, lag(lags$year, 7), 0)
lags$mid8 <- ifelse(lags$ter == 1, lag(lags$year, 8), 0)
lags$mid9 <- ifelse(lags$ter == 1, lag(lags$year, 9), 0)
lags$mid10 <- ifelse(lags$ter == 1, lag(lags$year, 10), 0)
lags$mid11 <- ifelse(lags$ter == 1, lag(lags$year, 11), 0)
lags$mid12 <- ifelse(lags$ter ==1, lag(lags$year, 12), 0)
lags$mid13 <- ifelse(lags$ter ==1, lag(lags$year, 13), 0)
lags$mid14 <- ifelse(lags$ter ==1, lag(lags$year, 14), 0)
lags$mid15 <- ifelse(lags$ter ==1, lag(lags$year, 15), 0)
lags$mid16 <- ifelse(lags$ter ==1, lag(lags$year, 16), 0)
lags$mid17 <- ifelse(lags$ter ==1, lag(lags$year, 17), 0)
lags$mid18 <- ifelse(lags$ter ==1, lag(lags$year, 18), 0)
lags$mid19 <- ifelse(lags$ter ==1, lag(lags$year, 19), 0)
lags$mid20 <- ifelse(lags$ter ==1, lag(lags$year, 20), 0)

#if year is equal to leads, a mid occurred within the window
lags$w5 <- ifelse(is.na(lags$mid0) == T | is.na(lags$mid1) == T | is.na(lags$mid2) == T | is.na(lags$mid3) == T | is.na(lags$mid4) == T | is.na(lags$mid5) == T, 0, ifelse(lags$year == lags$mid0 | lags$year == lags$mid1 | lags$year == lags$mid2 | lags$year == lags$mid3 | lags$year == lags$mid4 | lags$year == lags$mid5, 1, 0))

lags$w10 <- ifelse(is.na(lags$year) == T, 0, ifelse(lags$year == lags$mid1 | lags$year == lags$mid2 | lags$year == lags$mid3 | lags$year == lags$mid4 | lags$year == lags$mid5 | lags$year == lags$mid6 | lags$year == lags$mid7 | lags$year == lags$mid8 | lags$year == lags$mid9 | lags$year == lags$mid10, 1, 0))

lags$w20 <- ifelse(is.na(lags$year) == T, 0, ifelse(lags$year == lags$mid1 | lags$year == lags$mid2 | lags$year == lags$mid3 | lags$year == lags$mid4 | lags$year == lags$mid5 | lags$year == lags$mid6 | lags$year == lags$mid7 | lags$year == lags$mid8 | lags$year == lags$mid9 | lags$year == lags$mid10 | lags$year == lags$mid11 | lags$year == lags$mid12 | lags$year == lags$mid13 | lags$year == lags$mid14 | lags$year == lags$mid15 | lags$year == lags$mid16 | lags$year == lags$mid17 | lags$year == lags$mid18 | lags$year == lags$mid19 | lags$year == lags$mid20, 1, 0))

# clean up
drops <- c("mid0","mid1","mid2","mid3","mid4","mid5","mid6","mid7","mid8","mid9","mid10","mid11","mid12","mid13","mid14","mid15","mid16","mid17","mid18","mid19","mid20","V21","dyad")
data <- data[,!(names(data) %in% drops)]
rm(drops)

# at some point I want to fill down MID info. Zoo or maybe google refine.

######### shrink back down to tc as unit of analysis #################

data <- subset(data, !is.na(data$version))

########## create indicator for ongoing claim #################

data$ongoing <- ifelse(!is.na(lag(data$claim, -1)), 1, 0)

