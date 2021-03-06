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
tc[367,24] <- 1 #471 from 475 in 2008
tc[368,24] <- 1 #471 from 475 in 2003
tc[370,24] <- 1 #475 from 471 in 2003

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)

# merge in claims
data <- merge(data, icow, all=T)
rm(icow)

################ Create 5, 10, and 20 year windows for wars ################

library(plyr)

data$dyad <- paste(data$gainer,data$loser,sep="")
data <- data[!is.na(data$year),]
data <- arrange(data, dyad, year)
#data$year = as.Date(data$year,format = "%y",origin = "1816")

data <- ddply(data, .(dyad), transform, lag1 = c(NA, cwkeynum[-length(cwkeynum)]))




######### shrink back down to tc as unit of analysis #################

data <- subset(data, !is.na(data$version))

########## create indicator for ongoing claim #################

data$ongoing <- ifelse(!is.na(lag(data$claim, -1)), 1, 0)

