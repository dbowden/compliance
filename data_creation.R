#### Code to create data for Bowden and Diehl (2013) paper to be presented at Brazil Compliance Conference

############## load packages ###########
library(plyr)

############ load and clean data ##########
tc <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/tc2008.csv")
icow <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/ICOW-Diehl.csv")
frame <- read.csv("C:/Users/David/Desktop/Dropbox/RA Work/Compliance Paper/data/frame.csv")

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
icow$year[is.na(icow$year)] <- 2030
rm(trncint)

############ Merge data #############

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)

# merge in claims
data <- merge(data, icow, all=T)
rm(icow)

################ Create 5, 10, and 20 year windows for wars ################

# reorder data frame
data$dyad <- paste(data$gainer,data$loser,sep="")
data <- arrange(data, dyad, year)
