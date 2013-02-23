#### Code to create data for Bowden and Diehl (2013) paper to be presented at Brazil Compliance Conference ###

############ load and clean data ##########
tc <- read.csv("http://dl.dropbox.com/u/4115584/tc2008.csv")
icow <- read.csv("http://dl.dropbox.com/u/4115584/ICOW-Diehl.csv")
frame <- read.csv("http://dl.dropbox.com/u/4115584/frame.csv")

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

#lead years of terr mids
#data$mid0 <- ifelse(data$ter == 1, data$year, 0)
data$mid1 <- ifelse(data$ter == 1, lag(data$year, 1), 0) 
data$mid2 <- ifelse(data$ter == 1, lag(data$year, 2), 0)
data$mid3 <- ifelse(data$ter == 1, lag(data$year, 3), 0)
data$mid4 <- ifelse(data$ter == 1, lag(data$year, 4), 0)
data$mid5 <- ifelse(data$ter == 1, lag(data$year, 5), 0)
data$mid6 <- ifelse(data$ter == 1, lag(data$year, 6), 0)
data$mid7 <- ifelse(data$ter == 1, lag(data$year, 7), 0)
data$mid8 <- ifelse(data$ter == 1, lag(data$year, 8), 0)
data$mid9 <- ifelse(data$ter == 1, lag(data$year, 9), 0)
data$mid10 <- ifelse(data$ter == 1, lag(data$year, 10), 0)
data$mid11 <- ifelse(data$ter == 1, lag(data$year, 11), 0)
data$mid12 <- ifelse(data$ter ==1, lag(data$year, 12), 0)
data$mid13 <- ifelse(data$ter ==1, lag(data$year, 13), 0)
data$mid14 <- ifelse(data$ter ==1, lag(data$year, 14), 0)
data$mid15 <- ifelse(data$ter ==1, lag(data$year, 15), 0)
data$mid16 <- ifelse(data$ter ==1, lag(data$year, 16), 0)
data$mid17 <- ifelse(data$ter ==1, lag(data$year, 17), 0)
data$mid18 <- ifelse(data$ter ==1, lag(data$year, 18), 0)
data$mid19 <- ifelse(data$ter ==1, lag(data$year, 19), 0)
data$mid20 <- ifelse(data$ter ==1, lag(data$year, 20), 0)

#if year is equal to leads, a mid occurred within the window
data$w5 <- ifelse(is.na(data$year) == T, 0, ifelse(data$year == data$mid1 | data$year == data$mid2 | data$year == data$mid3 | data$year == data$mid4 | data$year == data$mid5, 1, 0))

data$w10 <- ifelse(is.na(data$year) == T, 0, ifelse(data$year == data$mid1 | data$year == data$mid2 | data$year == data$mid3 | data$year == data$mid4 | data$year == data$mid5 | data$year == data$mid6 | data$year == data$mid7 | data$year == data$mid8 | data$year == data$mid9 | data$year == data$mid10, 1, 0))

data$w20 <- ifelse(is.na(data$year) == T, 0, ifelse(data$year == data$mid1 | data$year == data$mid2 | data$year == data$mid3 | data$year == data$mid4 | data$year == data$mid5 | data$year == data$mid6 | data$year == data$mid7 | data$year == data$mid8 | data$year == data$mid9 | data$year == data$mid10 | data$year == data$mid11 | data$year == data$mid12 | data$year == data$mid13 | data$year == data$mid14 | data$year == data$mid15 | data$year == data$mid16 | data$year == data$mid17 | data$year == data$mid18 | data$year == data$mid19 | data$year == data$mid20, 1, 0))

# clean up
drops <- c("mid0","mid1","mid2","mid3","mid4","mid5","mid6","mid7","mid8","mid9","mid10","mid11","mid12","mid13","mid14","mid15","mid16","mid17","mid18","mid19","mid20","V21","dyad")
data <- data[,!(names(data) %in% drops)]
rm(drops)

# at some point I want to fill down MID info. Zoo or maybe google refine.

######### shrink back down to tc as unit of analysis #################

data <- subset(data, !is.na(data$version))

########## create indicator for ongoing claim #################

data$ongoing <- ifelse(!is.na(lag(data$claim, -1)), 1, 0)

m1 = glm(ongoing ~ non.pleb, data=data,family=binomial(logit))
summary(m1)
m2 = glm(ongoing ~ agreement, data=data,family=binomial(logit))
summary(m2)
m3 = glm(ongoing ~ non.pleb + contlose + polity21,data=data,family=binomial(logit))
summary(m3)


summary(glm(w20 ~ agreement, data=data,family=binomial(logit)))
summary(glm(w10 ~ agreement, data=data,family=binomial(logit)))
