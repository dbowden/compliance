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

############ Merge data #############

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)

# merge in claims
data <- merge(data, icow, all=T)
rm(icow)

################ Create 5, 10, and 20 year windows for wars ################

#lead years of terr mids
data$mid0 <- ifelse(data$ter == 1, data$year, 0)
data$mid1 <- ifelse(data$ter ==1, lag(data$year, -1), 0)
data$mid2 <- ifelse(data$ter ==1, lag(data$year, -2), 0)
data$mid3 <- ifelse(data$ter ==1, lag(data$year, -3), 0)
data$mid4 <- ifelse(data$ter ==1, lag(data$year, -4), 0)
data$mid5 <- ifelse(data$ter ==1, lag(data$year, -5), 0)
data$mid6 <- ifelse(data$ter ==1, lag(data$year, -6), 0)
data$mid7 <- ifelse(data$ter ==1, lag(data$year, -7), 0)
data$mid8 <- ifelse(data$ter ==1, lag(data$year, -8), 0)
data$mid9 <- ifelse(data$ter ==1, lag(data$year, -9), 0)
data$mid10 <- ifelse(data$ter ==1, lag(data$year, -10), 0)
data$mid11 <- ifelse(data$ter ==1, lag(data$year, -11), 0)
data$mid12 <- ifelse(data$ter ==1, lag(data$year, -12), 0)
data$mid13 <- ifelse(data$ter ==1, lag(data$year, -13), 0)
data$mid14 <- ifelse(data$ter ==1, lag(data$year, -14), 0)
data$mid15 <- ifelse(data$ter ==1, lag(data$year, -15), 0)
data$mid16 <- ifelse(data$ter ==1, lag(data$year, -16), 0)
data$mid17 <- ifelse(data$ter ==1, lag(data$year, -17), 0)
data$mid18 <- ifelse(data$ter ==1, lag(data$year, -18), 0)
data$mid19 <- ifelse(data$ter ==1, lag(data$year, -19), 0)
data$mid20 <- ifelse(data$ter ==1, lag(data$year, -20), 0)

#if year is equal to leads, a mid occurred within the window
data$w5 <- ifelse(is.na(data$year) == T, 0, ifelse(data$year == data$mid0 | data$year == data$mid1 | data$year == data$mid2 | data$year == data$mid3 | data$year == data$mid4 | data$year == data$mid5, 1, 0))

data$w10 <- ifelse(is.na(data$year) == T, 0, ifelse(data$year == data$mid0 | data$year == data$mid1 | data$year == data$mid2 | data$year == data$mid3 | data$year == data$mid4 | data$year == data$mid5 | data$year == data$mid6 | data$year == data$mid7 | data$year == data$mid8 | data$year == data$mid9 | data$year == data$mid10, 1, 0))


##### create windows

#create leads to drop cases that aren't within 20 years of a transfer
tc$lead0 <- (tc$year)
tc$lead1 <- lag(tc$year, -1)
tc$lead2 <- lag(tc$year, -2)
tc$lead3 <- lag(tc$year, -3)
tc$lead4 <- lag(tc$year, -4)
tc$lead5 <- lag(tc$year, -5)
tc$lead6 <- lag(tc$year, -6)
tc$lead7 <- lag(tc$year, -7)
tc$lead8 <- lag(tc$year, -8)
tc$lead9 <- lag(tc$year, -9)
tc$lead10 <- lag(tc$year, -10)
tc$lead11 <- lag(tc$year, -11)
tc$lead12 <- lag(tc$year, -12)
tc$lead13 <- lag(tc$year, -13)
tc$lead14 <- lag(tc$year, -14)
tc$lead15 <- lag(tc$year, -15)
tc$lead16 <- lag(tc$year, -16)
tc$lead17 <- lag(tc$year, -17)
tc$lead18 <- lag(tc$year, -18)
tc$lead19 <- lag(tc$year, -19)
tc$lead20 <- lag(tc$year, -20)

######### Merge ###########################

#merge tc into frame
data <- merge(tc, frame, all=T)
rm(tc,frame)

#remove missing years
data <- data[!is.na(data$year),]

#get rid of cases that aren't within 20 years of transfer
data$t0 = ifelse(is.na(data$lead0) == T, 0, ifelse(data$lead0 == data$year, 1, 0))
data$t1 = ifelse(is.na(data$lead1) == T, 0, ifelse(data$lead1 == data$year, 1, 0))
data$t2 = ifelse(is.na(data$lead2) == T, 0, ifelse(data$lead2 == data$year, 1, 0))
data$t3 = ifelse(is.na(data$lead3) == T, 0, ifelse(data$lead3 == data$year, 1, 0))
data$t4 = ifelse(is.na(data$lead4) == T, 0, ifelse(data$lead4 == data$year, 1, 0))
data$t5 = ifelse(is.na(data$lead5) == T, 0, ifelse(data$lead5 == data$year, 1, 0))
data$t6 = ifelse(is.na(data$lead6) == T, 0, ifelse(data$lead6 == data$year, 1, 0))
data$t7 = ifelse(is.na(data$lead7) == T, 0, ifelse(data$lead7 == data$year, 1, 0))
data$t8 = ifelse(is.na(data$lead8) == T, 0, ifelse(data$lead8 == data$year, 1, 0))
data$t9 = ifelse(is.na(data$lead9) == T, 0, ifelse(data$lead9 == data$year, 1, 0))
data$t10 = ifelse(is.na(data$lead10) == T, 0, ifelse(data$lead10 == data$year, 1, 0))
data$t11 = ifelse(is.na(data$lead11) == T, 0, ifelse(data$lead11 == data$year, 1, 0))
data$t12 = ifelse(is.na(data$lead12) == T, 0, ifelse(data$lead12 == data$year, 1, 0))
data$t13 = ifelse(is.na(data$lead13) == T, 0, ifelse(data$lead13 == data$year, 1, 0))
data$t14 = ifelse(is.na(data$lead14) == T, 0, ifelse(data$lead14 == data$year, 1, 0))
data$t15 = ifelse(is.na(data$lead15) == T, 0, ifelse(data$lead15 == data$year, 1, 0))
data$t16 = ifelse(is.na(data$lead16) == T, 0, ifelse(data$lead16 == data$year, 1, 0))
data$t17 = ifelse(is.na(data$lead17) == T, 0, ifelse(data$lead17 == data$year, 1, 0))
data$t18 = ifelse(is.na(data$lead18) == T, 0, ifelse(data$lead18 == data$year, 1, 0))
data$t19 = ifelse(is.na(data$lead19) == T, 0, ifelse(data$lead19 == data$year, 1, 0))
data$t20 = ifelse(is.na(data$lead20) == T, 0, ifelse(data$lead20 == data$year, 1, 0))



keeps = c("year", "loser", "gainer", "gaintype", "procedur", "entity", "contgain", "area", "pop", "portion", "losetype", "contlose", "entry", "exit", "number", "indep", "conflict", "version", "non.pleb", "agreement", "w5", "w10", "w20")

data = data[,(names(data) %in% keeps)]
rm(keeps)