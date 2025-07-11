### 
# Analysis of inshore scallop fishery logbook records related to NAJFM Manuscript "Accuracy of using calculated speeds with Vessel Monitoring System data: implications for fishing activity metrics"
# J.Sameoto 2025
#

library(tidyverse)


logs <- read.csv(./"logbookdata.csv")
dim(logs)

#Add year column:
logs$DATE_FISHED <- as.Date(logs$DATE_FISHED, format="%Y-%m-%d")  
logs$YEAR <- as.numeric(format(logs$DATE_FISHED, "%Y")) 

#Check data:
table(logs$YEAR) 
table(logs$FLEET) 

#subset to only full bay
logs %>% distinct(FLEET)
logs <- logs %>% filter(FLEET == "Full Bay")



#--- summarize effort ---- 
logs$effort_h <- (logs$NUM_OF_TOWS*logs$AVG_TOW_TIME)/60

eff.by.trip <- logs %>% group_by(TRIP_ID) %>% summarise(total.effort.h = sum(effort_h)) 

fleet.trip <- logs %>% distinct(TRIP_ID, FLEET)

trip.effort.fleet <- merge(eff.by.trip,fleet.trip, by=c("TRIP_ID")) 


#---- daily effort - hours fished in a day for a given trip ---- 
eff.by.trip.date <- logs %>% group_by(TRIP_ID, DATE_FISHED, YEAR) %>% summarise(total.effort.h = sum(effort_h)) 

mean(eff.by.trip.date$total.effort.h)
sd(eff.by.trip.date$total.effort.h)

eff.by.trip.date %>% group_by(YEAR) %>% summarise(mean(total.effort.h), sd(total.effort.h))


#--- trip duration, i.e. total trip time in days ---- 
logs$date.landed <- as.Date(logs$DATE_LANDED)


trips <- unique(logs$TRIP_ID)
trip.days.out <- data.frame(TRIP_ID = trips, trip.hours = NA)


#for each trip calculate number of days in trip using land date and first fished date 
for (i in 1:length(trips)){
  temp <- logs %>% filter(TRIP_ID == trips[i])
trip.days.out$trip.hours[trip.days.out$TRIP_ID == trips[i]] <- as.numeric(temp %>% summarise(trip.time.h = difftime(max(temp$date.landed, na.rm = TRUE),  min(temp$DATE_FISHED, na.rm = TRUE), units = c("hours"))) )
} 
trip.days.out$trip.day <- trip.days.out$trip.hours/24


effort.by.trip <- logs %>% group_by(TRIP_ID, YEAR) %>% summarise(fished.hours = sum(effort_h))

trip.all <- merge(trip.days.out,effort.by.trip, by=c("TRIP_ID") )
trip.all$prop.fished <- trip.all$fished.hours/trip.all$trip.hours




#--- tow time ---- 
logs %>% summarise(mean(AVG_TOW_TIME), sd(AVG_TOW_TIME))

logs %>% group_by(YEAR) %>% summarise(mean(AVG_TOW_TIME), sd(AVG_TOW_TIME))



#--- tow distance assuming average reported speed range from interviews ---- 
#Using Average tow time (converted to hours) 1 km/h = 0.539957 knots   
tow.speed <- 2.5
#tow speed in knots, tow time in minutes; get tow distance in km 

logs$tow.length.logs <- (logs$AVG_TOW_TIME/60) * (tow.speed/0.539957)

logs %>% summarise(mean(tow.length.logs), sd(tow.length.logs))

logs %>% group_by(YEAR) %>% summarise(mean(tow.length.logs), sd(tow.length.logs))






