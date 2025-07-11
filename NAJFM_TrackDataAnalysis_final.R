###
##  Data Analysis -  NAJFM Manuscript "Accuracy of using calculated speeds with Vessel Monitoring System data: implications for fishing activity metrics"
## J.Sameoto 2025
## 


library(tidyverse)
library(ggplot2)
library(sp)
library(sf)
library(lubridate)
library(amt)
library(ggspatial)


options(tibble.width = Inf)

## set wd
setwd(".")

#read in 1-min data from 12 trips 
orig.dat.outside.port <- read.csv("orig.dat.outside.port.csv")
dim(orig.dat.outside.port)
str(orig.dat.outside.port)

orig.dat.outside.port$vmsdate <- as.Date(orig.dat.outside.port$vmsdate)         
orig.dat.outside.port$vmstime <- as_datetime(orig.dat.outside.port$vmstime)         

#----  calc speed and turning angle and segments (amt) ---- 
#  1-min GPS data 
track.dat <- orig.dat.outside.port  
is.data.frame(track.dat)
#check for duplications 
any(duplicated(track.dat$vmstime)) #none 
tr1 <- make_track(track.dat, lon, lat, vmstime, id = Anon.ID,  crs = 4326 ) 
#convert to utm 
tr1 <- transform_coords(tr1, crs_to=32620)
summarize_sampling_rate(tr1)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defaults, turning angle in radians and between -pi and pi, east = zero
tr2 <- tr1 %>% nest(data = -"id")
tr2
tr3 <- tr2 %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr3)


#Pull out steps by trip.id.vessel 
out <- tr3 %>% select(id, steps) %>% unnest(cols = steps)
table(out$id)
out$sec <- as.numeric(out$dt_) 
out$speed_kmh <- (out$sl_/1000)/(out$sec/3600)
#1 km/h = 0.539957 knots 
out$speed_knots <- out$speed_kmh*0.539957

#convert turning angle to degrees 
out <- out %>% mutate(ta_degrees = as_degree(ta_))
out$abs_tadegrees <- abs(out$ta_degrees)

summary(out$ta_degrees)

#Speed frequency plot - 1-min GPS data ---- 
speed.freq.plot <- ggplot(out , aes(x=speed_knots)) + 
  geom_histogram(binwidth = 0.1) + #, color="black", fill="grey"
  theme_bw() + xlab("Speed (knots)") + ylab("Frequency") + facet_wrap(~id, scales = "free_y") +
  xlim(0, 12.5) + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=12))
speed.freq.plot


#---- Categorize Fishing activity based on speed ---- 
# if > 4knots you're steaming, if < 1 knot you're not fishing, if >= 1 or <= 4 knots you're fishing. 
out$Action <- NA 
out$Action[out$speed_knots > 4] <- "steaming" # also not fishing
out$Action[out$speed_knots < 1] <- "not fishing"
out$Action[!out$Action %in% c("steaming","not fishing")] <- "fishing"

out %>% group_by(Action) %>% summarise(n())




# ---- Identify Tows and calculate tow time to compare to logbooks ----
# create a new column called "TowId" and set it to 0 df$TowId <- 0
# For each unique "TRIP_ID" value, we want to identify all the groups of rows that have "fishing" as the action we will use the "TowId" column to identify these groups

list.id <- unique(out$id)
 

#create empty dataframe for data 
out.df <- out
out.df$TowId <- NA 
out.df <- out.df[1,]
out.df <- out.df[-1,]
colnames(out.df)

for(k in 1:length(list.id)){
  
  df <- out[out$id == list.id[k],]
  
  previous <- ''
  prevous_tripid <- 0
  counter <- 0
  
  for (i in 1:nrow(df)) {
    if (df[i, 'id'] != prevous_tripid) {
      counter <- 0
      prevous_tripid <- df[i, 'id']
    }
    if (df[i, 'Action'] == 'fishing') {
      if (previous == 'fishing') {
        df[i, 'TowId'] <- counter
      } else {
        counter <- counter + 1
        df[i, 'TowId'] <- counter
      }
    }
    previous <- df[i, 'Action']
  }
  
  
  out.df <- rbind(out.df,df)
} 



#create unique trip and towID since TowId is repeated within each id (where id identifies a unique trip)
out.df$id.TowId <- paste0(out.df$id, ".", out.df$TowId)
out.df$id.TowId[is.na(out.df$TowId)] <- NA

#Just look at those records tagged as fishing 
df.fishing <- out.df[!(is.na(out.df$TowId)),]
#Calculate time of each unique tow; time from start to end of each id.TowId
fishing.tow.times <- df.fishing %>% group_by(id, id.TowId) %>% summarise(tow.length.hr = as.numeric(difftime(max(t2_, na.rm = TRUE),  min(t1_, na.rm = TRUE), units = c("hours")))) 



### evaluate individual tows in GIS


#Based on review in GIS and feedback, reclass those tows <= 10 min not tows OR > 50 min not tows (99.2% of logs report between 10 and 50 min tows)
not.tows <- fishing.tow.times[fishing.tow.times$tow.length.hr <= (10/60) | fishing.tow.times$tow.length.hr > (50/60),]

#relabel to remove towIds that are not real tows 
out.df$TowIdverified <- out.df$id.TowId
out.df$TowIdverified[out.df$id.TowId %in% not.tows$id.TowId] <- NA

#create unique id 
out.df$uid <- rownames(out.df)


#tows 
fishing.df <- out.df[!is.na(out.df$TowIdverified),]
length(unique(fishing.df$TowIdverified))



# ---- Tow length/distance from olex data ---- 
tows.dist <- fishing.df %>% group_by(id, TowIdverified) %>% summarise(tow.length.dist.m = sum(sl_))

mean(tows.dist$tow.length.dist.m)
sd(tows.dist$tow.length.dist.m)


# ---- tow time within each trip ---- 
tows.hr <- fishing.df %>% group_by(id, TowIdverified) %>% summarise(tow.length.hr = as.numeric(difftime(max(t2_, na.rm = TRUE),  min(t1_, na.rm = TRUE), units = c("hours")))) 

tows.hr$tow.length.min <- tows.hr$tow.length.hr*60


#average tow time from olex GPS data at 1 min 
mean(tows.hr$tow.length.min)  
sd(tows.hr$tow.length.min) 


# ---- hours towed per trip per day ----

fishing.df$date <- date(fishing.df$t1_ )

tows.hr.per.day <- fishing.df %>% group_by(id, date, TowIdverified) %>% summarise(tow.length.hr = as.numeric(difftime(max(t2_, na.rm = TRUE),  min(t1_, na.rm = TRUE), units = c("hours")))) 
tows.hours.per.trip <- tows.hr.per.day %>% group_by(id, date ) %>% summarise(total.hours.towed = sum(tow.length.hr))

mean(tows.hours.per.trip$total.hours.towed)
sd(tows.hours.per.trip$total.hours.towed)


#---- trip duration ---- 
trip.length <- fishing.df %>% group_by(id) %>% summarise(trip.length.hr = as.numeric(difftime(max(t2_, na.rm = TRUE),  min(t1_, na.rm = TRUE), units = c("hours")))) 
trip.length$days <- trip.length$trip.length.hr/24

#trip duration in days 
mean(trip.length$days)
sd(trip.length$days)



#---- Relabelled as fishing or not fishing -- binary categories ---- 
#fishing is all records where TowIdverified is not NA; else not fishing 
out.df$fishing <- NA
out.df$fishing[is.na(out.df$TowIdverified)] <- "Not fishing"
out.df$fishing[!is.na(out.df$TowIdverified)] <- "Fishing"

# Add category of turning based on data review 
out.df$ActionFinal <- NA 
out.df$ActionFinal[out.df$fishing == "Fishing"] <- "Fishing" 
out.df$ActionFinal[out.df$fishing == "Not fishing" & out.df$Action == "steaming"] <- "Steaming" 
out.df$ActionFinal[out.df$fishing == "Not fishing" & out.df$Action == "not fishing"] <- "Turning" 
out.df$ActionFinal[out.df$fishing == "Not fishing" & out.df$Action == "fishing"] <- "Turning" 

###  CONFIRM AGAIN IN GIS ### 


# ---- Resample 1-min olex data to assess effect of polling interval ---- 

#Fully vetted clean dataset is out.df 
out.df  
summarize_sampling_rate(tr1)
dim(tr1)
#add identifier for polling.interval 
out.df$polling.interval.min <- 1 

# NOTE for all resampling intervals, use tolerance of 30 seconds set here: 
XX <- 30 



###.... 5 Minute Resample ....### 
#resample track with amt::track_resample 
tr1.5min <- amt::track_resample(tr1, rate = minutes(5), tolerance = seconds(XX))
dim(tr1.5min)
summarize_sampling_rate(tr1.5min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.5min)
tr1.5min <- tr1.5min %>% nest(data = -"id")
str(tr1.5min)

tr1.5min.steps <- tr1.5min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.5min.steps)

#Pull out steps by trip.id.vessel 
tr1.5min.steps.out <- tr1.5min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.5min.steps.out$id)
tr1.5min.steps.out
#dt_ is in minutes 
tr1.5min.steps.out$minutes <- as.numeric(tr1.5min.steps.out$dt_) 
tr1.5min.steps.out$speed_kmh <- (tr1.5min.steps.out$sl_/1000)/(tr1.5min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.5min.steps.out$speed_knots <- tr1.5min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.5min.steps.out <- tr1.5min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.5min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.5min.steps.out$polling.interval.min <- 5
print(tr1.5min.steps.out, width = Inf) 



###.... 10 Minute Resample ....### 
tr1.10min <- amt::track_resample(tr1, rate = minutes(10), tolerance = seconds(XX))
dim(tr1.10min)
summarize_sampling_rate(tr1.10min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.10min)
tr1.10min <- tr1.10min %>% nest(data = -"id")
str(tr1.10min)

tr1.10min.steps <- tr1.10min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.10min.steps)

#Pull out steps by trip.id.vessel 
tr1.10min.steps.out <- tr1.10min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.10min.steps.out$id)
tr1.10min.steps.out
#dt_ is in minutes 
tr1.10min.steps.out$minutes <- as.numeric(tr1.10min.steps.out$dt_) 
tr1.10min.steps.out$speed_kmh <- (tr1.10min.steps.out$sl_/1000)/(tr1.10min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.10min.steps.out$speed_knots <- tr1.10min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.10min.steps.out <- tr1.10min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.10min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.10min.steps.out$polling.interval.min <- 10 

print(tr1.10min.steps.out, width = Inf) 



###.... 15 Minute Resample ....### 
tr1.15min <- amt::track_resample(tr1, rate = minutes(15), tolerance = seconds(XX))
dim(tr1.15min)
summarize_sampling_rate(tr1.15min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.15min)
tr1.15min <- tr1.15min %>% nest(data = -"id")
str(tr1.15min)

tr1.15min.steps <- tr1.15min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.15min.steps)

#Pull out steps by trip.id.vessel 
tr1.15min.steps.out <- tr1.15min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.15min.steps.out$id)
tr1.15min.steps.out
#dt_ is in minutes 
tr1.15min.steps.out$minutes <- as.numeric(tr1.15min.steps.out$dt_) 
tr1.15min.steps.out$speed_kmh <- (tr1.15min.steps.out$sl_/1000)/(tr1.15min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.15min.steps.out$speed_knots <- tr1.15min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.15min.steps.out <- tr1.15min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.15min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.15min.steps.out$polling.interval.min <- 15

print(tr1.15min.steps.out, width = Inf) 


###.... 20 Minute Resample ....### 
tr1.20min <- amt::track_resample(tr1, rate = minutes(20), tolerance = seconds(XX))
dim(tr1.20min)
summarize_sampling_rate(tr1.20min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.20min)
tr1.20min <- tr1.20min %>% nest(data = -"id")
str(tr1.20min)

tr1.20min.steps <- tr1.20min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.20min.steps)

#Pull out steps by trip.id.vessel 
tr1.20min.steps.out <- tr1.20min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.20min.steps.out$id)
tr1.20min.steps.out
#dt_ is in minutes 
tr1.20min.steps.out$minutes <- as.numeric(tr1.20min.steps.out$dt_) 
tr1.20min.steps.out$speed_kmh <- (tr1.20min.steps.out$sl_/1000)/(tr1.20min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.20min.steps.out$speed_knots <- tr1.20min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.20min.steps.out <- tr1.20min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.20min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.20min.steps.out$polling.interval.min <- 20

print(tr1.20min.steps.out, width = Inf) 




###.... 25 Minute Resample ....### 
tr1.25min <- amt::track_resample(tr1, rate = minutes(25), tolerance = seconds(XX))
dim(tr1.25min)
summarize_sampling_rate(tr1.25min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.25min)
tr1.25min <- tr1.25min %>% nest(data = -"id")
str(tr1.25min)

tr1.25min.steps <- tr1.25min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.25min.steps)

#Pull out steps by trip.id.vessel 
tr1.25min.steps.out <- tr1.25min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.25min.steps.out$id)
tr1.25min.steps.out
#dt_ is in minutes 
tr1.25min.steps.out$minutes <- as.numeric(tr1.25min.steps.out$dt_) 
tr1.25min.steps.out$speed_kmh <- (tr1.25min.steps.out$sl_/1000)/(tr1.25min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.25min.steps.out$speed_knots <- tr1.25min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.25min.steps.out <- tr1.25min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.25min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.25min.steps.out$polling.interval.min <- 25

print(tr1.25min.steps.out, width = Inf) 




###.... 30 Minute Resample ....### 
tr1.30min <- amt::track_resample(tr1, rate = minutes(30), tolerance = seconds(XX))
dim(tr1.30min)
summarize_sampling_rate(tr1.30min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.30min)
tr1.30min <- tr1.30min %>% nest(data = -"id")
str(tr1.30min)

tr1.30min.steps <- tr1.30min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.30min.steps)

#Pull out steps by trip.id.vessel 
tr1.30min.steps.out <- tr1.30min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.30min.steps.out$id)
tr1.30min.steps.out
#dt_ is in minutes 
tr1.30min.steps.out$minutes <- as.numeric(tr1.30min.steps.out$dt_) 
tr1.30min.steps.out$speed_kmh <- (tr1.30min.steps.out$sl_/1000)/(tr1.30min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.30min.steps.out$speed_knots <- tr1.30min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.30min.steps.out <- tr1.30min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.30min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.30min.steps.out$polling.interval.min <- 30

print(tr1.30min.steps.out, width = Inf) 



###.... 35 Minute Resample ....### 
tr1.35min <- amt::track_resample(tr1, rate = minutes(35), tolerance = seconds(XX))
dim(tr1.35min)
summarize_sampling_rate(tr1.35min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.35min)
tr1.35min <- tr1.35min %>% nest(data = -"id")
str(tr1.35min)

tr1.35min.steps <- tr1.35min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.35min.steps)

#Pull out steps by trip.id.vessel 
tr1.35min.steps.out <- tr1.35min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.35min.steps.out$id)
tr1.35min.steps.out
#dt_ is in minutes 
tr1.35min.steps.out$minutes <- as.numeric(tr1.35min.steps.out$dt_) 
tr1.35min.steps.out$speed_kmh <- (tr1.35min.steps.out$sl_/1000)/(tr1.35min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.35min.steps.out$speed_knots <- tr1.35min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.35min.steps.out <- tr1.35min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.35min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.35min.steps.out$polling.interval.min <- 35

print(tr1.35min.steps.out, width = Inf) 



###.... 40 Minute Resample ....### 
tr1.40min <- amt::track_resample(tr1, rate = minutes(40), tolerance = seconds(XX))
dim(tr1.40min)
summarize_sampling_rate(tr1.40min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.40min)
tr1.40min <- tr1.40min %>% nest(data = -"id")
str(tr1.40min)

tr1.40min.steps <- tr1.40min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.40min.steps)

#Pull out steps by trip.id.vessel 
tr1.40min.steps.out <- tr1.40min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.40min.steps.out$id)
tr1.40min.steps.out
#dt_ is in minutes 
tr1.40min.steps.out$minutes <- as.numeric(tr1.40min.steps.out$dt_) 
tr1.40min.steps.out$speed_kmh <- (tr1.40min.steps.out$sl_/1000)/(tr1.40min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.40min.steps.out$speed_knots <- tr1.40min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.40min.steps.out <- tr1.40min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.40min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.40min.steps.out$polling.interval.min <- 40

print(tr1.40min.steps.out, width = Inf) 



###.... 45 Minute Resample ....### 
tr1.45min <- amt::track_resample(tr1, rate = minutes(45), tolerance = seconds(XX))
dim(tr1.45min)
summarize_sampling_rate(tr1.45min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.45min)
tr1.45min <- tr1.45min %>% nest(data = -"id")
str(tr1.45min)

tr1.45min.steps <- tr1.45min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.45min.steps)

#Pull out steps by trip.id.vessel 
tr1.45min.steps.out <- tr1.45min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.45min.steps.out$id)
tr1.45min.steps.out
#dt_ is in minutes 
tr1.45min.steps.out$minutes <- as.numeric(tr1.45min.steps.out$dt_) 
tr1.45min.steps.out$speed_kmh <- (tr1.45min.steps.out$sl_/1000)/(tr1.45min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.45min.steps.out$speed_knots <- tr1.45min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.45min.steps.out <- tr1.45min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.45min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.45min.steps.out$polling.interval.min <- 45

print(tr1.45min.steps.out, width = Inf) 



###.... 50 Minute Resample ....### 
tr1.50min <- amt::track_resample(tr1, rate = minutes(50), tolerance = seconds(XX))
dim(tr1.50min)
summarize_sampling_rate(tr1.50min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.50min)
tr1.50min <- tr1.50min %>% nest(data = -"id")
str(tr1.50min)

tr1.50min.steps <- tr1.50min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.50min.steps)

#Pull out steps by trip.id.vessel 
tr1.50min.steps.out <- tr1.50min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.50min.steps.out$id)
tr1.50min.steps.out
#dt_ is in minutes 
tr1.50min.steps.out$minutes <- as.numeric(tr1.50min.steps.out$dt_) 
tr1.50min.steps.out$speed_kmh <- (tr1.50min.steps.out$sl_/1000)/(tr1.50min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.50min.steps.out$speed_knots <- tr1.50min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.50min.steps.out <- tr1.50min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.50min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.50min.steps.out$polling.interval.min <- 50

print(tr1.50min.steps.out, width = Inf) 



###.... 55 Minute Resample ....### 
tr1.55min <- amt::track_resample(tr1, rate = minutes(55), tolerance = seconds(XX))
dim(tr1.55min)
summarize_sampling_rate(tr1.55min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.55min)
tr1.55min <- tr1.55min %>% nest(data = -"id")
str(tr1.55min)

tr1.55min.steps <- tr1.55min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.55min.steps)

#Pull out steps by trip.id.vessel 
tr1.55min.steps.out <- tr1.55min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.55min.steps.out$id)
tr1.55min.steps.out
#dt_ is in minutes 
tr1.55min.steps.out$minutes <- as.numeric(tr1.55min.steps.out$dt_) 
tr1.55min.steps.out$speed_kmh <- (tr1.55min.steps.out$sl_/1000)/(tr1.55min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.55min.steps.out$speed_knots <- tr1.55min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.55min.steps.out <- tr1.55min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.55min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.55min.steps.out$polling.interval.min <- 55

print(tr1.55min.steps.out, width = Inf) 



###.... 60 Minute Resample ....### 
tr1.60min <- amt::track_resample(tr1, rate = minutes(60), tolerance = seconds(XX))
dim(tr1.60min)
summarize_sampling_rate(tr1.60min)

#Next, we group the track by id and nest the track. #NOTE that in steps function are using the defauls, turning angle in radians and between -pi and pi, east = zero 
dim(tr1.60min)
tr1.60min <- tr1.60min %>% nest(data = -"id")
str(tr1.60min)

tr1.60min.steps <- tr1.60min %>% 
  mutate(steps = map(data, function(x) 
    x %>% steps()))
str(tr1.60min.steps)

#Pull out steps by trip.id.vessel 
tr1.60min.steps.out <- tr1.60min.steps %>% select(id, steps) %>% unnest(cols = steps)
table(tr1.60min.steps.out$id)
tr1.60min.steps.out
#dt_ is in minutes 
tr1.60min.steps.out$minutes <- as.numeric(tr1.60min.steps.out$dt_) 
tr1.60min.steps.out$speed_kmh <- (tr1.60min.steps.out$sl_/1000)/(tr1.60min.steps.out$minutes/60)
#1 km/h = 0.539957 knots 
tr1.60min.steps.out$speed_knots <- tr1.60min.steps.out$speed_kmh*0.539957

#convert turning angle to degrees 
tr1.60min.steps.out <- tr1.60min.steps.out %>% mutate(ta_degrees = as_degree(ta_))
summary(tr1.60min.steps.out$ta_degrees)
#add identifier for polling.interval 
tr1.60min.steps.out$polling.interval.min <- 60

print(tr1.60min.steps.out, width = Inf) 



#---- Merge all resampled data with calculated speed and angles together ---- 
out.1 <- out.df %>% select(-Action, -abs_tadegrees, -TowId, -id.TowId, -TowIdverified, -uid , -fishing, -ActionFinal) %>% rename(minutes = sec )
out.1$minutes <- out.1$minutes/60
out.1

all.trks <- rbind(out.1, tr1.5min.steps.out, tr1.10min.steps.out, tr1.15min.steps.out, 
                  tr1.20min.steps.out, tr1.25min.steps.out, tr1.30min.steps.out,
                  tr1.35min.steps.out, tr1.40min.steps.out, tr1.45min.steps.out,
                  tr1.50min.steps.out, tr1.55min.steps.out,  tr1.60min.steps.out) 

#summaries for checks 
table(all.trks$polling.interval.min)

##--- Export 
save(out.df, all.trks, file = "alltrks.RData")


### END 
