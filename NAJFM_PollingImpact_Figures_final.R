###
##  Polling Impact -  NAJFM Manuscript "Accuracy of using calculated speeds with Vessel Monitoring System data: implications for fishing activity metrics"
## J.Sameoto 2025
## 

library(tidyverse)
library(ggplot2)
library(sp)
library(sf)
library(lubridate)
library(amt)
library(ggspatial)
library(cowplot)


options(tibble.width = Inf)

#### Import function
funcs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Maps/convert_coords.R") 
       
dir <- getwd()
for(fun in funcs) 
{
  temp <- dir
  download.file(fun,destfile = basename(fun))
  source(paste0(dir,"/",basename(fun)))
  file.remove(paste0(dir,"/",basename(fun)))
}


load("alltrks.RData")
#loads objects out.df, all.trks


unique(all.trks$polling.interval.min) 

tracks.1min <- all.trks %>% filter(all.trks$polling.interval.min == 1)

##  merge to original correct Action 
true.action <- out.df %>% select(t1_,  ActionFinal) 

all.trks.action <- left_join(all.trks, true.action, by="t1_") 




# ---- SPEED ---- 

# Speed profiles of 1 minute data 
# Speed Freq Plot by trip 
speed.freq.1min.trips<- ggplot(all.trks %>% filter(polling.interval.min == 1) , aes(x=speed_knots)) + 
  geom_histogram(binwidth = 0.1, color='black', fill="grey") + 
  facet_wrap(~id) + 
  geom_vline(xintercept = 1, color = "grey", linetype="dashed", linewidth = 1) +  
  geom_vline(xintercept = 4, color = "grey", linetype="dashed", linewidth = 1) +  
  theme_bw() + xlab("Speed (knots)") + ylab("Frequency")  +
  xlim(0, 15) + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=12),  strip.text = element_text(size = 14))
speed.freq.1min.trips


#Change in SPEED with polling interval as boxplot 
speed.by.action.boxplot <- ggplot(all.trks.action, aes(x=as.factor(polling.interval.min), y=speed_knots) ) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  facet_wrap(~ActionFinal) +
  xlab("") + ylab("Speed (knots)") +  theme_bw() +   
  scale_x_discrete(labels = NULL) +
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15), strip.text = element_text(size = 15)) 
speed.by.action.boxplot


# ---- TURNING ANGLE ---- 

#Change in ABSOLUTE TURNING ANGLE with polling interval as boxplot 
ta.abs.by.action.boxplot <- ggplot(all.trks.action, aes(x=as.factor(polling.interval.min), y=abs(ta_degrees )) ) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  facet_wrap(~ActionFinal) +
  xlab("Polling interval (minutes)") + 
  ylab("Turning angle (degrees)") +  
  theme_bw()  + 
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15), strip.text = element_text(size = 15)) 
ta.abs.by.action.boxplot


#merge panels of speed and turning angle 
plot_grid(speed.by.action.boxplot, ta.abs.by.action.boxplot, labels = c('A', 'B'), ncol = 1)




# ---- TRIP LENGTH (distance traveled i.e. Trip Distance) by polling interval ---- 
#total length of all activity - total length traveled 
all.trks.action.trip.length <- all.trks.action %>% group_by(id, polling.interval.min) %>% summarise(trip.length.km = sum(sl_)/1000)

trip.length.1.min <- all.trks.action.trip.length %>% filter(polling.interval.min == 1) %>% select(id, trip.length.km)

all.trks.action.trip.length.prop <- merge(all.trks.action.trip.length, trip.length.1.min, by =c("id")) 

all.trks.action.trip.length.prop$prop.of.true.length <- round(all.trks.action.trip.length.prop$trip.length.km.x/ all.trks.action.trip.length.prop$trip.length.km.y, 2)


trip.length.plot <- ggplot(data = all.trks.action.trip.length, aes(y= trip.length.km, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  ylab("Total trip distance (km)") +
  xlab("") +  
  theme_bw() +   
  scale_x_discrete(labels = NULL) +
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15))
trip.length.plot


#proportion of true trip length 
trip.length.prop.plot <- ggplot(data = all.trks.action.trip.length.prop, aes(y= prop.of.true.length, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  ylab("Ratio of trip distance \n to true trip distance") +
  xlab("") +  
  theme_bw() +   
  scale_x_discrete(labels = NULL) +
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=15))
trip.length.prop.plot


all.trks.action.trip.length.prop %>% group_by(polling.interval.min) %>% summarize(mean(trip.length.km.x), median(trip.length.km.x))



#---- SWEPT AREA ----- 
# by fishing activity by polling interval ---- 
#From Licence conditions, max gear is 5.5m 
all.trks.action.trip.length.fishing <- all.trks.action %>% filter(ActionFinal == "Fishing") %>% group_by(id, polling.interval.min) %>% summarise(fished.length.km = sum(sl_)/1000)

all.trks.action.trip.length.fishing$Swept.Area.sqkm <- all.trks.action.trip.length.fishing$fished.length.km * (5.5/1000)

all.trks.action.trip.length.fishing


trip.length.fishing.1.min <- all.trks.action.trip.length.fishing %>% filter(polling.interval.min == 1) %>% select(id,  Swept.Area.sqkm)

all.trks.action.trip.length.fishing.prop <- merge(all.trks.action.trip.length.fishing, trip.length.fishing.1.min, by =c("id")) 

all.trks.action.trip.length.fishing.prop$prop.of.true.swept.area <- round(all.trks.action.trip.length.fishing.prop$Swept.Area.sqkm.x/ all.trks.action.trip.length.fishing.prop$Swept.Area.sqkm.y, 2)

all.trks.action.trip.length.fishing.prop



# swept area (km2)
swept.area.plot <- ggplot(data = all.trks.action.trip.length.fishing, aes(y= Swept.Area.sqkm, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  labs(y= bquote("Swept area"~(km^2))) + 
  xlab("") +  
  theme_bw() +   
  scale_x_discrete(labels = NULL) +
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14))
swept.area.plot


all.trks.action.trip.length.fishing %>% group_by(polling.interval.min) %>% summarise( mean(Swept.Area.sqkm), median(Swept.Area.sqkm))



#proportional swept area 
swept.area.plot.prop <- ggplot(data = all.trks.action.trip.length.fishing.prop, aes(y= prop.of.true.swept.area, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  #labs(y= bquote("Swept area"~(km^2))) + 
  ylab("Ratio of swept area \n to true swept area") + 
  xlab("") +  
  theme_bw() +   
  scale_x_discrete(labels = NULL) +
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14))
swept.area.plot.prop


# ---- Spatial plot example track ----

spatial.example.dat <- all.trks.action %>% filter(id == 3 & t1_ >= as.POSIXct("2014-01-31 10:37:00", tz = "GMT") & t1_ <= as.POSIXct("2014-02-02 06:03:00", tz = "GMT"))

spatial.example.dat <- st_as_sf(spatial.example.dat, coords= c("x1_","y1_"),  crs = 32620) 

spatial.example.dat.line <- st_as_sf(spatial.example.dat, coords= c("x1_","y1_"),  crs = 32620) %>%    group_by(polling.interval.min) %>%  summarize(do_union=FALSE) %>% st_cast("LINESTRING") 



#polylines 
ggplot() + 
  geom_sf(data = spatial.example.dat.line) +   
  facet_wrap(~polling.interval.min) +
  theme_bw() +   
  theme( panel.grid.minor = element_blank(),  strip.text = element_text(size = 15)) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotation_scale(location = "tl")


#### colored by action 
#points 
ggplot() + 
  geom_sf(data = spatial.example.dat, aes(color = ActionFinal)) + 
  facet_wrap(~polling.interval.min) +
  theme_bw() +   
  theme( panel.grid.minor = element_blank(),  strip.text = element_text(size = 15)) + 
  theme(legend.position = c(.5, .12), legend.text = element_text(size = 15), legend.title = element_blank(), legend.direction="horizontal")  +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  annotation_scale(location = "tl")






#----- CONVEX HULL by trip
# characterize fishing footprint of trip 

#filter to just fishing and then convex hull 
all.trks.action.sf <- st_as_sf(all.trks.action, coords= c("x1_","y1_"),  crs = 32620) 


all.trks.action.sf.fishing <- all.trks.action.sf %>% filter(ActionFinal == "Fishing")


#group and summarise by unique trip (id) and calculate convex hulls
hulls.fished.trip <- all.trks.action.sf %>% filter(ActionFinal == "Fishing") %>% 
  group_by( id, polling.interval.min ) %>%  #group by trip
  summarise( geometry = st_combine( geometry ) ) %>%
  st_convex_hull()


#convex hull area 
hulls.fished.trip$area <- st_area(hulls.fished.trip)
hulls.fished.trip$area.sqkm <- as.numeric(hulls.fished.trip$area)*(1e-6) #convert from m^2 to sq km 


#Convex hull by trip 
#area fished per TRIP as a proportion of true area fished 

hulls.fished.trip.1.min <- hulls.fished.trip %>% filter(polling.interval.min == 1) %>% as.data.frame()   %>%  select(id  , area.sqkm)

hulls.fished.trip.1.min.prop <- merge(hulls.fished.trip, hulls.fished.trip.1.min, by =c("id")) 

hulls.fished.trip.1.min.prop$prop.of.true.convex.area <- round(hulls.fished.trip.1.min.prop$area.sqkm.x/ hulls.fished.trip.1.min.prop$area.sqkm.y, 2)


convex.by.trip.plot <- ggplot(data = hulls.fished.trip.1.min.prop, aes(y= area.sqkm.x, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  labs(y= bquote("Fishing spatial footprint"~(km^2))) + 
  xlab("Polling interval (minutes)") +  
  theme_bw() +   
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  ylim(c(0,80))
convex.by.trip.plot 


convex.by.trip.plot.prop <- ggplot(data = hulls.fished.trip.1.min.prop, aes(y= prop.of.true.convex.area, x = as.factor(polling.interval.min))) + 
  geom_boxplot(outlier.alpha = 0.1) + 
  ylab("Ratio of spatial footprint \n to true spatial footprint") + 
  xlab("Polling interval (minutes)") +  
  theme_bw() +   
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14))
convex.by.trip.plot.prop


hulls.fished.trip.1.min.prop %>% filter(area.sqkm.x > 0) %>% group_by(polling.interval.min) %>% summarise(median(area.sqkm.x))



### --- Multipanel plot of behaviour metrics on ratio scale ---- 
#original scale and ratio scale together 
plot_grid(trip.length.plot, trip.length.prop.plot,
          swept.area.plot, swept.area.plot.prop, 
          convex.by.trip.plot, convex.by.trip.plot.prop, 
          labels = c("A","B", "C", "D", "E", "F"), ncol = 2) 



### ----- Plot speeds by proportion of bins ----- 
Bin.Break <- 0.25

speed.binned <- all.trks.action %>% mutate(SpeedBins = cut(speed_knots , breaks = c(seq(0,max(all.trks.action$speed_knots+5),by=Bin.Break))))


speed.binned$BinID <- as.numeric(speed.binned$SpeedBins) 

speed.binned$BinID.label <- speed.binned$BinID * Bin.Break

speed.binned$Activity <- speed.binned$ActionFinal


speed.binned.by.polling <- ggplot(speed.binned, aes(x = BinID.label,  fill=Activity ))   + 
  geom_histogram(binwidth = 0.25) + 
  facet_wrap(~polling.interval.min, scales = "free_y") + 
  theme_bw() + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=14), axis.text=element_text(size=14),  strip.text = element_text(size = 15))  + 
  xlab("Speed (knots)") +   ylab("Frequency") +
  scale_x_continuous(limits = c(0,12.5)) +
  geom_vline(xintercept = 1, color = "grey", linetype="dashed", size = 1) + 
  geom_vline(xintercept = 4, color = "grey", linetype="dashed", size = 1) + 
  theme(legend.position = c(.5, .12), legend.text = element_text(size = 15), legend.title = element_blank(), legend.direction="horizontal")  
speed.binned.by.polling




unique(speed.binned$ActionFinal)
speed.binned$Fishing.Nonfishing <- NA 
speed.binned$Fishing.Nonfishing[speed.binned$ActionFinal == 'Fishing'] <- 'Fishing'
speed.binned$Fishing.Nonfishing[speed.binned$ActionFinal == 'Steaming'] <- 'Non-Fishing'
speed.binned$Fishing.Nonfishing[speed.binned$ActionFinal == 'Turning'] <- 'Non-Fishing'



#--- Proportion of activity correctly classified as fishing for the different polling interval datasets using 1 and 4 knots as criteria 

Speed.Rule.1to4.results <- speed.binned %>% filter(speed_knots >= 1 & speed_knots <= 4) %>% group_by(polling.interval.min, ActionFinal) %>% summarise(count = n())

#total records by bin summarized 
totals <- speed.binned  %>% group_by(polling.interval.min, ActionFinal) %>% summarise(count = n()) #%>% print(n = 'Inf')

Speed.Rule.1to4.performance <- merge(as.data.frame(Speed.Rule.1to4.results), as.data.frame(totals), by = c("polling.interval.min", "ActionFinal"), all = TRUE) 
names(Speed.Rule.1to4.performance)[3] <- "classified.as.fishing"
names(Speed.Rule.1to4.performance)[4] <- "total"
Speed.Rule.1to4.performance

#change NA to 0
Speed.Rule.1to4.performance$classified.as.fishing[Speed.Rule.1to4.performance$ActionFinal == "Steaming" & Speed.Rule.1to4.performance$polling.interval.min == 1] <- 0 
Speed.Rule.1to4.performance

#True positives (Fishing correctly classified) and false positives (nonfishing (ie. steaming or turning/hauling) classified as fishing based on 1 to 4 speed rule
Speed.Rule.1to4.performance$TP.FP <- round(Speed.Rule.1to4.performance$classified.as.fishing/Speed.Rule.1to4.performance$total,2)

True.Positives <- Speed.Rule.1to4.performance %>% filter(ActionFinal == 'Fishing')
False.Positives <- Speed.Rule.1to4.performance %>% filter(ActionFinal != 'Fishing')
False.Positives

# True.Positives -- in field TP.FP is the proportion of correctly classified fishing records as fishing 

# False.Positives -- in field TP.FP is the proportion of  nonfishing records mis-classified as fishing 

#write out file and calculate proportion misclassified and correctly classified for each activity category;  Speed.Rule.1to4.performance gives you e.g. number Turning records at 60-min misclassified as fishing (e.g. 31 out of 148), therefore know that 148-31 = 117 turning records were correctly categorized as non-fishing at 60-min (117/148 = 79%)
#write.csv(Speed.Rule.1to4.performance, "./Speed.Rule.1to4.performance.csv")
Speed.Rule.1to4.performance.all <- read.csv("./Speed.Rule.1to4.performance.csv")


#--- proportion of records correctly classified based on speed range - fishing vs non-fishing ----- 

all.trks.action.F.NF <- all.trks.action
all.trks.action.F.NF$action[all.trks.action.F.NF$ActionFinal == "Fishing"] <- "Fishing"
all.trks.action.F.NF$action[all.trks.action.F.NF$ActionFinal == "Steaming"] <- "Non-fishing"
all.trks.action.F.NF$action[all.trks.action.F.NF$ActionFinal == "Turning"] <- "Non-fishing"

unique(all.trks.action.F.NF$action)

#Proportion of records fishing and non-fishing, or fishing, steaming, haul/turn for each polling interval dataset 
total.record.by.polling <- all.trks.action.F.NF  %>% group_by(polling.interval.min) %>% summarise(total.n = n())
total.record.by.polling

record.by.polling.by.action <- all.trks.action.F.NF  %>% group_by(polling.interval.min, action) %>% summarise(count = n())



prop.record.by.polling.by.action <- merge(record.by.polling.by.action, total.record.by.polling, by = c("polling.interval.min"))

prop.record.by.polling.by.action$prop <- prop.record.by.polling.by.action$count/prop.record.by.polling.by.action$total.n


prop.record.by.polling.by.action




#--- speed break only  between 1 and 4 at 1 min - characterizes what % of records correctly 
all.trks.action.F.NF %>% filter(polling.interval.min == 1) %>% filter(speed_knots >= 1 & speed_knots <= 4) %>% group_by(action) %>% summarise(count = n())


#total records by bin summarized 
totals.F.NF <- all.trks.action.F.NF  %>% group_by(polling.interval.min, action ) %>% summarise(count = n()) 
totals.F.NF

Speed.Rule.1to4.results.F.NF <- all.trks.action.F.NF %>% filter(speed_knots >= 1 & speed_knots <= 4) %>% group_by(polling.interval.min, action) %>% summarise(count = n())



Speed.Rule.1to4.performance.F.NF <- merge(as.data.frame(Speed.Rule.1to4.results.F.NF), as.data.frame(totals.F.NF), by = c("polling.interval.min", "action"), all = TRUE) 
names(Speed.Rule.1to4.performance.F.NF)[3] <- "classified.as.fishing"
names(Speed.Rule.1to4.performance.F.NF)[4] <- "total"
Speed.Rule.1to4.performance.F.NF


#True positives  and false positives based on 1 to 4 speed rule
Speed.Rule.1to4.performance.F.NF$TP.FP <- round(Speed.Rule.1to4.performance.F.NF$classified.as.fishing/Speed.Rule.1to4.performance.F.NF$total,2)
Speed.Rule.1to4.performance.F.NF


True.Positives <- Speed.Rule.1to4.performance.F.NF %>% filter(action == 'Fishing')
False.Positives <- Speed.Rule.1to4.performance.F.NF %>% filter(action != 'Fishing')
False.Positives


#relabel for legend 
Speed.Rule.1to4.performance.F.NF$Legend <- NA 
Speed.Rule.1to4.performance.F.NF$Legend[Speed.Rule.1to4.performance.F.NF$action == "Fishing"] <- "Fishing: True Positive"
Speed.Rule.1to4.performance.F.NF$Legend[Speed.Rule.1to4.performance.F.NF$action == "Non-fishing"] <- "Non-Fishing: False Positive"


### Figure TP and FP 
ggplot(Speed.Rule.1to4.performance.F.NF , aes(x=polling.interval.min, y=TP.FP, color = Legend, shape = Legend, linetype = Legend )) + 
  geom_line() + geom_point() + 
  xlab("Polling interval (minutes)") + 
  ylab("Rate") +  
  theme_bw() +   
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=14), legend.position = c(.68, .9), legend.title=element_blank(), legend.text=element_text(size=15))  + 
  xlim(0,60) 


#see Values 
Speed.Rule.1to4.performance %>% filter(ActionFinal == 'Fishing')
#using rule of 1 to 4 knots for 60 min data results in only 12% of fishing being categorized as fishing 


#---- Records  in 1to4 knot range --- 
record.in.speed.window <- Speed.Rule.1to4.performance %>% group_by(polling.interval.min) %>% summarise(classified.as.fishing = sum(classified.as.fishing), total.nonfishing = sum(total ))
record.in.speed.window$prop.records.in.speed.window <- record.in.speed.window$classified.as.fishing/record.in.speed.window$total.nonfishing


record.in.speed.window.plot <- ggplot(record.in.speed.window, aes(x=polling.interval.min, y=prop.records.in.speed.window) ) + 
  geom_line() + geom_point() + 
  xlab("Polling interval (minutes)") + ylab("Proportion of records between 1 to 4 knots") +  theme_bw() +   
  theme( panel.grid.minor = element_blank(), axis.title=element_text(size=15), axis.text=element_text(size=14)) + 
  xlim(0,60) + ylim(0,1)
record.in.speed.window.plot

record.in.speed.window




# ----- proportion correctly identified by different speed window ----- 

library(caret)

dim(all.trks.action)
#set speed filter 
unique(all.trks.action$ActionFinal)
all.trks.action[is.na(all.trks.action$ActionFinal),]


confusion.data <- all.trks.action
unique(confusion.data$ActionFinal)
#care about correctly identifying fishing versus everything else, so recode so just dealing with fishing and non-fishing - so 2 categories 
confusion.data$Fishing <- NA 
confusion.data$Fishing[confusion.data$ActionFinal == "Fishing"] <- "Fishing"
confusion.data$Fishing[confusion.data$ActionFinal == "Steaming"] <- "Not fishing"
confusion.data$Fishing[confusion.data$ActionFinal == "Turning"] <- "Not fishing"
unique(confusion.data$Fishing)
confusion.data[is.na(confusion.data$Fishing),]
table(confusion.data$Fishing) 

confusion.data$Fishing <- as.factor(confusion.data$Fishing) 
str(confusion.data) #first level is fishing 


## Run for each combination 
filter.UL <- 4  #0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4
filter.LL <- 0 #c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25, 3.5, 3.75) 

#reclassification based on speed rule combination above 
confusion.data$Action.Reclass <- NA 
confusion.data$Action.Reclass[confusion.data$speed_knots >= filter.LL & confusion.data$speed_knots <= filter.UL] <- "Fishing"
confusion.data$Action.Reclass[confusion.data$speed_knots < filter.LL] <- "Not fishing"
confusion.data$Action.Reclass[confusion.data$speed_knots > filter.UL] <- "Not fishing"

table(confusion.data$Action.Reclass)

#make as factor 
confusion.data$Action.Reclass <- as.factor(confusion.data$Action.Reclass)
str(confusion.data) #first level is fishing 
unique(confusion.data$Action.Reclass)
table(confusion.data$Action.Reclass)
table(confusion.data$Fishing)

#temp has all polling intervals, so need to do confusion matrix per each polling interval 
intervals <- unique(confusion.data$polling.interval.min)
confmtx.list <- list()


for(i in 1:length(intervals)) {
  data <- confusion.data %>% filter(polling.interval.min == intervals[i])
  confmtx.list[[i]] <- caret::confusionMatrix(data=data$Action.Reclass, reference=data$Fishing) 
  names(confmtx.list)[i] <- intervals[i]
}
confmtx.list


 
byClass.out <- rbind(confmtx.list$`1`$byClass, confmtx.list$`5`$byClass, confmtx.list$`10`$byClass, 
                     confmtx.list$`15`$byClass, confmtx.list$`20`$byClass, confmtx.list$`25`$byClass,
                     confmtx.list$`30`$byClass, confmtx.list$`35`$byClass, confmtx.list$`40`$byClass,
                     confmtx.list$`45`$byClass, confmtx.list$`50`$byClass, confmtx.list$`55`$byClass, confmtx.list$`60`$byClass) 
byClass.out <- as.data.frame(byClass.out) 
byClass.out$filter.UL <- filter.UL
byClass.out$filter.LL <- filter.LL
byClass.out$polling.interval.min <- intervals
byClass.out

#save out for later merge 
#byClass.out.0.5 <- byClass.out 
#byClass.out.1 <- byClass.out 
#byClass.out.1.5 <- byClass.out 
#byClass.out.2 <- byClass.out 
#byClass.out.2.5 <- byClass.out 
#byClass.out.3 <- byClass.out 
#byClass.out.3.5 <- byClass.out
byClass.out.4 <- byClass.out 




#####
#once run every UL combination with static lower limit, merge:

byClass.final.LL.0 <- rbind( byClass.out.1, byClass.out.1.5, byClass.out.2, byClass.out.2.5, byClass.out.3, byClass.out.3.5, byClass.out.4)

#byClass.final.LL.0.25 <- rbind( byClass.out.1, byClass.out.1.5, byClass.out.2, byClass.out.2.5, byClass.out.3, byClass.out.3.5, byClass.out.4)

#byClass.final.LL.0.5 <- rbind( byClass.out.1, byClass.out.1.5, byClass.out.2, byClass.out.2.5, byClass.out.3, byClass.out.3.5, byClass.out.4)

#byClass.final.LL.0.75 <- rbind( byClass.out.1, byClass.out.1.5, byClass.out.2, byClass.out.2.5, byClass.out.3, byClass.out.3.5, byClass.out.4)

#byClass.final.LL.1.0 <- rbind( byClass.out.1.5, byClass.out.2, byClass.out.2.5, byClass.out.3, byClass.out.3.5, byClass.out.4)



## Once all combinations run, merge to get final balance accuracy matrix
ba.matrix <- rbind(byClass.final.LL.0, byClass.final.LL.0.25, byClass.final.LL.0.5, byClass.final.LL.0.75, byClass.final.LL.1.0)


#rename field for plots 
ba.matrix$`Balanced Accuracy` <- ba.matrix$Balanced.Accuracy

temp <- ba.matrix 
str(temp)

temp$`Lower speed limit (knots):` <- as.factor(temp$filter.LL )


#Balanced Accuracy  = (True positive rate + true negative rate)/2 ... maximimal value is best tradeoff in maximizing true positives and true negatives; the average between the sensitivity and the specificity, which measures the average accuracy obtained from both the minority and majority classes. 
plot.optim.break.balanced.accuracy <- ggplot(temp, aes(x=filter.UL, y=`Balanced Accuracy`, color = `Lower speed limit (knots):`, shape = `Lower speed limit (knots):`)) +
  facet_wrap(~polling.interval.min) + 
  geom_point() + 
  geom_line() +
  xlab("Upper speed limit (knots)") + 
  ylab("Balanced Accuracy") +
  theme_bw(base_size = 15) + 
  ylim(c(0,1)) + 
  theme(legend.position = c(.6, .08), legend.text = element_text(size = 15), legend.title =  element_text( size = 14), legend.direction="horizontal")  

plot.optim.break.balanced.accuracy


#max balanced accuracy at 60 min polling interval: 
xx <- max(temp$`Balanced Accuracy`[temp$polling.interval.min == 60])
    
temp %>% filter(polling.interval.min == 60 & `Balanced Accuracy`== xx)



### END ### 
           