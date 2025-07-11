###
# Summary and Analysis of Fishermen questionnaire data to determine Speed Range 
# J.Sameoto 2025 - NAJFM MS: Accuracy of using calculated speeds with Vessel Monitoring System data: implications for fishing activity metrics  
#

library(tidyverse)
library(ggplot2)


#---- Read in data summary for defining TRUE vessel speed breaks ----### 
questionnaire <- read.csv("./Fisherquestionnaire.csv")

#mean values 
mean.speeds <- questionnaire %>% group_by(Action.description) %>% summarize(mean = mean(Speed))
mean.speeds

# FIGURE OF INSTANTANEOUS FISHER REPORTED SPEED BY ACTIVITY STATE #  
ggplot(questionnaire, aes(x=Action.description, y=Speed)) + 
  geom_boxplot() + theme_bw() + ylab("Speed (knots)") + xlab("") + 
  scale_y_continuous(breaks=seq(0,15,2.5), limits = c(0, NA)) + 
  geom_point(data = mean.speeds, aes(x = Action.description, y = mean, colour = "red", size = 3)) + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=14), legend.position="none") 


# FIGURE OF INSTANTANEOUS FISHERMAN REPORTED SPEED BY ACTIVITY STATE BY TIDAL STATE 
ggplot(questionnaire, aes(x=Action.description, y=Speed)) + 
  geom_boxplot() + 
  facet_wrap(~Tide.description)+ 
  theme_bw() + ylab("Speed (knots)") + xlab("") + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=14), strip.text = element_text(size = 14))


# mean, median, var, sd, for fishing and steaming 
questionnaire.summary <- questionnaire %>% 
  group_by(Action) %>%
  summarize(mean.knots = mean(Speed), med.knots = median(Speed), var.knots = var(Speed), stdev=sd(Speed), n=length(Speed) )

# Set Speed Limits dnorm
mean.fish <- questionnaire.summary$mean.knots[questionnaire.summary$Action=='fishing']
stdev.fish <- questionnaire.summary$stdev[questionnaire.summary$Action=='fishing']

mean.steam <- questionnaire.summary$mean.knots[questionnaire.summary$Action=='steaming']
stdev.steam <- questionnaire.summary$stdev[questionnaire.summary$Action=='steaming']


#select Fishing limits based on speeds from interview data: 
fishing.UL <- mean.fish + 3*stdev.fish
fishing.LL <- mean.fish - 3*stdev.fish


### means and SD of fishing and steaming if wanted to manually set to run below plot:   
#mean.fish <- 2.5
#stdev.fish <- 0.5

#mean.steam <- 7.4 
#stdev.steam <- 1.9

# Resample from fishers speed observations 
x <- 100000
dist.rnorm <- data.frame(Action = c(rep("Fishing",x),rep("Steaming",x)), Value = c(rnorm(x,mean.fish,stdev.fish), rnorm(x,mean.steam,stdev.steam)))


ggplot() + 
  geom_density(data = dist.rnorm, aes(x=Value, group=Action, fill = Action), alpha = 0.2) +                               
#  geom_density(data = questionnaire, aes(x=Speed, colour = Action), lwd=1, lty=2, show.legend = F) + 
  geom_density(data = dist.rnorm, aes(x=Value, group=Action, fill = Action), alpha = 0.2, show.legend = F) +   
  geom_vline(xintercept = 1, color = "grey", linetype="dashed", size = 1.5) +  
  geom_vline(xintercept = 4, color = "grey", linetype="dashed", size = 1.5) +  
  theme_bw() + xlab("Speed (knots)") + ylab("Frequency") + 
  theme( panel.grid.minor = element_blank(),
         axis.title=element_text(size=15), axis.text=element_text(size=15),  legend.position = c(0.8, 0.8), legend.title=element_text(size=15), 
         legend.text=element_text(size=15))


