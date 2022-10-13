# Testing StreamMetabolizer functions ##
## Call packages and load data ####
  library(streamMetabolizer)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  tum <- read.csv("./TumTum_DO-temp_2021.csv")
  
  
## Clean TumTum data and fill in missing information
  tum <- tum %>% select(., "Date.Time", "DO", "Temp") # Select relevant columns
  tum$depth <- 0.2 # Assume a constant water depth
  tum$DO.sat <- calc_DO_sat(tum$Temp, press = 1018, sal = 0) ## Calculate DO saturation assuming baro. pressure 1018 (approximated for now)

  
  
### Get date/times in correct format    
  tum <- tum %>% mutate(Date.Time = mdy_hms(Date.Time))
  tum$local.time <- as.POSIXct(tum$Date.Time, tz="")
  tum$solar.time <- calc_solar_time(tum$Date.Time, longitude = -119.287179)
  
### Calculate light - assuming default max PAR
  tum$light <- calc_light(tum$solar.time, 51.83099, -119.287179)
### Get final data set w/ proper names
  tum<- tum %>% rename("DO.obs" = "DO", "temp.water" = "Temp")
  tum <- tum %>% select(., "solar.time","DO.obs", "DO.sat", "depth","temp.water","light")
  
### Quick filter of temperatures where loggers were out of water
  tum <- tum %>% filter(., temp.water < 25)
  
  
## Plotting data per tutorial
  
  tum %>% 
    mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
    select(solar.time, starts_with('DO')) %>%
    gather(type, DO.value, starts_with('DO')) %>%
    mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
    ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_point() + 
    facet_grid(units ~ ., scale='free_y') + theme_bw() +
    scale_color_discrete('variable')
  
  labels <- c(depth='depth\n(m)', temp.water='water temp\n(deg C)', light='PAR\n(umol m^-2 s^-1)')
  tum %>% 
    select(solar.time, depth, temp.water, light) %>%
    gather(type, value, depth, temp.water, light) %>%
    mutate(
      type=ordered(type, levels=c('depth','temp.water','light')),
      units=ordered(labels[type], unname(labels))) %>%
    ggplot(aes(x=solar.time, y=value, color=type)) + geom_point() + 
    facet_grid(units ~ ., scale='free_y') + theme_bw() +
    scale_color_discrete('variable')
  
## Fitting some models  
  
