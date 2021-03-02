setwd("~/STRIPS2Helmers")

#rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)
library(STRIPS2Helmers)


#importing rain data -----------------------------------------------------
#importing "fix" data from mesonet weather stations
#these data are fixing problems when rain gages weren't working, causing some samples to be clipped out

rainfixfiles = list.files(path = "rain-fixes",
                          pattern = "*.csv",
                          recursive = TRUE,
                          full.names = TRUE)


rainfiximport <- function(t) {
  name <- t
  file <- readr::read_csv(t, col_names = T, col_types = c("ccn"))
  file$name <- name
  file
}


rainfixes <- map_dfr(rainfixfiles, rainfiximport) %>%
  mutate(date_time = as.POSIXct(valid, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = as.numeric(`p01m`)/1000) %>%
  filter(!is.na(`rain-m`)) %>%
  select(-valid, -p01m) %>%
  rename(watershed = station)

rainfixes$watershed[rainfixes$watershed=="EST"] <- "spiritctl"
rainfixes$watershed[rainfixes$watershed=="AMW"] <- "worlectl"
rainfixes$watershed[rainfixes$watershed=="CNC"] <- "mcnayctl"
rainfixes$watershed[rainfixes$watershed=="ADU"] <- "whitetrt"
rainfixes$watershed[rainfixes$watershed=="MIW"] <- "marshctl"

#The ASOS AWOS data is cumulative by hour so I have to take the max value for each hour to use
rainfixes <- rainfixes %>% 
  mutate(dayhour = floor_date(date_time, "hour")) %>%
  group_by(watershed, dayhour, name) %>% 
  summarize_at(vars('rain-m'), max) %>%
  rename(date_time = dayhour) %>%
  ungroup()

fixlist <- split(rainfixes, list(rainfixes$name))

rainfixes <- rainfixes %>% select(-name)


#replacing rhodesctl rain with worlectl rain for 9/15/16
# s <- filter(STRIPS2Helmers::rain, watershed == "worlectl" & date_time >= as.Date("2016-09-15") &
#               date_time <= as.Date("2016-09-16"))

s <- filter(rain, watershed == "worlectl" & date_time >= as.Date("2016-09-15") &
               date_time <= as.Date("2016-09-16"))


# rain <- STRIPS2Helmers::rain %>%
#   mutate(`rain-m` = replace(`rain-m`, date_time >= as.Date("2016-09-15") & 
#                               date_time <= as.Date("2016-09-16") & 
#                               watershed == "rhodestrt", s$`rain-m`)) %>% 

# THIS SCRIPT IS BEING SOURCED BY RAIN-READ, SO RAIN BELOW IS ALREADY PRESENT
rain <- rain %>%
  mutate(`rain-m` = replace(`rain-m`, date_time >= as.Date("2016-09-15") & 
                              date_time <= as.Date("2016-09-16") & 
                              watershed == "rhodestrt", s$`rain-m`)) %>% 
  
    #FILTERING OUT ADDITIONAL BAD DATA... can write function to automate this repetition?
  filter(!(date_time >= min(pluck(fixlist, 1, 2)) & date_time <= max(pluck(fixlist, 1, 2)) & 
            watershed == pluck(fixlist, 1, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 2, 2)) & date_time <= max(pluck(fixlist, 2, 2)) & 
            watershed == pluck(fixlist, 2, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 3, 2)) & date_time <= max(pluck(fixlist, 3, 2)) & 
            watershed == pluck(fixlist, 3, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 4, 2)) & date_time <= max(pluck(fixlist, 4, 2)) & 
            watershed == pluck(fixlist, 4, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 5, 2)) & date_time <= max(pluck(fixlist, 5, 2)) & 
            watershed == pluck(fixlist, 5, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 6, 2)) & date_time <= max(pluck(fixlist, 6, 2)) & 
            watershed == pluck(fixlist, 6, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 7, 2)) & date_time <= max(pluck(fixlist, 7, 2)) & 
            watershed == pluck(fixlist, 7, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 8, 2)) & date_time <= max(pluck(fixlist, 8, 2)) & 
            watershed == pluck(fixlist, 8, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 9, 2)) & date_time <= max(pluck(fixlist, 9, 2)) & 
            watershed == pluck(fixlist, 9, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 10, 2)) & date_time <= max(pluck(fixlist, 10, 2)) & 
            watershed == pluck(fixlist, 10, 1, 1))) %>%
  filter(!(date_time >= min(pluck(fixlist, 11, 2)) & date_time <= max(pluck(fixlist, 11, 2)) & 
             watershed == pluck(fixlist, 11, 1, 1))) %>%
  
  #old way of filtering out bad data below
  # filter(!(date_time>= min(spl2016fix$date_time) & date_time <= max(spl2016fix$date_time) & 
  #            watershed == "spiritctl"))%>%
  # filter(!(date_time>= min(spl2017fix$date_time) & date_time <= max(spl2017fix$date_time) & 
  #            watershed == "spiritctl"))%>%
  # filter(!(date_time>= min(spl2018fix$date_time) & date_time <= max(spl2018fix$date_time) & 
  #            watershed == "spiritctl"))%>%
  # filter(!(date_time>= min(mcn2018fix$date_time) & date_time <= max(mcn2018fix$date_time) & 
  #            watershed == "mcnayctl"))%>%
  # filter(!(date_time>= min(wor2018fix$date_time) & date_time <= max(wor2018fix$date_time) & 
  #            watershed == "worlectl"))%>%
  # filter(!(date_time>= min(wor2018fix2$date_time) & date_time <= max(wor2018fix2$date_time) & 
  #            watershed == "worlectl"))%>%
  # filter(!(date_time>= min(whi2019fix$date_time) & date_time <= max(whi2019fix$date_time) & 
  #            watershed == "whitetrt"))%>%
  # filter(!(date_time>= min(whi2019fix1$date_time) & date_time <= max(whi2019fix1$date_time) & 
  #            watershed == "whitetrt"))%>%
  # filter(!(date_time>= min(mcn2019fix$date_time) & date_time <= max(mcn2019fix$date_time) & 
  #            watershed == "mcnayctl"))%>%
  # filter(!(date_time>= min(mar2019fix$date_time) & date_time <= max(mar2019fix$date_time) & 
  #            watershed == "marshctl"))%>%
          
  #ADDING IN GOOD DATA
  rbind(rainfixes) %>%
  #ADDITIONAL FORMATTING, ETC.
  mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
         site = gsub("ctl", "", watershed),
         site = gsub("trt", "", site),
         year = lubridate::year(date_time)) %>%
  rename(rain = `rain-m`) %>%
  group_by(site,year) %>%
  arrange(date_time) %>%
  mutate(cumulative_rain = cumsum(rain * 39.3701), # convert to inches
         watershed_year = paste(watershed,year,sep="_"))

myrain <- rain %>%
  select(-watershed, -watershed_year, -treatment, -cumulative_rain)


save(myrain, file = "~/prairiestrips/data/raindataallyears.rds")

rm(list=setdiff(ls(), c("rain")))

setwd("~/STRIPS2Helmers/data-raw")

# 
# #testing mar problems
# test <- rainfixes %>% mutate(year = year(date_time)) %>%
#   group_by(watershed, year) %>%
#   summarize_at(vars('rain-m'), sum)
# 
# test1 <- rainfixes %>% mutate(year = year(date_time)) %>% 
#   filter(watershed == "marshctl" & year == "2019")
