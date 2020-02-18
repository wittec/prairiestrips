# setwd("~/STRIPS2Helmers/vignettes/")

rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)
library(STRIPS2Helmers)


#importing rain data -----------------------------------------------------
#importing "fix" data from mesonet weather stations
#these data are fixing problems when rain gages weren't working, causing some samples to be clipped out

spl2016fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2016/esthervilleasosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

spl2017fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2017/esthervilleasosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

spl2018fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2018/esthervilleasosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

wor2018fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2018/amesasosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

wor2018fix2 <- read_csv("~/STRIPS2Helmers/rain-fixes/2018/amesasosfix2.csv",
                        col_names = c("watershed", "date_time", "rain-m"),
                        col_types = c("ccn"),
                        skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

mcn2018fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2018/charitonawosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

whi2019fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2019/audubonawosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

whi2019fix1 <- read_csv("~/STRIPS2Helmers/rain-fixes/2019/audubonawosfix1.csv",
                        col_names = c("watershed", "date_time", "rain-m"),
                        col_types = c("ccn"),
                        skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

mcn2019fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2019/charitonawosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

mar2019fix <- read_csv("~/STRIPS2Helmers/rain-fixes/2019/marshalltownasosfix.csv",
                       col_names = c("watershed", "date_time", "rain-m"),
                       col_types = c("ccn"),
                       skip = 1) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "UTC", format = "%m/%d/%Y %H:%M"),
         `rain-m` = `rain-m`/1000) %>%
  filter(!is.na(`rain-m`))

rainfixes <- rbind(spl2016fix, spl2017fix, spl2018fix, wor2018fix, wor2018fix2, mcn2018fix,
                   whi2019fix, whi2019fix1, mcn2019fix, mar2019fix)

rainfixes$watershed[rainfixes$watershed=="EST"] <- "spiritctl"
rainfixes$watershed[rainfixes$watershed=="AMW"] <- "worlectl"
rainfixes$watershed[rainfixes$watershed=="CNC"] <- "mcnayctl"
rainfixes$watershed[rainfixes$watershed=="ADU"] <- "whitetrt"
rainfixes$watershed[rainfixes$watershed=="MIW"] <- "marshctl"

#The ASOS AWOS data is cumulative by hour so I have to take the max value for each hour to use
rainfixes <- rainfixes %>% 
  mutate(dayhour = floor_date(date_time, "hour"),
         watershed = as.factor(watershed)) %>%
  group_by(watershed, dayhour) %>% 
  summarize_at(vars('rain-m'), max) %>%
  rename(date_time = dayhour) %>%
  ungroup()


#replacing rhodesctl rain with worlectl rain for 9/15/16
s <- filter(STRIPS2Helmers::rain, watershed == "worlectl" & date_time >= as.Date("2016-09-15") & 
              date_time <= as.Date("2016-09-16"))

rain <- STRIPS2Helmers::rain %>%
  mutate(`rain-m` = replace(`rain-m`, date_time >= as.Date("2016-09-15") & 
                              date_time <= as.Date("2016-09-16") & 
                              watershed == "rhodestrt", s$`rain-m`)) %>% 
  filter(!(date_time>= min(spl2016fix$date_time) & date_time <= max(spl2016fix$date_time) & 
             watershed == "spiritctl"))%>%
  filter(!(date_time>= min(spl2017fix$date_time) & date_time <= max(spl2017fix$date_time) & 
             watershed == "spiritctl"))%>%
  filter(!(date_time>= min(spl2018fix$date_time) & date_time <= max(spl2018fix$date_time) & 
             watershed == "spiritctl"))%>%
  filter(!(date_time>= min(mcn2018fix$date_time) & date_time <= max(mcn2018fix$date_time) & 
             watershed == "mcnayctl"))%>%
  filter(!(date_time>= min(wor2018fix$date_time) & date_time <= max(wor2018fix$date_time) & 
             watershed == "worlectl"))%>%
  filter(!(date_time>= min(wor2018fix2$date_time) & date_time <= max(wor2018fix2$date_time) & 
             watershed == "worlectl"))%>%
  filter(!(date_time>= min(whi2019fix$date_time) & date_time <= max(whi2019fix$date_time) & 
             watershed == "whitetrt"))%>%
  filter(!(date_time>= min(whi2019fix1$date_time) & date_time <= max(whi2019fix1$date_time) & 
             watershed == "whitetrt"))%>%
  filter(!(date_time>= min(mcn2019fix$date_time) & date_time <= max(mcn2019fix$date_time) & 
             watershed == "mcnayctl"))%>%
  filter(!(date_time>= min(mar2019fix$date_time) & date_time <= max(mar2019fix$date_time) & 
             watershed == "marshctl"))%>%
  
  rbind(rainfixes) %>%
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


saveRDS(myrain, file = "~/prairiestrips/raindataallyears.Rda")

rm(list=setdiff(ls(), c("rain", "myrain")))


# 
# #testing mar problems
# test <- rainfixes %>% mutate(year = year(date_time)) %>%
#   group_by(watershed, year) %>%
#   summarize_at(vars('rain-m'), sum)
# 
# test1 <- rainfixes %>% mutate(year = year(date_time)) %>% 
#   filter(watershed == "marshctl" & year == "2019")
