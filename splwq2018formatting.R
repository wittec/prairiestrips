setwd("~/STRIPS2Helmers/data-raw/water_quality/2018/")

rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)

spl2018wq <- read.csv("Hoien Nutrient_Sediment data 2018.csv", header = T) %>%
  select(Collected.Date, Environmental.Location, Analyte, Value) %>%
  rename(date = Collected.Date) %>%
  spread(Analyte, Value) %>%
  mutate(sampleid = 3300:3311)

write.csv(spl2018wq, "spl2018wq.csv")
  
#GOING TO COPY and FORMAT THIS DATA AND PUT IT INTO SPLRUNOFF.CSV FILE TO BE USED, 
#THEN WILL MOVE THE SPL2018.CSV FILE I EXPORTED ABOVE INTO C:\Users\Chris\Box Sync\Documents\Data\runoff   
