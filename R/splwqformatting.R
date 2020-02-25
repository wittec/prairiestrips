
rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)

spl2018wq <- read.csv("~/STRIPS2Helmers/data-raw/water_quality/2018/Hoien Nutrient_Sediment data 2018.csv", header = T) %>%
  select(Collected.Date, Environmental.Location, Analyte, Value) %>%
  rename(date = Collected.Date) %>%
  spread(Analyte, Value) %>%
  mutate(sampleid = 3300:3311)

spl2019wq <- read.csv("~/STRIPS2Helmers/data-raw/water_quality/2019/Hoien Nutrient_Sediment data 2019.csv", header = T) %>%
  select(Collected.Date, Environmental.Location, Analyte, Value) %>%
  rename(date = Collected.Date) %>%
  spread(Analyte, Value) %>%
  mutate(sampleid = 4300:4301)

splrunoff <- spl2018wq %>%
  rbind(spl2019wq) %>%
  mutate(sampleid = paste0("SPL", sampleid),
         "Date sampled" = "na",
         "Plot ID" = "na",
         "Bottle" = "na",
         "Nitrate + nitrite Analysis Date" = "na",
         "Total Kjeldahl Nitrogen Analysis Date" = "na",
         "TKN (ammonia + organic N) (mg N/L)" = "na",
         "ph1" = "na",
         "DRP Analysis Date" = "na",
         "Orthophosphate (mg P/L)" = "na",
         "ph2" = "na",
         "Total Phosphorus Analysis Date" = "na",
         "ph3" = "na",
         "TKP analysis date" = "na",
         "TKP (mg P/L)" = "na",
         "ph4" = "na",
         "ph5" = "na"
         ) %>%
  rename("Nitrate + nitrite (mg N/L)" = "Nitrate + Nitrite nitrogen as N",
         "Total Phosphorus (mg P/L)" = "Phosphorus",
         "TSS (mg/L)" = "Total Suspended Solids",
         "Sample ID" = sampleid,
         "Total Nitrogen (nitrate+nitrite + TKN) (mg N/L)" = "Total Kjeldahl Nitrogen as N",
         
        ) %>%
  select(c(7, 8, 9, 10, 11, 3, 14, 12, 13, 5, 17, 15, 16, 19, 18, 4, 22, 20, 21, 23, 6))

bla <-rep(NA,dim(splrunoff)[2])
bla <- rbind(bla, bla, bla)

write.table(bla, file = "~/STRIPS2Helmers/data-raw/water_quality/2018/splrunofftest.csv", sep = ",", row.names = F)
write.table(splrunoff, file = "~/STRIPS2Helmers/data-raw/water_quality/2018/splrunofftest.csv", append = TRUE, sep = ",", row.names = F)

#GOING TO FORMAT THIS DATA (add 4 rows above column headers) AND PUT IT INTO DATA-RAW/2018/SPLRUNOFF.CSV


