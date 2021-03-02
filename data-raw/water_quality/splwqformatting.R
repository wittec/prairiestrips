
# #THIS IS FOR APPENDING SPL WATER QUALITY DATA TO THE "RUNOFF" WA --------

rm(list=ls(all=TRUE))

library(dplyr)
library(tidyr)


# IMPORTING SPL WATER QUALITY FILES AS I GET THEM -------------------------


spl2018wq <- read.csv("~/STRIPS2Helmers/data-raw/water_quality/2018/Hoien Nutrient_Sediment data 2018.csv", header = T) %>%
  select(Collected.Date, Environmental.Location, Analyte, Value) %>%
  rename(date = Collected.Date) %>%
  spread(Analyte, Value) %>%
  mutate(sampleid = 3300:3311,
         year = 2018)

spl2019wq <- read.csv("~/STRIPS2Helmers/data-raw/water_quality/2019/Hoien Nutrient_Sediment data 2019.csv", header = T) %>%
  select(Collected.Date, Environmental.Location, Analyte, Value) %>%
  rename(date = Collected.Date) %>%
  spread(Analyte, Value) %>%
  mutate(sampleid = 4300:4301,
         year = 2019)


# MERGING FILES AND FORMATTING DATA ---------------------------------------


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
         "Total Nitrogen (nitrate+nitrite + TKN) (mg N/L)" = "Total Kjeldahl Nitrogen as N"
        ) %>%
  select(c(7, 8, 9, 10, 11, 3, 14, 12, 13, 5, 17, 15, 16, 19, 18, 4, 22, 20, 21, 23, 6)) #%>%

  splrunoff$`Nitrate + nitrite (mg N/L)` <- recode_factor(splrunoff$`Nitrate + nitrite (mg N/L)`, '<0.05' = '0.025')
  splrunoff$`Nitrate + nitrite (mg N/L)` <- recode_factor(splrunoff$`Nitrate + nitrite (mg N/L)`, '<0.050' = '0.025')


# SPLITTING UP DATA BY YEAR AND APPENDING TO CORRESPONDING WATER Q --------

spl2018 <- filter(splrunoff, year == 2018)
spl2019 <- filter(splrunoff, year == 2019)

write.table(spl2018, file = "~/STRIPS2Helmers/data-raw/water_quality/2018/runoff.csv", append = TRUE, sep = ",", row.names = F, col.names = F)
write.table(spl2019, file = "~/STRIPS2Helmers/data-raw/water_quality/2019/runoff.csv", append = TRUE, sep = ",", row.names = F, col.names = F)


