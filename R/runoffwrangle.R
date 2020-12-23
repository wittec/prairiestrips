# 
rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)

setwd("~/STRIPS2Helmers/vignettes/")

load("~/prairiestrips/data/raindataallyears.rds")

#write.csv(myrain, file = "C:/Users/Chris/Documents/prairiestrips/csvdata/strips2rainallyearsfixed.csv")

# importing flow data and clipping when there was no rain for 24 hours-------------------------------------------

flow <- STRIPS2Helmers::runoff %>%
  filter(!is.na(flow)) %>%
  
  mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
         site = gsub("ctl", "", watershed),
         site = gsub("trt", "", site),
         year = lubridate::year(date_time),
         watershed = as.character(watershed),
         sampleID = as.character(sampleID)) %>%
  left_join(myrain, by=c("date_time", "site", "year")) %>%
  filter(!is.na(flow), !is.na(rain)) %>%
  group_by(watershed,year) %>%
  do(HelmersLab::clip_flow(.)) %>%
  
  left_join(readr::read_csv("../data-raw/sitenamesandwatershedsizes.csv")) %>%
  group_by(watershed,year) %>%
  mutate(
    cumulative_flow = cumsum(flow) * 231 * 5 / # convert gpm to in^3 from 5 minutes
      (acres * 6.273e6) )                 # normalize by watershed area
# after converting acres to square inches

flow$subtreatment <- flow$treatment 
flow$subtreatment[flow$subtreatment != "control"] <- "prairie strip"
flow$subtreatment[flow$watershed == "marshtrt"] <- "grass strip"

# combine rain and flow data ----------------------------------------------
load("~/STRIPS2Helmers/data/rain.rda")

rain <- rain %>%
  mutate(treatment = "rain",
         watershed = paste(site,"_rain", sep=""),
         y = cumulative_rain)

flow <- flow %>%
  mutate(y = cumulative_flow)

wnames <- data.frame(site = c("arm","eia","marsh","mcnay","rhodes","spirit","white","worle"),
                     full = c("Armstrong","E. IA Airport","Marshalltown","McNay",
                              "Rhodes","Spirit Lake ","Whiterock","Worle"),
                     codes = c("ARM", "EIA", "MAR", "MCN", "RHO", "SPL", "WHI", "WOR"))

wnames <- wnames %>%
  mutate(site = as.character(site))


d <- bind_rows(flow,rain) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("rain","control","treatment"))) %>%
  left_join(wnames)


save(d, file = "~/prairiestrips/data/clippedrainandflowdataallyears.rds")
#csv file below is too big to commit to git
write.csv(d, file = "C:/Users/Chris/Documents/prairiestrips/csvdata/clippedrainandflowdataallyears.csv")



# water quality (tss, no3, orthop, tn, tp) ----------------------------------------------------------------
sed <- STRIPS2Helmers::runoff %>%
  mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
         site = gsub("ctl", "", watershed),
         site = gsub("trt", "", site),
         year = lubridate::year(date_time),
         watershed = as.character(watershed),
         sampleID = as.character(sampleID)) %>%
  
  left_join(myrain, by=c("date_time", "site", "year")) %>%
  # filter(!is.na(flow), !is.na(rain_m)) %>%
  group_by(watershed,year) %>%
  do(HelmersLab::clip_flow(.))  # should we be using do() here?

sed$subtreatment <- sed$treatment 
sed$subtreatment[sed$subtreatment != "control"] <- "prairie strip"
sed$subtreatment[sed$watershed == "marshtrt"] <- "grass strip"

# mutate(sampleID = as.numeric(sampleID)) %>%
# group_by(watershed,year) %>%

# Remove any watershed-year where no sampleIDs exist
# mutate(anySampleID = any(!is.na(sampleID))) %>%
# filter(anySampleID) %>%

sed2 <- sed %>%
  group_by(watershed,year) %>%
  do(HelmersLab::spread_sampleID(.)) %>% 
  filter(!is.na(sampleID), !is.na(flow)) %>%
  left_join(STRIPS2Helmers::water_quality, by = "sampleID") %>%
  tidyr::gather(analyte, value,
                `Nitrate + nitrite (mg N/L)`,
                `Orthophosphate (mg P/L)`,
                `TSS (mg/L)`) %>%
  left_join(readr::read_csv("../data-raw/sitenamesandwatershedsizes.csv")) %>%
  mutate(valueload = ((value*flow*5)/453592.37)/acres) %>%  #this converts the mg/L units of values into lbs/acre
  group_by(watershed, year, analyte) %>%
  filter(!is.na(value)) %>%
  mutate(cumulative = cumsum(valueload)) %>%
  left_join(wnames)

write.csv(sed2, file = "~/prairiestrips/csvdata/sed2.csv", row.names = FALSE)
save(sed2, file = "~/prairiestrips/data/sed2.rds")

# Sediment summary by day all years-------------------------------------------------

daysed <- sed2 %>% filter(analyte == "TSS (mg/L)") %>%
  mutate(date = date(date_time),
         sitename = paste(full,treatment,sep=" ")
  ) %>%
  group_by(sitename, date) %>%
  summarise("TSS lbs/ac" = sum(valueload))

dayrainflowsed <- d %>%
  mutate(date = date(date_time),
         sitename = paste(full, treatment, sep = " "),
         flowin = flow * 231 * 5 / (acres * 6.273e6)
  ) %>%
  filter(treatment != "rain") %>%
  group_by(sitename, date, treatment) %>%
  summarise("rainday (in)" = sum(rain*39.3701), "flowday (in)" = sum(flowin)) %>%
  select(-treatment) %>%
  left_join(daysed)
# 
# write.csv(dayrainflowsed, file = ("C:/Users/Chris/Documents/prairiestrips/csvdata/dayrainflowsed.csv"))
#  


# fixing up sediment and nutrient graph lines ---------------------------------------------------
#THIS ADDS THE LAST DATE OF THE MONITORING SEASON TO EACH OF THE WATERSHEDS...THIS HELPS FOR GRAPHING PURPOSES,
#AS IT DRAWS ALL OF THE GRAPH LINES THE SAME LENGTH

library(tidyverse)
library(lubridate)
library(zoo)
library(purrr)

# sed2 <- read.csv(file = "~/prairiestrips/csvdata/sed2.csv") %>% mutate(date_time = ymd_hms(date_time))
#load(file = "~/prairiestrips/data/sed2.rds")

sed2 <- sed2 %>%
  mutate(date = date(date_time)) %>%
  arrange(date_time)

max2016seddate <- max(sed2$date_time[sed2$year=="2016"])

max2017seddate <- max(sed2$date_time[sed2$year=="2017"])

max2018seddate <- max(sed2$date_time[sed2$year=="2018"])

max2019seddate <- max(sed2$date_time[sed2$year=="2019"])

max2020seddate <- max(sed2$date_time[sed2$year=="2020"])

yearwatershedanalytesplit <- split(sed2, list(sed2$year, sed2$watershed, sed2$analyte)) #makes a list of groups of "sed2" dataset based on year, watershed, and analyte

applymaxdate <- function(data) 
{ t <- data %>% #reads in the data
  select(-date_time, date_time) #this just moves date_time to end of dataset
year <- max(sed2$year) #finds the year of the data

newrow <- tail(t, 1)#t[1, ] #takes a copy of the last observation of the data

newrow <-newrow %>% #inserts the maxdate (eg. "max2016date") into the date_time column
  mutate(date_time = ifelse(year == 2016, "2016-11-03 06:25:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2017, "2017-06-28 11:25:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2018, "2018-10-11 02:30:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2019, "2019-10-21 20:00:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2020, "2020-06-22 21:10:00", date_time)) %>%
  mutate(date_time = as.POSIXct(date_time))

t <- t %>%
  rbind(newrow) #adds the new row onto the end of the data

}

sed3 <- map_dfr(yearwatershedanalytesplit, applymaxdate)
save(sed3, file = "C:/Users/Chris/Documents/prairiestrips/data/sed3.rds")
write.csv(sed3, file = "C:/Users/Chris/Documents/prairiestrips/csvdata/sed3.csv")
