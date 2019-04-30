
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)

d <- readRDS(file = "~/prairiestrips/clippedrainandflowdataallyears.Rda")

e <- d %>%
  filter(flow != "NA") %>%
  filter(flow != 0) %>%
  select(date_time, site, watershed, year, flow) %>%
  group_by(site, year) %>%
  spread(watershed, flow) %>%
  ungroup %>%
  group_by(site, year) %>%
  mutate(event = (c(0, cumsum(diff(date_time) > 240)))) %>%
  arrange(site, year, date_time)

events <- e %>%
  gather(watershed, flow, armctl:worletrt) %>%
  arrange(watershed, date_time) %>%
  filter(flow != "NA")

ids <- STRIPS2Helmers::runoff %>% filter(sampleID != "NA") %>% 
  arrange(sampleID) %>% select(-level, -flow) %>%
  mutate(date_time = ceiling_date(date_time, "5 minutes")) %>%
  mutate(date_time = replace(date_time, watershed == "eiatrt" & sampleID == 1022, "2016-08-12 07:00:00"),
         date_time = replace(date_time, watershed == "spirittrt" & sampleID == "SPL3307", "2018-06-30 20:40:00"),
         date_time = replace(date_time, watershed == "mcnayctl" & sampleID == 3057, "2018-09-07 09:50:00"))

events <- full_join(events, ids) %>% 
  arrange(watershed, date_time) %>%
  ungroup()

#making sampleids to make sure all ids are in events...they are now
sampleids <- events %>% filter(!is.na(sampleID)) %>% 
  select(sampleID, event, watershed) %>% 
  filter(!is.na(event)) %>%
  arrange(sampleID)

#SPREADING SAMPLEID THROUGHOUT EVENT
events <- events %>%
  group_by(watershed, year, event) %>%
  mutate(sampleID = zoo::na.locf(sampleID, fromLast = TRUE, na.rm = FALSE),  # carry backward
         sampleID = zoo::na.locf(sampleID, fromLast = FALSE, na.rm = FALSE)) # carry forward

#ADD IN THE ACRES, etc. TO EVENTS
atsfc <- d %>% select(date_time, watershed, acres, treatment, subtreatment, full, codes)
treatments <- d %>% select(watershed, treatment) %>% unique()

# events table ------------------------------------------------------------

eventstable <- left_join(events, atsfc) %>% 
  left_join(treatments) %>%
  arrange(watershed, date_time) %>%
  left_join(STRIPS2Helmers::water_quality, by = "sampleID") %>%
  tidyr::gather(analyte, value,
                `Nitrate + nitrite (mg N/L)`,
                `Orthophosphate (mg P/L)`,
                `TSS (mg/L)`) %>%
  mutate(valueload = ((value*flow*5)/453592.37)/acres,) %>% #this converts the mg/L units of values into lbs/acre
  group_by(watershed, year, event, analyte) %>%
  summarize(eventflow = sum(flow),
            eventwqtotal = sum(valueload)) %>%
  left_join(readr::read_csv("~/STRIPS2Helmers/data-raw/sitenamesandwatershedsizes.csv")) %>%
  mutate(eventflowinches = eventflow * 231 * 5 / (acres * 6.273e6)) %>% # convert gpm to in^3 from 5 minutes normalize by watershed area
  spread(analyte, eventwqtotal) %>%                                     # after converting acres to square inches
  arrange(year, watershed)

#do paired t test?... group by site and treatment


# event graphs ------------------------------------------------------------

graphevents <- left_join(events, atsfc) %>% 
  arrange(watershed, date_time) %>%
  left_join(STRIPS2Helmers::water_quality, by = "sampleID") %>%
  tidyr::gather(analyte, value,
                `Nitrate + nitrite (mg N/L)`,
                `Orthophosphate (mg P/L)`,
                `TSS (mg/L)`) %>%
  mutate(valueload = ((value*flow*5)/453592.37)/acres,
         eventflowinches = flow * 231 * 5 / (acres * 6.273e6))
  
#MAKE EVENTLIST TO MAP FUNTION ONTO EVENTS
grapheventlist <- split(graphevents, list(graphevents$site, graphevents$year, graphevents$event)) 


# event graphs ---------------------------------------

eventgraphmaker <- function(data)
{
  v <- ggplot(data, 
              aes(x = date_time, 
                  y = eventflowinches,
                  group = watershed,
                  color = watershed)) +
    geom_line(size = 1) + 
    scale_x_datetime(date_labels= "%m/%d/%Y %H:%M:%S")
  
setwd("~/prairiestrips/graphs/flowevents/")
  
graphname <- paste0(head(data[5], n=1), "_", head(data[3], n=1), "_", (head(data[4], n=1)))

ggsave(v, file=paste0(graphname,".jpg"), width = 6, height = 8)
  
  #return(v)
}

map(grapheventlist, eventgraphmaker)



