
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)

d <- readRDS(file = "~/prairiestrips/clippedrainandflowdataallyears.Rda")

e <- d %>%
  filter(level != "NA") %>%
  filter(level != 0) %>%
  select(date_time, site, watershed, year, level) %>% 
  group_by(site, year) %>%
  
  spread(watershed, level) %>%
  ungroup %>%
  group_by(site, year) %>%
  mutate(event = (c(0, cumsum(diff(date_time) > 240))))


events <- e %>% 
  gather(watershed, level, armctl:worletrt) %>% 
  filter(level != "NA") %>%
  group_by(site, year, event) %>%
  mutate(maxlevel = max(level)) %>%
  filter(maxlevel > 0.035)

#THIS PUTS IN ALL OF THE SAMPLID'S INTO EVENTS (SOME WERE FILTERED OUT WHEN MAKING
#"EVENTS")
t <- d %>% select(date_time, sampleID)
events <- left_join(events, t)

#THIS ADDS  THE FLOW INTO EVENTS
flowvariable <- d %>% select(date_time, watershed, flow)
events <- left_join(events, flowvariable)

#ADD IN THE ACRES TO EVENTS
acres <- d %>% select(date_time, watershed, acres)
events <- left_join(events, acres)

#MAKE EVENTLIST TO MAP FUNTION ONTO EVENTS
eventlist <- split(events, list(events$site, events$year, events$event)) 
eventlist <- eventlist[sapply(eventlist, function(x) dim(x)[1]) > 0]

#CAN USE IDS IN FUNCTION TO INSERT INTO GRAPHS WHEN SAMPLES WERE COLLECTED...
#HOWEVER!!!!... CURRENTLY THE IDS ARE NOT INSERTED BY SITE, SO NOT COMPLETELY USEFUL
ids <- STRIPS2Helmers::runoff %>% filter(sampleID != "NA")

# event graphs ---------------------------------------

eventgraphs <- function(data)
{
  v <- ggplot(data, 
              aes(x = date_time, 
                  y = flow,
                  group = watershed,
                  color = watershed)) +
    geom_line(size = 1) + 
    scale_x_datetime(date_labels= "%m/%d/%Y %H:%M:%S")
  
  #x <- v + geom_vline(data = ids, xintercept = ids$date_time, color = "blue",
   #                                   size = 0.25) 
  # + 
  #   geom_text(data = ids, aes(label = ids$samplID, x = date_time, y = -Inf), angle = 90, 
  #             inherit.aes = F, hjust = -.5, vjust = -.5)
  #   
     
  #facet_grid(site~year, scales='free_x')
  
setwd("~/prairiestrips/graphs/flowevents/")
  
graphname <- paste0(head(data[5], n=1), "_", head(data[3], n=1), "_", (head(data[4], n=1)))

ggsave(v, file=paste0(graphname,".jpg"), width = 6, height = 8)
  
  #return(v)
}


map(eventlist, eventgraphs)


# find event totals of runoff, sed, nutrients -----------------------------

#TO GET EVENT FLOW SUMS, i NEED TO MAKE NEW EVENTLIST AND SPLIT BY WATERSHED NOT SITE
flowsumeventlist <- split(events, list(events$watershed, events$year, events$event))

flowsumeventlist <- flowsumeventlist[sapply(flowsumeventlist, function(x) dim(x)[1]) > 0]


sumflow <- function(data)
{
  watershedsizes <- readr::read_csv("~/STRIPS2Helmers/data-raw/sitenamesandwatershedsizes.csv") %>%
    select(-site)
  
  f <- summarize(data, flowsum = sum(data$flow)) %>%
    mutate(watershed = head(data$watershed, n=1),
           eventstart = head(data$date_time, n=1)) %>%
    left_join(watershedsizes) %>%
    mutate(flowinches = flowsum* 231 * 5 / # convert gpm to in^3 from 5 minutes
                         acres * 6.273e6)  # normalize by watershed area
                                          # after converting acres to square inches
}

treatments <- d %>% select(watershed, treatment)

flowinchesbyevent <- map(flowsumeventlist, sumflow) %>%
  #bind_rows(.id = "column_label") %>%
  bind_rows() %>%
  filter(flowsum != 0) %>%
  left_join(treatments)

#do paired t test?... group by site and treatment

#sed, nutrient event sums
sed2 <- readr::read_csv("~/prairiestrips/clippedsedandnutdataallyears.Rda") %>%


 
















myrain <- readRDS(file = "~/prairiestrips/raindataallyears.Rda")

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

sed2 <- sed %>%
  group_by(watershed,year) %>%
  do(HelmersLab::spread_sampleID(.)) %>% 
  #filter(!is.na(sampleID), !is.na(flow)) %>%
  
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

saveRDS(sed2, file = "~/prairiestrips/clippedsedandnutdataallyears.Rda")

