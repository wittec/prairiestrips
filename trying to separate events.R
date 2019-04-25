
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)

d <- readRDS(file = "~/prairiestrips/clippedrainandflowdataallyears.Rda")
# 
# e <- d %>%
#   filter(level != "NA") %>%
#   filter(level != 0) %>%
#   select(date_time, site, watershed, year, level) %>% 
#   group_by(site, year) %>%
#   
#   spread(watershed, level) %>%
#   ungroup %>%
#   group_by(site, year) %>%
#   mutate(event = (c(0, cumsum(diff(date_time) > 240))))
# 
# 
# events <- e %>% 
#   gather(watershed, level, armctl:worletrt) %>% 
#   filter(level != "NA") %>%
#   group_by(site, year, event) %>%
#   mutate(maxlevel = max(level)) %>%
#   filter(maxlevel > 0.035)  #removes all events when max flume level was under 0.035m


e <- d %>%
  filter(flow != "NA") %>%
  filter(flow != 0) %>%
  select(date_time, site, watershed, year, flow) %>%
  group_by(site, year) %>%
  spread(watershed, flow) %>%
  ungroup %>%
  group_by(site, year) %>%
  mutate(event = (c(0, cumsum(diff(date_time) > 240))))

events <- e %>%
  gather(watershed, flow, armctl:worletrt) %>%
  filter(flow != "NA") %>%
  group_by(site, year, event)

###NOT WORKING YET!!! #THIS PUTS IN ALL OF THE SAMPLID'S INTO EVENTS
ids <- STRIPS2Helmers::runoff %>% filter(sampleID != "NA") %>% 
  arrange(sampleID) %>% select(-level, -flow) %>%
  mutate(date_time = ceiling_date(date_time, "5 minutes")) %>%
  mutate(date_time = replace(date_time, watershed == "eiatrt" & sampleID == 1022, "2016-08-12 07:00:00"),
         date_time = replace(date_time, watershed == "spirittrt" & sampleID == "SPL3307", "2018-06-30 20:40:00"))
#   
# ids <- ids[match(unique(ids$sampleID), ids$sampleID),] %>%
#   mutate(date_time = ceiling_date(date_time, "5 minutes")) %>%
#   mutate(date_time = replace(date_time, watershed == "eiatrt" & sampleID == 1022, "2016-08-12 07:00:00"),
#          date_time = replace(date_time, watershed == "spirittrt" & sampleID == "SPL3307", "2018-06-30 20:40:00"))
#           #the line above - sampleID was getting clipped out so had to move it forward in time a little to fix

# testevents <- full_join(events, ids) %>% arrange(watershed, date_time)

events <- full_join(events, ids) %>% arrange(watershed, date_time)

sampleids <- events %>% filter(!is.na(sampleID)) %>% 
  select(sampleID, event, watershed) %>% 
  filter(!is.na(site)) %>%
  arrange(sampleID) %>%
  unique()

#####LEFT OFF HERE... NEED TO join the sampleids to events to spread the ids to all obs within event"

#####need to check to make sure all sampleids are included above





# 
# test <- testevents %>% filter(!is.na(sampleID))
# test1 <- test %>% filter(is.na(flow))
# write.csv(test1, "~/prairiestrips/clippedsamples.csv")
# 
# check <- d %>% filter(watershed == "armctl" & year == 2016)
# 
# spl18 <- runoff %>% filter(watershed == "spiritctl")




# #THIS ADDS  THE FLOW INTO EVENTS
# flowvariable <- d %>% select(date_time, watershed, flow)
# events <- left_join(events, flowvariable)

#ADD IN THE ACRES TO EVENTS
acres <- d %>% select(date_time, watershed, acres)
events <- left_join(events, acres) %>% arrange(watershed, date_time)
















#MAKE EVENTLIST TO MAP FUNTION ONTO EVENTS
eventlist <- split(events, list(events$site, events$year, events$event)) 
eventlist <- eventlist[sapply(eventlist, function(x) dim(x)[1]) > 0]

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

#ggsave(v, file=paste0(graphname,".jpg"), width = 6, height = 8)
  
  #return(v)
}


map(eventlist, eventgraphs)


# find event totals of runoff -----------------------------

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
  left_join(treatments) %>%
  unique() %>%
  arrange(year, event)


#do paired t test?... group by site and treatment


#  sed nutrients event sums -----------------------------------------------

# rain <- STRIPS2Helmers::rain %>%
#   mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
#          site = gsub("ctl", "", watershed),
#          site = gsub("trt", "", site),
#          year = lubridate::year(date_time)) %>%
#   rename(rain = `rain-m`) %>%
#   group_by(site,year) %>%
#   arrange(date_time) %>%
#   mutate(cumulative_rain = cumsum(rain * 39.3701), # convert to inches
#          watershed_year = paste(watershed,year,sep="_"))
# 
# myrain <- rain %>%
#   select(-watershed, -watershed_year, -treatment, -cumulative_rain)
# 
# wnames <- data.frame(site = c("arm","eia","marsh","mcnay","rhodes","spirit","white","worle"),
#                      full = c("Armstrong","E. IA Airport","Marshalltown","McNay",
#                               "Rhodes","Spirit Lake ","Whiterock","Worle"),
#                      codes = c("ARM", "EIA", "MAR", "MCN", "RHO", "SPL", "WHI", "WOR"))
# 
# wnames <- wnames %>%
#   mutate(site = as.character(site))
# 
# sed <- STRIPS2Helmers::runoff %>%
#   mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
#          site = gsub("ctl", "", watershed),
#          site = gsub("trt", "", site),
#          year = lubridate::year(date_time),
#          watershed = as.character(watershed),
#          sampleID = as.character(sampleID)) %>%
#   
#   left_join(myrain, by=c("date_time", "site", "year")) %>%
#   # filter(!is.na(flow), !is.na(rain_m)) %>%
#   group_by(watershed,year) %>%
#   do(HelmersLab::clip_flow(.))  # should we be using do() here?
# 
# sed$subtreatment <- sed$treatment 
# sed$subtreatment[sed$subtreatment != "control"] <- "prairie strip"
# sed$subtreatment[sed$watershed == "marshtrt"] <- "grass strip"
# 
# sed2 <- sed %>%
#   group_by(watershed,year) %>%
#   do(HelmersLab::spread_sampleID(.)) %>% 
#   filter(!is.na(sampleID), !is.na(flow)) %>%
#   left_join(STRIPS2Helmers::water_quality, by = "sampleID") %>%
#   tidyr::gather(analyte, value,
#                 `Nitrate + nitrite (mg N/L)`,
#                 `Orthophosphate (mg P/L)`,
#                 `TSS (mg/L)`) %>%
#   left_join(readr::read_csv("~/STRIPS2Helmers/data-raw/sitenamesandwatershedsizes.csv")) %>%
#   mutate(valueload = ((value*flow*5)/453592.37)/acres) %>%  #this converts the mg/L units of values into lbs/acre
#   group_by(watershed, year, analyte) %>%
#   filter(!is.na(value)) %>%
#   mutate(cumulative = cumsum(valueload)) %>%
#   left_join(wnames)
sed2 <- read_csv("~/prairiestrips/sed2.csv")

sampleIDs$sampleID <- as.numeric(as.character(sampleIDs$sampleID))

sed2eventsums <- sed2 %>%
  group_by(sampleID, analyte) %>%
  summarize(sum(valueload)) %>%
  rename(eventsum = 'sum(valueload)') %>%
  spread(analyte, eventsum) %>%
  left_join(sampleIDs) %>%
  select(-level, -flow) #%>%
  #mutate(eventstart = date_time)
    
#this isn't working yet
alleventdata <- left_join(flowinchesbyevent, sed2eventsums) 
 


test <- d %>% select(-sampleID)#mutate(sampleID = as.numeric(sampleID))
testjoin <- full_join(test, sed2) %>% filter(flow != 0)