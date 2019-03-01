# setwd("~/STRIPS2Helmers/vignettes/")

rm(list=ls(all=TRUE))

library(tidyverse)
library(lubridate)

# importing rain data -----------------------------------------------------

rain <- STRIPS2Helmers::rain %>%
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


# combine rain and flow data ----------------------------------------------

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


saveRDS(d, file = "~/prairiestrips/clippedrainandflowdataallyears.Rda")


# COMMENTED OUT BECAUSE I DON'T KNOW IF THIS IS WHAT WE WANT FOR FLOW - fixing up rain and flow graph lines ---------------------------------------------------
#IF I DO USE THIS, NEED TO CHANGE THE REFERENCED DATASET IN THE GRAPH PLOTS TO D2!!!

#THIS ADDS THE LAST DATE OF THE MONITORING SEASON TO EACH OF THE WATERSHEDS...THIS HELPS FOR GRAPHING PURPOSES,
#AS IT DRAWS ALL OF THE GRAPH LINES THE SAME LENGTH
# 
# library(tidyverse)
# library(lubridate)
# library(zoo)
# library(purrr)
# 
# d <- d %>%
#   mutate(date = date(date_time)) %>%
#   arrange(date_time)
# 
# max2016flowdate <- max(d$date_time[d$year=="2016"])
# 
# max2017flowdate <- max(d$date_time[d$year=="2017"])
# 
# max2018flowdate <- max(d$date_time[d$year=="2018"])
# 
# yearwatershedanalytesplit <- split(d, list(d$year, d$watershed))
# 
# applymaxdate <- function(data) 
# { t <- data %>%
#   select(-date_time, date_time)
# year <- max(d$year)
# 
# newrow <- tail(t, 1)#t[1, ]
# 
# newrow <-newrow %>%
#   mutate(date_time = ifelse(year == 2016, "2016-11-18 12:25:00", date_time)) %>%
#   mutate(date_time = ifelse(year == 2017, "2017-11-14 14:30:00", date_time)) %>%
#   mutate(date_time = ifelse(year == 2018, "2018-10-24 12:15:00", date_time)) %>%
#   mutate(date_time = as.POSIXct(date_time))
# 
# t <- t %>%
#   rbind(newrow)
# 
# }
# 
# d2 <- map_dfr(yearwatershedanalytesplit, applymaxdate)


# custom settings for graphs ----------------------------------------------

colorscale <- c(rain = "blue", 
                control = "black",
                treatment = "seagreen")
linescale <- c(rain = "dotted",
               control = "solid",
               treatment = "dashed")


# runoff and rain graph, all years -------------------------------

g <- ggplot(d, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line(size = 1) + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

g

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff2018.jpg", plot=g, width = 6, height=8)

# create table of final cumulative values for rain and flow in 2016---------------

endtable <- d %>% filter(year=="2016") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2016.csv")

# create table of final cumulative values for rain and flow in 2017---------------

endtable <- d %>% filter(year=="2017") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2017.csv")


# create table of final cumulative values for rain and flow in 2018---------------

endtable <- d %>% filter(year=="2018") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2018.csv")

#MAKING TABLE WITH 3 LETTER CODES INSTEAD OF FULL NAMES OF SITES
endtable <- d %>% filter(year=="2018") %>% group_by(codes, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = codes, Rain = rain, Control = control, Treatment = treatment)

write.csv(e1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2018sitecodes.csv")


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

saveRDS(sed2, file = "~/prairiestrips/clippedsedandnutdataallyears.Rda")


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
# write.csv(dayrainflow, file = ("C:/Users/Chris/Documents/prairiestrips/dailyflowrainandsed.csv"))
#  


# fixing up sediment and nutrient graph lines ---------------------------------------------------
#THIS ADDS THE LAST DATE OF THE MONITORING SEASON TO EACH OF THE WATERSHEDS...THIS HELPS FOR GRAPHING PURPOSES,
#AS IT DRAWS ALL OF THE GRAPH LINES THE SAME LENGTH

library(tidyverse)
library(lubridate)
library(zoo)
library(purrr)

sed2 <- sed2 %>%
  mutate(date = date(date_time)) %>%
  arrange(date_time)

max2016seddate <- max(sed2$date_time[sed2$year=="2016"])

max2017seddate <- max(sed2$date_time[sed2$year=="2017"])

max2018seddate <- max(sed2$date_time[sed2$year=="2018"])

yearwatershedanalytesplit <- split(sed2, list(sed2$year, sed2$watershed, sed2$analyte))

applymaxdate <- function(data) 
{ t <- data %>%
  select(-date_time, date_time)
year <- max(sed2$year)

newrow <- tail(t, 1)#t[1, ]

newrow <-newrow %>%
  mutate(date_time = ifelse(year == 2016, "2016-11-03 06:25:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2017, "2017-06-28 11:25:00", date_time)) %>%
  mutate(date_time = ifelse(year == 2018, "2018-10-11 02:30:00", date_time)) %>%
  mutate(date_time = as.POSIXct(date_time))

t <- t %>%
  rbind(newrow)

}

sed3 <- map_dfr(yearwatershedanalytesplit, applymaxdate)


# nitrate graph all years-----------------------------------------------------------


no3graph <- ggplot(sed3 %>% 
         filter(analyte == "Nitrate + nitrite (mg N/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Nitrate - Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/no32018.jpg", plot=no3graph, width = 6, height=8)


# orthophosphate graph all years----------------------------------------------------

orthopgraph <- ggplot(sed3 %>% 
         filter(analyte == "Orthophosphate (mg P/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Phosphorus (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/orthop2018.jpg", plot=orthopgraph, width = 6, height=8)


# tss graph all years---------------------------------------------------------------

tssgraph <- ggplot(sed3 %>% 
         filter(analyte == "TSS (mg/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Total Suspended Solids (lbs/ac)') + 
  theme_bw() + 
  theme(legend.position = "bottom",
      legend.title    = element_blank(),
      axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/tss2018.jpg", plot=tssgraph, width = 6, height=8)

# create table of final cumulative values for nutrients in 2016 ---------------

nuttable <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2016") %>% summarize_at(c("cumulative"), max, na.rm = T)
n1 <- nuttable %>% spread(analyte, cumulative)
no3table <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable <- n1 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable <- no3table %>%
  left_join(orthotable, by = "full") %>%
  left_join(tsstable, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2016.csv")


# create table of final cumulative values for nutrients in 2017 ---------------

nuttable <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2017") %>% summarize_at(c("cumulative"), max, na.rm = T)
n1 <- nuttable %>% spread(analyte, cumulative)
no3table <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable <- n1 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable <- no3table %>%
  left_join(orthotable, by = "full") %>%
  left_join(tsstable, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2017.csv")

# create table of final cumulative values for nutrients in 2018 ---------------

nuttable <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2018") %>% summarize_at(c("cumulative"), max, na.rm = T)
n1 <- nuttable %>% spread(analyte, cumulative)
no3table <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable <- n1 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable <- no3table %>%
  left_join(orthotable, by = "full") %>%
  left_join(tsstable, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2018.csv")

#MAKING 2018 TABLE WITH THE SITE CODES INSTEAD OF FULL NAMES

nuttable <- sed2 %>% group_by(codes, treatment, analyte) %>% filter(year=="2018") %>% summarize_at(c("cumulative"), max, na.rm = T)
n1 <- nuttable %>% spread(analyte, cumulative)
no3table <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable <- n1 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable <- no3table %>%
  left_join(orthotable, by = "codes") %>%
  left_join(tsstable, by = "codes") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = codes, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2018sitecodes.csv")



# site specific graphs, can edit for which site you want --------
# 
# setwd("C:/Users/Chris/Documents/prairiestrips/graphs/")
#
# siterainrunplot <- ggplot(d %>% filter(site=="white"), aes(x = date_time, 
#                    y = y, 
#                    group = watershed, 
#                    linetype = treatment,
#                    color = treatment)) +
#   ggtitle("Rain and Surface Runoff") +
#   geom_line() + 
#   facet_grid(full~year, scales='free_x') + 
#   labs(x = '',  
#        y = 'Cumulative rainfall and runoff (inches)') + 
#   scale_color_manual(values = colorscale) +
#   scale_linetype_manual(values = linescale) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "whiterunoff.jpg", plot=siterainrunplot, width = 6, height=8)
# 
# 
#
# siteorthopgraph <- ggplot(sed3 %>% 
#                         filter(site=="white", analyte == "Orthophosphate (mg P/L)"), 
#                       aes(x = date_time, 
#                           y = cumulative,
#                           group = treatment,
#                           color = treatment,
#                           linetype = treatment)) + 
#   ggtitle("Surface Runoff Orthophosphate") +
#   geom_line() + 
#   scale_color_manual(values = colorscale) +
#   scale_linetype_manual(values = linescale) +
#   facet_grid(full ~ year, scales = 'free_x') + 
#   labs(x = '',  
#        y = 'Cumulative Dissolved Nitrogen (lbs/ac)') + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "whiteorthop.jpg", plot=siteorthopgraph, width = 6, height=8)
# 
# 
# sitetssgraph <- ggplot(sed3 %>% 
#                      filter(site=="white", analyte == "TSS (mg/L)"), 
#                    aes(x = date_time, 
#                        y = cumulative,
#                        group = treatment,
#                        color = treatment,
#                        linetype = treatment)) + 
#   ggtitle("Surface Runoff Sediment") +
#   geom_line() + 
#   scale_color_manual(values = colorscale) +
#   scale_linetype_manual(values = linescale) +
#   facet_grid(full ~ year, scales = 'free_x') + 
#   labs(x = '',  
#        y = 'Cumulative Total Suspended Solids (lbs/ac)') + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "whitetss.jpg", plot=sitetssgraph, width = 6, height=8)
# 
# 
# siteno3graph <- ggplot(sed3 %>% 
#                           filter(site=="white", analyte == "Nitrate + nitrite (mg N/L)"), 
#                         aes(x = date_time, 
#                             y = cumulative,
#                             group = treatment,
#                             color = treatment,
#                             linetype = treatment)) + 
#   ggtitle("Surface Runoff Nitrate - Nitrogen") +
#   geom_line() + 
#   scale_color_manual(values = colorscale) +
#   scale_linetype_manual(values = linescale) +
#   facet_grid(full ~ year, scales = 'free_x') + 
#   labs(x = '',  
#        y = 'Cumulative Runoff Nitrate - Nitrogen (lbs/ac)') + 
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "whiteno3.jpg", plot=siteno3graph, width = 6, height=8)
# 
