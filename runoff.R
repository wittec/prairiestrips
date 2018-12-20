# setwd("~/STRIPS2Helmers/vignettes/")

rm(list=ls(all=TRUE))

library("dplyr")
library("ggplot2")
library(lubridate)
library(tidyr)

# Rain
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

# Flow
myrain <- rain %>%
  select(-watershed, -watershed_year, -treatment, -cumulative_rain)

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


ggplot(flow, aes(x = date_time, y = cumulative_flow, 
              group = watershed, linetype = treatment)) +
  geom_line(size=3) + 
  facet_grid(site~year, scales='free') + 
  theme_bw()

# Combine flow and rain
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



# Customized colors
colorscale <- c(rain = "blue", 
                control = "black",
                treatment = "seagreen")
linescale <- c(rain = "dotted",
               control = "solid",
               treatment = "dashed")

# test graph for 2018 whiterock-----------------------------------------------------

flowtest <- STRIPS2Helmers::runoff %>%
  filter(!is.na(flow)) %>%
  
  mutate(treatment = ifelse(grepl("ctl", watershed), "control", "treatment"),
         site = gsub("ctl", "", watershed),
         site = gsub("trt", "", site),
         year = lubridate::year(date_time),
         watershed = as.character(watershed),
         sampleID = as.character(sampleID)) %>%
  
  left_join(myrain, by=c("date_time", "site", "year")) %>%
  filter(!is.na(flow), !is.na(rain)) %>%
  filter(year == "2018") %>%  # need to remove junk data
  filter(date_time <= "2018-06-19 19:05:00" | date_time >= "2018-06-24 15:15:00") %>%
  filter(date_time <= "2018-06-24 23:25:00" | date_time >= "2018-06-26 04:40:00") %>%
  filter(date_time <= "2018-06-26 11:25:00" | date_time >= "2018-06-30 18:55:00") %>%
  filter(date_time <= "2018-07-01 09:20:00" | date_time >= "2018-07-04 21:35:00") %>%
  filter(date_time <= "2018-07-05 03:20:00" | date_time >= "2018-09-25 08:10:00") %>%
  filter(date_time <= "2018-09-25 13:20:00" | date_time >= "2018-10-01 09:00:00") %>%
  group_by(watershed,year) %>%
  do(HelmersLab::clip_flow(.)) %>%
  
  left_join(readr::read_csv("../data-raw/sitenamesandwatershedsizes.csv")) %>%
  group_by(watershed,year) %>%
  mutate(
    cumulative_flow = cumsum(flow) * 231 * 5 / # convert gpm to in^3 from 5 minutes
      (acres * 6.273e6) )                 # normalize by watershed area
# after converting acres to square inches


# Combine flow and rain
raintest <- rain %>%
  mutate(treatment = "rain",
         watershed = paste(site,"_rain", sep=""),
         y = cumulative_rain) %>%
  filter(year == "2018")

flowtest <- flowtest %>%
  mutate(y = cumulative_flow)

wnames <- data.frame(site = c("arm","eia","marsh","mcnay","rhodes","spirit","white","worle"),
                     full = c("Armstrong","E. IA Airport","Marshalltown","McNay",
                              "Rhodes","Spirit Lake ","Whiterock","Worle"),
                     codes = c("ARM", "EIA", "MAR", "MCN", "RHO", "SPL", "WHI", "WOR"))

wnames <- wnames %>%
  mutate(site = as.character(site))


dtest <- bind_rows(flowtest,raintest) %>%
  mutate(treatment = factor(treatment, 
                            levels = c("rain","control","treatment"))) %>%
  left_join(wnames)


e <- ggplot(dtest, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line(size = 1) +
  geom_line(size = 1.5) +
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

e

# resume regular program --------------------------------------------------


g <- ggplot(d, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line() + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff2018.jpg", plot=g, width = 6, height=8)


###################################################
# Sediment
###################################################
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
  #HelmersLab::spread_sampleID() # should we be using do() here?

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

#############################################
#THIS IS A SED SUMMARIZATION BY DAY FOR ELISE
elisesed <- sed2 %>% filter(analyte == "TSS (mg/L)") %>%
  mutate(date = date(date_time),
         sitename = paste(full,treatment,sep=" ")
        ) %>%
         group_by(sitename, date) %>%
  summarise("TSS lbs/ac" = sum(valueload))

# daysed <- summarise(elisesed, valueload = sum(valueload)) %>%
#   arrange(sitename, date) %>%
#   spread(sitename, valueload)

eliseflow <- d %>%
  mutate(date = date(date_time),
        sitename = paste(full, treatment, sep = " "),
        flowin = flow * 231 * 5 / (acres * 6.273e6)
        ) %>%
  filter(treatment != "rain") %>%
  group_by(sitename, date, treatment) %>%
  summarise("rainday (in)" = sum(rain*39.3701), "flowday (in)" = sum(flowin)) %>%
  select(-treatment) %>%
  left_join(elisesed)
# 
# write.csv(eliseflow, file = ("C:/Users/Chris/Documents/prairiestrips/eliseflowrainandsed.csv"))
#  

##########################################
no3graph <- ggplot(sed2 %>% 
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

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/no3.jpg", plot=no3graph, width = 6, height=8)


#stuff between ### is only stuff I am messing with
##############################################################################

orthopgraph <- ggplot(sed2 %>% 
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

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/orthop.jpg", plot=orthopgraph, width = 6, height=8)


tssgraph <- ggplot(sed2 %>% 
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

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/tss.jpg", plot=tssgraph, width = 6, height=8)


# 2018 whi tss graph ------------------------------------------------------
sed2test <- sed2 %>%
  filter(year == "2018")

tssgraphtest <- ggplot(sed2test %>% 
                     filter(analyte == "TSS (mg/L)"), 
                   aes(x = date_time, 
                       y = cumulative,
                       group = treatment,
                       color = treatment,
                       linetype = treatment)) + 
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Total Suspended Solids (lbs/ac)') + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

tssgraphtest


#############################################################################

# THIS IS FOR lISA TO SEND TO EIA, edited to give to hoien at spir --------

setwd("C:/Users/Chris/Documents/prairiestrips/graphs/")

white <- d %>%
  filter(site=="white")

whiterainrunplot <- ggplot(white, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  ggtitle("Rain and Surface Runoff") +
  geom_line() + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "whiterunoff.jpg", plot=whiterainrunplot, width = 6, height=8)


whitesed2 <- sed2%>%
  filter(site=="white")

whiteorthopgraph <- ggplot(whitesed2 %>% 
                        filter(analyte == "Orthophosphate (mg P/L)"), 
                      aes(x = date_time, 
                          y = cumulative,
                          group = treatment,
                          color = treatment,
                          linetype = treatment)) + 
  ggtitle("Surface Runoff Orthophosphate") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Cumulative Dissolved Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "whiteorthop.jpg", plot=whiteorthopgraph, width = 6, height=8)


whitetssgraph <- ggplot(whitesed2 %>% 
                     filter(analyte == "TSS (mg/L)"), 
                   aes(x = date_time, 
                       y = cumulative,
                       group = treatment,
                       color = treatment,
                       linetype = treatment)) + 
  ggtitle("Surface Runoff Sediment") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Cumulative Total Suspended Solids (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "whitetss.jpg", plot=whitetssgraph, width = 6, height=8)


whiteno3graph <- ggplot(whitesed2 %>% 
                          filter(analyte == "Nitrate + nitrite (mg N/L)"), 
                        aes(x = date_time, 
                            y = cumulative,
                            group = treatment,
                            color = treatment,
                            linetype = treatment)) + 
  ggtitle("Surface Runoff Nitrate - Nitrogen") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Cumulative Runoff Nitrate - Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "whiteno3.jpg", plot=whiteno3graph, width = 6, height=8)


# for Lisa to bring to DC -------------------------------------------------
r2017 <- d %>%
  filter(year=="2017")

rainrunplot2017 <- ggplot(r2017, aes(x = date_time, 
                                        y = y, 
                                        group = watershed, 
                                        linetype = treatment,
                                        color = treatment)) +
  ggtitle("2017 Rain and Surface Runoff") +
  geom_line() + 
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/runoff2017.jpg", plot=rainrunplot2017, width = 6, height=8)

r2016 <- d %>%
  filter(year=="2016")

rainrunplot2017 <- ggplot(r2016, aes(x = date_time, 
                                     y = y, 
                                     group = watershed, 
                                     linetype = treatment,
                                     color = treatment)) +
  ggtitle("2016 Rain and Surface Runoff") +
  geom_line() + 
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/runoff2016.jpg", plot=rainrunplot2017, width = 6, height=8)

sed2017 <- sed2%>%
  filter(year=="2017")

orthopgraph2017 <- ggplot(sed2017 %>% 
                              filter(analyte == "Orthophosphate (mg P/L)"), 
                            aes(x = date_time, 
                                y = cumulative,
                                group = treatment,
                                color = treatment,
                                linetype = treatment)) + 
  ggtitle("2017 Surface Runoff Dissolved Phosphorus") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative Orthophosphate (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/orthop2017.jpg", plot=orthopgraph2017, width = 6, height=8)

sed2016 <- sed2%>%
  filter(year=="2016")

orthopgraph2016 <- ggplot(sed2016 %>% 
                            filter(analyte == "Orthophosphate (mg P/L)"), 
                          aes(x = date_time, 
                              y = cumulative,
                              group = treatment,
                              color = treatment,
                              linetype = treatment)) + 
  ggtitle("2016 Surface Runoff Dissolved Phosphorus") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative Orthophosphate (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/orthop2016.jpg", plot=orthopgraph2016, width = 6, height=8)


tssgraph2017 <- ggplot(sed2017 %>% 
                           filter(analyte == "TSS (mg/L)"), 
                         aes(x = date_time, 
                             y = cumulative,
                             group = treatment,
                             color = treatment,
                             linetype = treatment)) + 
  ggtitle("2017 Surface Runoff Sediment") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative Total Suspended Solids (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/tss2017.jpg", plot=tssgraph2017, width = 6, height=8)


tssgraph2016 <- ggplot(sed2016 %>% 
                         filter(analyte == "TSS (mg/L)"), 
                       aes(x = date_time, 
                           y = cumulative,
                           group = treatment,
                           color = treatment,
                           linetype = treatment)) + 
  ggtitle("2016 Surface Runoff Sediment") +
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_wrap(~full, ncol = 2) + 
  labs(x = '',  
       y = 'Cumulative Total Suspended Solids (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "~/prairiestrips/graphs/tss2016.jpg", plot=tssgraph2016, width = 6, height=8)

# trying to extend lines of graphs to the end of x axis -------------------

sed2daterange <- ungroup(sed2) %>%
  mutate(date = date(date_time)) %>%
  arrange(date_time)

range(sed2daterange$date_time)

library(lubridate)
library(zoo)
graphrange <- as.data.frame(seq(ymd_hms('2016-03-09 14:10:00'), ymd_hms('2017-11-14 14:30:00'), by = '5 min')) %>%
  rename(date_time = "seq(ymd_hms(\"2016-03-09 14:10:00\"), ymd_hms(\"2017-11-14 14:30:00\"), by = \"5 min\")")

names(graphrange)
library(purrr)
t <- ungroup(sed2) %>%
  filter(analyte == "Nitrate + nitrite (mg N/L)") %>%
  group_by(watershed, year) %>%
  arrange(year, watershed)

watersheds <- list(unique(sed2$watershed))
analytes <- list(unique(sed2$analyte))
apply <- list(watersheds, analytes)
applygraphrange <- function(x) { s <- filter(t, watershed == x)
siterange <- left_join(graphrange, s)
}
testmap <- map_dfr(watersheds, applygraphrange)



###NEED TO MAKE A NEW LIST OF THE COMBOS OF WATERSHEDS AND ANALYTES?

#t <- sed2 %>% group_by(watershed, year) %>% filter(analyte == "Nitrate + nitrite (mg N/L)") %>%full_join(graphrange)




# u <- sed2 %>%
#   select(watershed, date_time, cumulative)
#   
# v <- graphrange %>%
#   left_join(u)
# 
# testrange <- graphrange %>%
#   full_join(sed2) %>%
#   group_by(watershed, year, analyte) %>%
#   arrange(watershed, analyte, date_time) %>%
#   mutate(cumulative = na.locf(cumulative, na.rm = F),
#          treatment = na.locf(treatment, na.rm = F),
#          codes = na.locf(codes, na.rm = F)#,
#          #year = na.locf(year, na.rm = F)
#          ) 
# 
# t <- filter(testrange, watershed == "armctl", analyte == "Nitrate + nitrite (mg N/L)", year == "2016") %>%
#   mutate(month = month(date_time))


ggplot(data = testrange %>%
         filter(analyte == "Nitrate + nitrite (mg N/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line() + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(codes ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))


