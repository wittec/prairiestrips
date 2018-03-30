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

# ggplot(rain, aes(x = date_time, y = cumulative_rain, 
#               group = watershed_year)) +
#   geom_line() + 
#   facet_grid(site ~ year, scales = 'free_x') + 
#   labs(x = '', y = 'Cumulative rainfall (inches)') + 
#   theme_bw()


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

#test <- readr::read_csv("../data-raw/sitenamesandwatershedsizes.csv")


ggplot(flow, aes(x = date_time, y = cumulative_flow, 
              group = watershed, linetype = treatment)) +
  geom_line(size=3) + 
  facet_grid(site~year, scales='free') + 
  theme_bw()
  
# ggplot(flow, aes(x = date_time, y = cumulative_flow, 
#               group = watershed, linetype = treatment)) +
#   geom_line(size=3) + 
#   facet_grid(site ~ year, scales='free') + 
#   theme_bw()

# Combine flow and rain
rain <- rain %>%
  mutate(treatment = "rain",
         watershed = paste(site,"_rain", sep=""),
         y = cumulative_rain)

flow <- flow %>%
  mutate(y = cumulative_flow)

wnames <- data.frame(site = c("arm","eia","marsh","mcnay","rhodes","spirit","white","worle"),
                     full = c("Armstrong","E. IA Airport","Marshaltown","McNay",
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

#test <- d %>% filter(codes=="SPL")


g <- ggplot(d, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line() + 
  facet_grid(codes~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff.jpg", plot=g, width = 6, height=8)

# 
# # Multiply flow by 10
# g <- ggplot(d %>% left_join(wnames), aes(x = date_time, 
#                                          y = y*ifelse(treatment != "rain", 10, 1), 
#                                          group = watershed, 
#                                          linetype = treatment,
#                                          color = treatment)) +
#   geom_line() + 
#   facet_grid(full~year, scales='free_x') + 
#   labs(x = '',  
#        y = 'Cumulative runoff (x10) and rainfall in inches') + 
#   scale_color_manual(values = colorscale) +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         legend.title    = element_blank())
# 
# ggsave(filename = "runoff_x10.jpg", plot=g, width = 6, height=8)
# 
# 


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
  ##########################################
#ADDING THIS STUFF IN


#############################################
  left_join(wnames)

#############################################
#THIS IS A SED SUMMARIZATION BY DAY FOR ELISE
elisesed <- sed2 %>% filter(analyte == "TSS (mg/L)") %>%
  mutate(date = date(date_time),
         sitename = paste(full,treatment,sep=" ")
        ) %>%
         group_by(sitename, date)
  
daysed <- summarise(elisesed, valueload = sum(valueload)) %>%
  arrange(sitename, date) %>%
  spread(sitename, valueload)

eliseflow <- d %>%
  mutate(date = date(date_time),
        sitname = paste(full, treatment, sep = " "), 
    flowin = flow * 231 * 5 /      # convert gpm to in^3 from 5 minutes
    (acres * 6.273e6))  %>%           # normalize by watershed area
                                   # after converting acres to square inches
  group_by(sitename, date)



##########LEFT OFF HERE


%>% 
  mutate(cumflowin = cumsum(flowin)
)
  
merge(eliseflow, addfullname, by = watershed)
dayflowsum <- daysum %>%
  select(-valueload) %>%
  arrange(sitename, date) %>%
  spread(sitename, flow)

test <- merge(daysedsum, dayflowsum, by = "date")

names(test) <- gsub(".x", " TSS", names(test))

names(test) <- gsub(".y", " Flow", names(test))
  
  
########################################
#testing stuff below

test1 <- sed2 %>%
  group_by(watershed, analyte, year)%>%
  full_join(test7)%>%
  arrange(watershed, analyte, date_time)%>%
  na.locf(, fromlast = T)
  
  #complete(watershed, date_time)


test2 <- as.data.frame(unique(sed2$date_time)) %>%
  rename(date_time = 'unique(sed2$date_time)')

test8 <- as.data.frame(seq(start(sed2$date_time, end(sed2$date_time, "5 min"))))


az <- as.data.frame(zoo(1:6))

bz <- as.data.frame(zoo(c(2,NA,1,4,5,2)))
bz1 <- na.locf(bz)
bz2 <- na.locf(bz, fromLast = TRUE)

z <- as.data.frame(zoo(c(0.007306621, 0.007659046, 0.007681013,
           0.007817548, 0.007847579, 0.007867313),
         as.Date(c("1993-01-01", "1993-01-09", "1993-01-16",
                   "1993-01-23", "1993-01-30", "1993-02-06"))))
g <-  seq(start(z), end(z), "day")
na.locf(z, xout = g)




# test2 <- as.data.frame(seq(ISOdate(2016,4,26), ISOdate(2017,6,19), by = "5 min")) %>%
#   rename(date_time = "seq(ISOdate(2016, 4, 26), ISOdate(2017, 6, 19), by = \"5 min\")")
#   
#   # 
# test3 <- merge(test2, test1, by = "date_time", all.x = T) %>%
#   arrange(watershed, year, date_time)
#   
#   colnames(date)
#   
# test4 <- sed2 %>%
#   mutate(date_time = as.factor(date_time),
#          analyte = as.factor(analyte),
#          watershed = as.factor(watershed)) %>%
#   complete(analyte, watershed, date_time)
# 
# df <- tibble(
#   group = c(1:2, 1),
#   item_id = c(1:2, 2),
#   item_name = c("a", "b", "b"),
#   value1 = 1:3,
#   value2 = 4:6
# )
# 
# test5 <- df %>%
#   complete(group, nesting(item_id, item_name))

test7 <- expand.grid(test6$watershed, test2$date_time) %>%
  rename(watershed = 'Var1', date_time = 'Var2')

test6<- as.data.frame(unique(test1$watershed)) %>%
  rename(watershed = `unique(test1$watershed)`)


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
  facet_grid(codes ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Nitrogen (lbs/ac)') + 
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
  facet_grid(codes ~ year, scales = 'free_x') + 
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
  facet_grid(codes ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Total Suspended Solids (lbs/ac)') + 
  theme_bw() + 
  theme(legend.position = "bottom",
      legend.title    = element_blank(),
      axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/tss.jpg", plot=tssgraph, width = 6, height=8)

#############################################################################
#THIS IS FOR lISA TO SEND TO EIA, edited to give to hoien at spirit lake

spirit <- d %>%
  filter(site=="spirit")

spiritrainrunplot <- ggplot(spirit, aes(x = date_time, 
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

ggsave(filename = "spiritrunoff.jpg", plot=spiritrainrunplot, width = 6, height=8)


spiritsed2 <- sed2%>%
  filter(site=="spirit")

spiritorthopgraph <- ggplot(spiritsed2 %>% 
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
       y = 'Cumulative Orthophosphate (lbs/ac)') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank())

ggsave(filename = "spiritorthop.jpg", plot=spiritorthopgraph, width = 6, height=8)


spirittssgraph <- ggplot(spiritsed2 %>% 
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

ggsave(filename = "spirittss.jpg", plot=spirittssgraph, width = 6, height=8)


