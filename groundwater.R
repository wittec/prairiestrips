# setwd("~/prairiestrips/groundwater/data-raw/waterquality/")

rm(list=ls(all=TRUE))


library("dplyr")
library("tidyr")
library("ggplot2")


gw2016 <- read.csv("./2016/2016gwno3results2.csv", skip = 21, header = T) %>%
  select(ID..., NOx.result..mg.N.L.) %>%
  rename(id = ID..., no3mgL = NOx.result..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))


gw2016a <- read.csv("./2016/2016gwno3results.csv", skip = 20, header = T) %>%
  select(ID..., Nitrate.nitrite..mg.N.L.) %>%
  rename(id = ID..., no3mgL = Nitrate.nitrite..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2016 <- rbind(gw2016, gw2016a)

gw2016codes <- read.csv("2016/2016gwcodes.csv", header = T) %>%
  select(-Sample.Source) %>%
  filter(!is.na(ID.)) %>%
  rename(id = ID., position = Position, site = Site, date = Date) %>%
  mutate(year = "2016",
         month = 12,
         position = gsub("TRT-Top", "TRT-top", position),
         position = gsub("TRT-Bot", "TRT-bot", position),
         position = gsub("Toe-Top", "TRT-top", position),
         position = gsub("Toe-Bot", "TRT-bot", position),
         position = gsub("CTL", "CTL-bot", position),
         position = gsub("CTL-bot-Bot", "CTL-bot", position),
         position = gsub("CTL-bot-Top", "CTL-top", position)) %>%
  filter(!is.na(site)) %>%
  left_join(gw2016, by = c("id"))
# 
# gw2016codesa <- xlsx::read.xlsx("../data-raw/water_quality/2016/CopyofSTRIPSgroundwaternitrateresults.xlsx", sheetName = "Lab Codes") %>%
#   select(-Sample.Source) %>%
#   rename(id = ID., position = Position, site = Site, date = Date) %>%
#   mutate(year = "2016",
#          month = 12,
#          position = gsub("TRT-Top", "TRT-top", position),
#          position = gsub("TRT-Bot", "TRT-bot", position),
#          position = gsub("Toe-Top", "TRT-top", position),
#          position = gsub("Toe-Bot", "TRT-bot", position),
#          position = gsub("CTL", "CTL-bot", position),
#          position = gsub("CTL-bot-Bot", "CTL-bot", position),
#          position = gsub("CTL-bot-Top", "CTL-top", position),
#          date = gsub("March 2016", "March", date)) %>%
#   left_join(gw2016a, by = c("id")) %>%
#   filter(!is.na(site))
# 
# gw2016codes <- rbind(gw2016codes, gw2016codesa)

gw2016codes$year[gw2016codes$id < 5000 ] = "2015"
gw2016codes$date[gw2016codes$id < 5000 ] <- "Dec."
gw2016codes$date[gw2016codes$id < 4200 ] <- "Sept."
gw2016codes$month[gw2016codes$date == "March" ] <- 3
gw2016codes$month[gw2016codes$date == "April" ] <- 4
gw2016codes$month[gw2016codes$date == "May" ] <- 5
gw2016codes$month[gw2016codes$date == "June" ] <- 6
gw2016codes$month[gw2016codes$date == "July" ] <- 7
gw2016codes$month[gw2016codes$date == "Aug." ] <- 8
gw2016codes$month[gw2016codes$date == "Sept." ] <- 9
gw2016codes$month[gw2016codes$date == "Oct." ] <- 10
gw2016codes$month[gw2016codes$date == "Nov." ] <- 11
#gw2016codes$newdate <- paste(gw2016codes$date, gw2016codes$year)
gw2016codes$sitepos <- paste(gw2016codes$site, gw2016codes$position)
gw2016codes$drpmgL <- "NA"
gw2016codes$sitepos[gw2016codes$sitepos== "Guthrie Top"] <- "Guthrie TRT-top"
gw2016codes$sitepos[gw2016codes$sitepos== "Guthrie Bottom"] <- "Guthrie TRT-bot"
# gw2016codes <- gw2016codes %>% 
#   select(-date)





gw2017 <- read.csv("./2017/STRIPS2017groundwaterresults.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

gw2017codes <- read.csv("./2017/STRIPS2017groundwatercodes.csv", skip = 2) %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") %>%
  left_join(gw2017, by = c("id")) %>%
  mutate(trt = gsub("trt", "TRT", trt),
         trt = gsub("ctl", "CTL", trt),
         site = gsub("rhodes", "Rhodes", site),
         site = gsub("spirit", "Spirit Lake", site),
         site = gsub("worle", "Worle", site),
         site = gsub("white", "Whiterock", site),
         site = gsub("eia", "EIA", site),
         site = gsub("mcnay", "McNay", site),
         site = gsub("arm", "Arm.", site),
         site = gsub("White", "Whiterock", site),
         site = gsub("Whiterockrock", "Whiterock", site),
         site = gsub("guthrie", "Guthrie", site),
         position = paste(trt, position, sep="-"),
         sitepos = paste(site, position)) %>%
  separate(newdate, c("date", "year1"), " ") %>%
  filter(!is.na(site)) %>%
  select(-trt, -year1)
 

# reworking stuff above to be more streamlined ----------------------------

# setwd("~/prairiestrips/groundwater/data-raw/waterquality/")

rm(list=ls(all=TRUE))


library("dplyr")
library("tidyr")
library("ggplot2")


gw2016codes <- read.csv("2016/2016gwcodes.csv", header = T) %>%
  select(-Sample.Source) %>%
  filter(!is.na(ID.)) %>%
  rename(id = ID., position = Position, site = Site, date = Date)
  
gw2016 <- read.csv("./2016/2016gwno3results2.csv", skip = 21, header = T)[ , 1:3] %>%
  select(ID..., NOx.result..mg.N.L.) %>%
  rename(id = ID..., no3mgL = NOx.result..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2016a <- read.csv("./2016/2016gwno3results.csv", skip = 20, header = T)[ , 1:3] %>%
  select(ID..., Nitrate.nitrite..mg.N.L.) %>%
  rename(id = ID..., no3mgL = Nitrate.nitrite..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2017codes <- read.csv("./2017/STRIPS2017groundwatercodes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2017 <- read.csv("./2017/STRIPS2017groundwaterresults.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)
  #left_join(gw2017codes, by = c("id"))


gw2016 <- gw2016 %>%
  rbind(gw2016a) %>%
  full_join(gw2016codes)

gw2017 <- gw2017 %>%
  full_join(gw2017codes)

all <- gw2016 %>%
  full_join(gw2017)







#creating a funciton to correct all formatting, type o's, etc. in data
correction <- function(data, column) { 
  correct <- read.csv("./corrections.csv", header = T)
  x
  
  data %>%
  mutate(column = gsub(correct$bad, correct$good, column))
} 




rbind(gw2016, gw2017) %>%
  left_join(gw2016codes, by = c("id")) %>%
  left_join(gw2017codes, by = c("id"))


gw2016 <- rbind(gw2016, gw2016a) %>%
  left_join(gw2016codes) %>%
  separate(date, c("month", "year"), " ") %>%
  mutate(
    month = gsub("Sept.", "September", month),
    month = gsub("Dec.", "December", month),
    month = gsub("Aug.", "August", month),
    month = gsub("Oct.", "October", month),
    month = gsub("Nov.", "November", month)
  )

gw2016$year <- gw2016$year %>%  replace_na("2016")



















all <- rbind(gw2016codes, gw2017codes) %>%
  filter(site != "NA") %>%
  arrange(year, month) %>%
  mutate(position = gsub("-", "", position),
         site = gsub("Arm.", "Armstrong", site),
         date = gsub("August", "Aug.", date),
         date = gsub("September", "Sept.", date),
         date = gsub("October", "Oct.", date),
         date = gsub("November", "Nov.", date),
         date = gsub("December", "Dec.", date),
         trt = position,
         trt = gsub("CTLbot", "CTL", trt),
         trt = gsub("CTLtop", "CTL", trt),
         trt = gsub("TRTtop", "TRT", trt),
         trt = gsub("TRTbot", "TRT", trt),
         trt = gsub("Bottom", "TRT", trt),
         trt = gsub("Top", "TRT", trt),
         trt = gsub("TRTBot", "TRT", trt),
         trt = gsub("TRTTRT", "TRT", trt),
         trt = factor(trt),
         pos = position,
         pos = gsub("CTLbot", "Bottom", pos),
         pos = gsub("TRTbot", "Bottom", pos),
         pos = gsub("CTLtop", "Top", pos),
         pos = gsub("TRTtop", "Top", pos),
         pos = gsub("TRTBot", "Bottom", pos),
         pos = factor(pos),
        position = factor(position, 
                          levels = c("CTLbot","TRTbot","TRTtop", "CTLtop", "Top", "Bottom"))
  )

all$order <- factor(all$date, levels = c("Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red")
linescale <- c(Top = "dashed",
               Bottom = "solid")

no3data <- all %>%
  filter(no3mgL!="NA" & year != "2015")

no3 <- ggplot(no3data, aes(x = order, 
                   y = no3mgL, 
                   group = position,
                  linetype = pos,
                 color = trt)) +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Nitrate - Nitrogen (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

setwd("~/prairiestrips")

ggsave(filename = "./graphs/gwno3.jpg", plot=no3, width = 6, height=8)

drpdata <- filter(all, year=="2017", drpmgL!="NA")
drpdata$drpmgL <- as.numeric(drpdata$drpmgL)

drp <- ggplot(drpdata, aes(x = order, 
                   y = drpmgL, 
                   group = position,
                   linetype = pos,
                   color = trt)) +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Dissolved Phosphorus (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "./graphs/gwdrp.jpg", plot=drp, width = 6, height=8)

########################################################################################
setwd("~/prairiestrips/")

gwdepth2016 <- read.csv("./groundwater/data-raw/depth/2016/2016strips2gwdepth.csv", header = T) %>%
  filter(!is.na(site)) %>%
  rename(rawdepthft = uncorrected.depth..ft.) %>%
  mutate(site = gsub("Arm", "Armstrong", site),
         site = gsub("arm", "Armstrong", site),
         site = gsub("white", "Whiterock", site),
         site = gsub("white ", "Whiterock", site),
         site = gsub("spirit", "Spirit Lake", site),
         site = gsub("eia", "EIA", site),
         site = gsub("guthrie", "Guthrie", site),
         site = gsub("mcnay", "McNay", site),
         site = gsub("rhodes", "Rhodes", site),
         site = gsub("worle", "Worle", site),
         pos = gsub("top", "Top", pos),
         pos = gsub("bot", "Bottom", pos),
         trt = gsub("trt", "TRT", trt),
         trt = gsub("ctl", "CTL", trt),
         position = paste(trt, pos),
         sitepos = paste(site, position)
          )

gwdepth2017 <- read.csv("C:/Users/Chris/Documents/prairiestrips/groundwater/data-raw/depth/2017/2017strips2gwdepth.csv", header = T) %>%
  filter(!is.na(site)) %>%
  rename(rawdepthft = uncorrected.depth..ft.) %>%
  mutate(site = gsub("Arm", "Armstrong", site),
         site = gsub("arm", "Armstrong", site),
         site = gsub("white", "Whiterock", site),
         site = gsub("white ", "Whiterock", site),
         site = gsub("spirit", "Spirit Lake", site),
         site = gsub("eia", "EIA", site),
         site = gsub("guthrie", "Guthrie", site),
         site = gsub("mcnay", "McNay", site),
         site = gsub("rhodes", "Rhodes", site),
         site = gsub("worle", "Worle", site),
         pos = gsub("top", "Top", pos),
         pos = gsub("bot", "Bottom", pos),
         trt = gsub("trt", "TRT", trt),
         trt = gsub("ctl", "CTL", trt),
         position = paste(trt, pos),
         sitepos = paste(site, position)
         )

gwdepth <- rbind(gwdepth2016, gwdepth2017)
gwdepth$abovegroundin = 1.5
gwdepth$abovegroundin[gwdepth$sitepos=="Whiterock CTL Bottom"] <- 2
gwdepth$abovegroundin[gwdepth$sitepos=="Spirit Lake TRT Top"] <- 1
gwdepth$abovegroundin[gwdepth$sitepos=="Spirit Lake CTL Top"] <- 13.8 
gwdepth$abovegroundin[gwdepth$sitepos=="Spirit Lake CTL Bottom"] <- 0
gwdepth$abovegroundin[gwdepth$sitepos=="Armstrong TRT Top"] <- 2.5
gwdepth$abovegroundin[gwdepth$sitepos=="Armstrong TRT Bottom"] <- 4
gwdepth$abovegroundin[gwdepth$sitepos=="Armstrong CTL Bottom"] <- 0
gwdepth$abovegroundin[gwdepth$sitepos=="Worle TRT Top"] <- 0
gwdepth$abovegroundin[gwdepth$sitepos=="Worle TRT Bottom"] <- 2
gwdepth$abovegroundin[gwdepth$sitepos=="McNay CTL Bottom"] <- 1
gwdepth$abovegroundin[gwdepth$sitepos=="EIA TRT Top"] <- 1
gwdepth$abovegroundin[gwdepth$sitepos=="EIA TRT Bottom"] <- 0.5
gwdepth$abovegroundin[gwdepth$sitepos=="Rhodes TRT Top"] <- 5
gwdepth$abovegroundin[gwdepth$sitepos=="Rhodes TRT Bottom"] <- 0
gwdepth$abovegroundin[gwdepth$sitepos=="Rhodes CTL Bottom"] <- 2
gwdepth$abovegroundin[gwdepth$sitepos=="Guthrie TRT Top"] <- 0
gwdepth$abovegroundin[gwdepth$sitepos=="Guthrie TRT Bottom"] <- 2

gwdepth <- gwdepth %>%
  mutate(adjdepthft = as.numeric(as.character(rawdepthft))-(abovegroundin/12))
  
gwdepth$adjdepthft <- as.numeric(format(round(gwdepth$adjdepthft, 2), nsmall = 2))

gwdepth$negadjdepthft <- -(gwdepth$adjdepthft)

gwdepth$date<- "Feb."
gwdepth$date[gwdepth$month=="3"] <- "March"
gwdepth$date[gwdepth$month=="4"] <- "April"
gwdepth$date[gwdepth$month=="5"] <- "May"
gwdepth$date[gwdepth$month=="6"] <- "June"
gwdepth$date[gwdepth$month=="7"] <- "July"
gwdepth$date[gwdepth$month=="8"] <- "Aug."
gwdepth$date[gwdepth$month=="9"] <- "Sept."
gwdepth$date[gwdepth$month=="10"] <- "Oct."
gwdepth$date[gwdepth$month=="11"] <- "Nov."
gwdepth$date[gwdepth$month=="12"] <- "Dec."


gwdepth$order <- factor(gwdepth$date, levels = c("Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))


# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red")
linescale <- c(Top = "dashed",
               Bottom = "solid")

gwdepthplot <- ggplot(gwdepth, aes(x = order, 
                           y = negadjdepthft, 
                           group = position,
                           linetype = pos,
                           color = trt)) +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "gwdepth.jpg", plot=gwdepthplot, width = 6, height=8)

test <- filter(gwdepth, site=="Whiterock")












######################################################################
#THIS WAS FOR LISA TO GIVE TO EIA, also will edit to give to Hoien for spirit lake, also will do for Guthrie

setwd("./graphs/")

white <- all %>%
  filter(site=="Whiterock")

whiteno3plot <- ggplot(data = subset(white, !is.na(no3mgL)), aes(x = order, 
                          y = no3mgL, 
                          group = position,
                          linetype = pos,
                          color = trt)) +
  ggtitle("Groundwater Nitrate - Nitrogen") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Nitrate - Nitrogen (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "gwwhiteno3.jpg", plot=whiteno3plot, width = 6, height=8)


whitedrp <- drpdata %>%
  filter(site=="Whiterock")

whitedrpplot <- ggplot(whitedrp, aes(x = order, 
                           y = drpmgL, 
                           group = position,
                           linetype = pos,
                           color = trt)) +
  ggtitle("Groundwater Dissolved Reactive Phosphorus") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Dissolved Reactive Phosphorus (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "gwwhitedrp.jpg", plot=whitedrpplot, width = 6, height=8)


whitegwdepth <- gwdepth %>%
  filter(site=="Whiterock")

whitegwdepthplot <- ggplot(whitegwdepth, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = position,
                                   linetype = pos,
                                   color = trt)) +
  ggtitle("Groundwater Depth from Ground Surface") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "whitegwdepth.jpg", plot=whitegwdepthplot, width = 6, height=8)





##########################################################################
