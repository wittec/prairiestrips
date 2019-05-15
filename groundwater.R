
# groundwater water quality data manip ------------------------------------

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

gw2018codes <- read.csv("./2018/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2018 <- read.csv("./2018/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

gw2016 <- gw2016 %>%
  rbind(gw2016a) %>%
  full_join(gw2016codes) %>%
  rename(month = date) %>%
  mutate(month = as.character(month))

gw2017 <- gw2017 %>%
  full_join(gw2017codes) %>%
  mutate(month = as.character(month))

gw2018 <- gw2018 %>%
  full_join(gw2018codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL))
        )

all <- gw2016 %>%
  full_join(gw2017) %>%
  full_join(gw2018) %>%
  filter(!is.na(id)) %>%
  filter(!is.na(site)) %>%
  rename(sampleID = id)

all$trt[all$position == "CTL" | all$position == "CTL-Bot" | all$position == "CTL-Top"] <- "CTL"
all$trt[is.na(all$trt)] <- "TRT"
all$position[all$position == "CTL"] <- "Bot"
is.na(all$sampleID) <- NULL
all$year[all$sampleID <5000] <- "2015"
all$year[all$sampleID >= 5000 & all$sampleID < 6000] <- "2016"
all$year[all$sampleID >= 6000 & all$sampleID < 7000] <- "2017"
all$year[all$sampleID >= 7000] <- "2018"

# water quality - correcting typo's, wrong codes, etc. ------------------------------------

fixit <- function (x) {
   correct <- read.csv("~/prairiestrips/groundwater/data-raw/corrections.csv", header = T) %>%
     mutate(bad = as.character(bad),
            good = as.character(good)
     )

   testdata <- as.data.frame(x)
  
  left_join(testdata, correct, by = c("x" = "bad"))  %>%
  mutate(x = ifelse(!is.na(good), good, x)) %>%
  select(-ends_with("good"))
  
}

corrected <- as.data.frame(lapply(all[c(3:5,8)], fixit)) %>%
  rename(site = x, position = x.1, month = x.2, trt = x.3)
  
all$site <- corrected$site
all$position <- corrected$position
all$month <- corrected$month
all$trt <- corrected$trt
all <- all %>%
  mutate(pos = paste(trt, position, sep = "-"))


# graphing water quality ----------------------------------------------------------------

all$order <- factor(all$month, levels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red",
                MCL = "green")
linescale <- c(Top = "dashed",
               Bot = "solid",
               MCL = "solid")

no3data <- all %>%
  filter(no3mgL!="NA" & year != "2015")

no3 <- ggplot(no3data, aes(x = order, 
                   y = no3mgL, 
                   group = pos,
                  linetype = position,
                 color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  #add in next line for max concentration limit for drinking water 
  #geom_hline(aes(color = "MCL", yintercept = 10)) +
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

drpdata <- filter(all, year>="2017", drpmgL!="NA")
drpdata$drpmgL <- as.numeric(drpdata$drpmgL)

drp <- ggplot(drpdata, aes(x = order, 
                   y = drpmgL, 
                   group = pos,
                   linetype = position,
                   color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
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


# gw depth data manip -------------------------------------------

#rm(list=ls(all=TRUE))

library(tidyverse)

fixit <- function (x) {
  correct <- read.csv("~/prairiestrips/groundwater/data-raw/corrections.csv", header = T) %>%
    mutate(bad = as.character(bad),
           good = as.character(good)
    )
  
  testdata <- as.data.frame(x)
  
  left_join(testdata, correct, by = c("x" = "bad"))  %>%
    mutate(x = ifelse(!is.na(good), good, x)) %>%
    select(-ends_with("good"))
  
}

setwd("~/prairiestrips/")

gwdepth2016 <- read.csv("./groundwater/data-raw/depth/2016/2016strips2gwdepth.csv", header = T)
  
gwdepth2017 <- read.csv("C:/Users/Chris/Documents/prairiestrips/groundwater/data-raw/depth/2017/2017strips2gwdepth.csv", header = T) 

gwdepth2018 <- read.csv("C:/Users/Chris/Documents/prairiestrips/groundwater/data-raw/depth/2018/2018strips2gwdepth.csv", header = T) 

gwdepth2019 <- read.csv("C:/Users/Chris/Documents/prairiestrips/groundwater/data-raw/depth/2019/2019strips2gwdepth.csv", header = T) 

gwdepth <- rbind(gwdepth2016, gwdepth2017, gwdepth2018, gwdepth2019) %>%
  mutate(month = as.character(month)) %>%
  select(-X)

#changing dry wells to depth of 15
gwdepth$uncorrected.depth..ft. <- as.character(gwdepth$uncorrected.depth..ft.)
gwdepth$uncorrected.depth..ft.[gwdepth$uncorrected.depth..ft.== "dry"] <- 15
gwdepth$uncorrected.depth..ft. <- as.numeric(gwdepth$uncorrected.depth..ft.)
  

correcteddepth <- as.data.frame(lapply(gwdepth[c(2:5)], fixit)) %>%
  rename(month = x, site = x.1, trt = x.2, pos = x.3)

gwdepth$month <- correcteddepth$month
gwdepth$site <- correcteddepth$site
gwdepth$trt <- correcteddepth$trt
gwdepth$pos <- correcteddepth$pos
gwdepth <- gwdepth %>%
  mutate(wellid = paste(site, trt, pos, sep = ""),
         uncorrected.depth..ft. = as.numeric(as.character(uncorrected.depth..ft.)))

depthadj <- read.csv("./groundwater/data-raw/depth/depthadjustment.csv", header = T) %>%
  select(wellid, abovegroundin)
gwdepth <- left_join(gwdepth, depthadj) %>%
  mutate(adjdepthft = round(uncorrected.depth..ft. - (abovegroundin/12), digits = 2))

gwdepth$negadjdepthft <- -(gwdepth$adjdepthft)

gwdepth$order <- factor(gwdepth$month, levels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

# gwdepth graph -----------------------------------------------------------


# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red")
linescale <- c(Top = "dashed",
               Bot = "solid")

gwdepthplot <- ggplot(gwdepth, aes(x = order, 
                           y = negadjdepthft, 
                           group = wellid,
                           linetype = pos,
                           color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
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

# site specific graphs ----------------------------------------------------

setwd("~/prairiestrips/graphs/")

arm <- all %>%
  filter(site=="ARM")

armno3 <- arm %>%
  filter(year == "2016" | year == "2017")

armno3plot <- ggplot(data = subset(armno3, !is.na(no3mgL)), aes(x = order, 
                          y = no3mgL, 
                          group = pos,
                          linetype = position,
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

armno3plot

ggsave(filename = "gwarmno3.jpg", plot=armno3plot, width = 6, height=6)

armdrp <- arm %>%
  filter(year ==2017)

armdrpplot <- ggplot(armdrp, aes(x = order, 
                           y = drpmgL, 
                           group = pos,
                           linetype = position,
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

ggsave(filename = "gwarmdrp.jpg", plot=armdrpplot, width = 6, height= 6)


armgwdepth <- gwdepth %>%
  filter(site=="ARM")

armgwdepthplot <- ggplot(armgwdepth, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = wellid,
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

ggsave(filename = "armgwdepth.jpg", plot=armgwdepthplot, width = 6, height=6)



# year specific gw depth graph --------------------------------------------

y2018 <- gwdepth %>%
  filter(year == "2018")


# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red")
linescale <- c(Top = "dashed",
               Bot = "solid")

yeargwdepthplot <- ggplot(y2018, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = wellid,
                                   linetype = pos,
                                   color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

yeargwdepthplot




# testing cowplot ---------------------------------------------------------


library(cowplot)
armno3drplot <- plot_grid(armno3plot, armdrpplot, labels = c("A", "B"))

armno3drplot
