#setwd()
rm(list=ls(all=T))

library(tidyverse)
library(lubridate)

##################################################
#THIS IS MAKING TIBBLES TO CORRECT/RENAME SITE,TRT,POS
goodsitenames <- tribble( ~site, ~goodsite,
                      #-----------------
                      "arm", "Armstrong",
                      "eia", "EIA",
                      "rhodes", "Rhodes",
                      "mcnay", "McNay",
                      "worle", "Worle",
                      "white", "Whiterock",
                      "guthrie", "Guthrie",
                      "spirit", "Spirit Lake"
                      )
                      
 goodtrtnames <- tribble(~trt, ~goodtrt,
                        #---------------
                        "trt", "TRT",
                        "ctl", "CTL",
                        "TRT", "TRT",
                        "CTL", "CTL")

 goodposnames <- tribble(~pos, ~goodpos,
                         #--------------
                         "top", "Top",
                         "bot", "Bot")
##############################################
 
#IMPORTING DATA AND MERGING GOOD NAMES
 data <- readr::read_csv("./groundwater/data-raw/depth/2017/2017strips2gwdepth.csv") %>%
   rename(datemeas = 'date measured',
          rawdepthft = 'uncorrected depth (ft)') %>%
   select(-X8) %>%
   #mutate(newmeas = as_date(datemeas, format="%m/%d/%Y")) %>%
   left_join(goodposnames) %>%
   left_join(goodsitenames) %>%
   left_join(goodtrtnames) 
 
data$datemeas <- as.POSIXct(data$datemeas, format = "%m/%d/%Y")
 
#THIS IS FINDING UNIQUE COMBINATIONS OF DIFFERENT VARIBABLES!!!! GOOD STUFF!
correctionsites <- unique(data [c("goodsite", "goodtrt", "goodpos")]) %>%
  arrange(goodsite, goodtrt, goodpos)

  
#MAKING A TIBBLE OF CORRECTION DEPTHS...THEY ARE LISTED IN ORDER TO MERGE WITH THE CORRECTION SITES 
depthcorrectionin <- tribble(~correctionin,
                            #------------
                            0,
                            4,
                            2.5,
                            1.5,
                            0.5,
                            1,
                            2,
                            0,
                            1,
                            1.5,
                            2,
                            0,
                            5,
                            0,
                            13.8,
                            1.5,
                            1,
                            2,
                            1.5,
                            1.5,
                            1.5,
                            2,
                            0
                            )

correctionsites <- cbind(correctionsites, depthcorrectionin)

data <- data %>%
  left_join(correctionsites) %>%
  mutate(stp = paste(goodsite, goodtrt, goodpos),
         goodmonth = month(datemeas, label = T),     #THIS USES THE MONTH COMMAND FROM LUBRIDATE
         rawdepthft = as.numeric(rawdepthft),
         correctdepthft = rawdepthft - (correctionin/12)
         )    

#############################################################################

#TRYING TO LEARN ABOUT LISTS AND PURR
sites <- as.list(unique(data$site))

trts <- as.list(unique(data$trt))

poss <- as.list(unique(data$pos))

all <- sites, trts, poss

##########################################################################
#CHAPTER 3 IN RFORDATASCIENCE
#  REALLY GOOD GRAPHING CHAPTER!!!!
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color=class), size=2) +
  facet_wrap(~manufacturer, nrow=4)

#PLAYING WITH GW DATA
gw <- ggplot(data, aes(goodmonth, correctdepthft, na.rm=T, group=stp))  #TELLS WHAT TO PLOT

gw + geom_point(aes(color=goodtrt, shape = goodpos)) +   #MAKES IT A SCATTERPLOT
  facet_wrap(~goodsite, nrow=4) +   #BREAKS UP GRAPH INTO FACETS BY SITE
  geom_line(aes(color=goodtrt)) +   #DRAWS LINES BETWEEN EACH OF THE POINTS, GROUPED BY THE GGPLOT AES GROUP=STP
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +   #ROTATES AND ADJUSTS THE X AXIS TEXT
  scale_y_continuous(breaks=seq(0,15,3)) +      #SUPPOSED TO SET Y AXIS LIMITS 0-15 (THIS WORKS) AND MAKE TICKS EVERY 3 (BUT DOESN'T)
  scale_y_reverse() +     #FLIPS THE Y AXIS
  theme(legend.title = element_blank()) +     #REMOVES THE LEGEND TITLES
  theme(axis.title.x = element_blank()) +        #REMOVES X AXIS TITLE
  labs(y="Depth from Ground Surface (ft)")      #RELABELS Y AXIS