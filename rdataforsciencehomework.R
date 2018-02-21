#setwd()
rm(list=ls(all=T))

library(tidyverse)

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
   select(-X8, -datemeas) %>%
   left_join(goodposnames) %>%
   left_join(goodsitenames) %>%
   left_join(goodtrtnames) 

 
#THIS IS FINDING UNIQUE COMBINATIONS OF DIFFERENT VARIBABLES!!!! GOOD STUFF!
correctionsites <- unique(data [c("goodsite", "goodtrt", "goodpos")]) %>%
  arrange(goodsite, goodtrt, goodpos) %>%

  
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
  left_join(correctionsites)
#############################################################################

#TRYING TO LEARN ABOUT LISTS AND PURR
sites <- as.list(unique(data$site))

trts <- as.list(unique(data$trt))

poss <- as.list(unique(data$pos))

all <- sites, trts, poss
