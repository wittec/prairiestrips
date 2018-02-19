#setwd()
rm(list=ls(all=T))

library(tidyverse)

data <- readr::read_csv("./groundwater/data-raw/depth/2017/2017strips2gwdepth.csv") %>%
  rename(datemeas = 'date measured',
  rawdepthft = 'uncorrected depth (ft)') %>%
  select(-X8, -datemeas) %>%
 
         
goodnames <- tribble( ~site, ~goodname,
                      #-----------------
                      "arm", "Armstrong",
                      "eia", "EIA",
                      "rhodes", "Rhodes",
                      "mcnay", "McNay",
                      "worle", "Worle",
                      "white", "Whiterock",
                      "guthrie", "Guthrie",
                      "spirit", "Spirit Lake",
                      )
