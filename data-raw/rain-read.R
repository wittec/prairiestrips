# setwd("~/STRIPS2Helmers/data-raw")

library("dplyr")

my_read_csv = function(f, into) {
  readr::read_csv(f,
                  skip=6,
                  col_types = c("cn")) %>%
    mutate(file=f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_read_csv, into = into)
}



rain <- read_dir(path="rain",
                 pattern = "*.csv",
                 into = c("rain", "year", "watershed","csv")) %>%
  
  mutate(year = as.numeric(year),
         watershed = gsub("rain", "", watershed),
         watershed = factor(watershed),
         `rain-m` = as.numeric(`rain-m`),
         date_time = lubridate::parse_date_time(date_time,
                                                orders = c("mdY HMS",
                                                           "mdY IMSp",
                                                           "mdY HM"))) %>%
  
  select(watershed, date_time, `rain-m`) %>%
  arrange(watershed, date_time) %>%
  
  # Remove bad rain data
  filter(
    
    # rain gauge was plugged into the wrong port
    !(watershed == "spiritctl" &
        date_time > lubridate::ymd("2016-01-01") &
        date_time < lubridate::ymd("2016-09-24")))

#THIS IS SCRIPT THAT REPLACES BAD ISCO RAIN DATA
source("~/STRIPS2Helmers/rain-fixes/rainfix.R")
