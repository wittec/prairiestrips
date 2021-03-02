# setwd("data-raw/")

library("dplyr")
library("lubridate")

#IMPORTING RUNOFF DATA
my_runoff_csv = function(f, into) {
  readr::read_csv(f, 
                  skip = 6, 
                  col_types = "cnnic") %>%
    mutate(file = f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_runoff_csv, into = into)
}

runoff <- read_dir(path="runoff",
                   pattern = "*.csv",
                   into = c("runoff", "year", "watershed","csv")) %>%
  
  mutate(year = as.numeric(year),
                flow = as.numeric(flow),
                date_time = lubridate::parse_date_time(date_time,
                                                       orders = c("mdY HMS",
                                                                  "mdY IMSp",
                                                                  "mdY HM")),
         watershed = factor(watershed),
         sampleID  = factor(sampleID)) %>%
  
  select(watershed, date_time, level, flow, sampleID) %>%
  arrange(watershed, date_time) %>%

# ALL DATA BEFORE 2020 FOR SPL TRT WAS ONLY MEASURING 1 CULVERT INSTEAD OF 2, SO DOUBLING THE VOLUMES BELOW
  mutate(year = year(date_time)) %>%
  mutate(flow = ifelse(watershed == "spirittrt" & year < 2020, flow*2, flow)) %>%
  select(-year)

