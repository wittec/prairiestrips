# setwd("data-raw/")

library("dplyr")

my_csv = function(f, into) {
  readr::read_csv(f, 
                  skip = 4) %>%
    mutate(file=f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  plyr::ldply(files, my_csv, into = into)
}


# Combining and cleaning
water_quality <- read_dir(path="water_quality",
                          pattern = "*.csv",
                          into = c("water", "quality", "year", "source","csv")) %>%
  
  select(
    -water, -quality, -year, -source, -csv,
    -starts_with("X"),                     # This are empty columns
    -`Date Sampled`,                       # The following columns aren't used
    -`Plot ID`,
    -`Bottle #`,
    -contains("date"),                     # These two columns probably should be used
    -contains("Date")) %>%
  
  rename(sampleID = `Sample ID #`) %>%
  
  tidyr::gather(analyte, result, -sampleID) %>%
  
  filter(!is.na(sampleID)) %>% # exclude extraneous rows
  filter(!is.na(result)) %>%   # exclude missing and those that have not been taken yet
  
  mutate(
    sampleID = as.character(sampleID),
    result = plyr::revalue(result,
                           replace = c("ND" = 0.003,
                                       ">60 ppm" = 60)),
    result = as.numeric(result)) %>%
  
  tidyr::spread(analyte, result)
