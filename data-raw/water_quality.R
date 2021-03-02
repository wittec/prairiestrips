# setwd("data-raw/")

source("water_quality-read.R")

# Save data as R object in package
usethis::use_data(water_quality, overwrite = TRUE)
