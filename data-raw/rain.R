# setwd("data-raw/")

source("rain-read.R")

#Save clean data to project
#devtools::use_data(rain, overwrite = TRUE)
usethis::use_data(rain, overwrite = TRUE)
