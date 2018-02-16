# setwd("C:/Users/Chris/Box Sync/Documents/Data/soil moisture/raw data/2013")

rm(list=ls(all=T))

library("tidyverse")

pr2files <- list.files("./", pattern = "pr*") #THIS CREATES A LIST OF FILES I WANT TO IMPORT

importing <- function(t) {
                          name <- t
                          file <- readr::read_csv(t, skip = 38, col_names = F)
                          file$name <- name
                          file
}

data <- plyr::ldply(pr2files, importing) %>%          #THIS RUNS LDPLY ON THE LIST OF FILES USING MY IMPORTING FUNCTION

select(name, X1, X2, X5, X7, X9, X11, X13, X15) %>%
rename(datetime = X1, measnum = X2, mv100 = X5, mv200 = X7, mv300 = X9, mv400 = X11, mv600 = X13, mv1000 = X15)