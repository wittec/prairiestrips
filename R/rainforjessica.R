
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)

myrain <- readRDS(file = "C:/Users/Chris/Documents/prairiestrips/raindataallyears.Rda")

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

newsite <- fixit(myrain$site)

myrain$site <- newsite$x
myrain <- myrain %>% arrange(site, date_time) %>% rename(rain_mm = rain)

rain2016 <- myrain %>% filter(year==2016)
write.csv(rain2016, file = "C:/Users/Chris/Documents/prairiestrips/2016rain.csv")

rain2017 <- myrain %>% filter(year==2017)
write.csv(rain2017, file = "C:/Users/Chris/Documents/prairiestrips/2017rain.csv")

rain2018 <- myrain %>% filter(year==2018)
write.csv(rain2018, file = "C:/Users/Chris/Documents/prairiestrips/2018rain.csv")

rain2019 <- myrain %>% filter(year==2019)
write.csv(rain2019, file = "C:/Users/Chris/Documents/prairiestrips/2019rain.csv")
