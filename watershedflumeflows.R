
rm(list=ls(all=TRUE))


library(tidyverse)

flume2.5lps <- 0.042446953-0.90725263*0.035^0.4+108.676075*0.035^1.4+937.5943603*0.035^2.5

flume2.0lps<- 0.022285358-0.55496382*0.035^0.5+125.5275778*0.035^1.5+939.5717311*0.035^2.5

flume2.5gpm <- flume2.5lps*15.850323
flume2.0gpm <- flume2.0lps*15.850323

watershedsizes <- read.csv("C:/Users/Chris/Documents/prairiestrips/sitenamesandwatershedsizes.csv")
watershedpeakflow <- read.csv("C:/Users/Chris/Documents/prairiestrips/watershedflumesizeandpeakflow.csv")

watersheds <- watershedsizes %>% left_join(watershedpeakflow) %>%
  mutate(flowat035m = flume2.0lps)

watersheds$flowat035m[watersheds$flumesize==2.5] <- flume2.5lps

watersheds <- watersheds %>%
  select(-c(4, 6, 7)) %>% 
  mutate(sqmm = acres*4046856422) %>%
  mutate(litersperwatershedonemmdeep = sqmm/1000000) %>%
  mutate(secondsforonemmofwaterat035m=litersperwatershedonemmdeep/flowat035m,
         minutesforonemmofwaterat035m=litersperwatershedonemmdeep/flowat035m/60)

write.csv(watersheds, file = "C:/Users/Chris/Documents/prairiestrips/watershedtimetoflow1mm.csv")

