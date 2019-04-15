
rm(list=ls(all=TRUE))


library(tidyverse)
library(lubridate)

d <- readRDS(file = "~/prairiestrips/clippedrainandflowdataallyears.Rda")

dflow <- d %>% filter(flow != 0) %>%
  group_by(site, year) %>%
  mutate(event = (c(0,cumsum(diff(date_time) > 240))),
         date = date(date_time)) #this is making a new event 
                                                      #when > 240 minute gap in date_time 

eventlist <- split(dflow, list(dflow$site, dflow$year, dflow$event))

#need to graph each event
#also should maybe flag events where level is over .035?

# custom settings for graphs ----------------------------------------------

colorscale <- c(rain = "blue", 
                control = "black",
                treatment = "seagreen")
linescale <- c(rain = "dotted",
               control = "solid",
               treatment = "dashed")


graphit <- function(data)
  { g <- ggplot(data, 
              aes(x = date_time, 
                  y = level, 
                  group = watershed, 
                  linetype = treatment,
                  color = treatment)) +
                  geom_line(size = 1)
    
  
  return(g)

}

map(eventlist, graphit)





# this does what I want, just need to expand to do all sites and y --------


test <- d %>% 
  filter(site=="arm", year == 2017, flow != 0) %>%
  select(date_time, site, watershed, year, level) %>% 
  select(-site) %>%
  spread(watershed, level) %>%
  mutate(event = (c(0, cumsum(diff(date_time) > 240))))

testlist <- split(test, test$event)


testgraphit <- function(data)
{ g <- ggplot(data, 
              aes(x = date_time, 
                  y = armctl,
                  color = "armctl")) +
                  #group = armctl)) +#, 
                  #linetype = treatment,
                   #color = treatment)) + 
  geom_line(size = 1) +
    geom_line(data = data, aes(y = armtrt,
    #                          #group = watershed.y, 
    #                          #linetype = treatment.y,
                              color = "armtrt"))


return(g)

}

map(testlist, testgraphit)

# trying to expand the test example ---------------------------------------
# THIS IS CREATING TOO MANY EVENTS!
gen <- d %>%
  filter(level != "NA") %>%
  filter(level != 0) %>%
  select(date_time, site, watershed, year, level) %>% 
  group_by(site, year) %>%
  
  spread(watershed, level) %>%
  ungroup %>%
  group_by(site, year) %>%
  mutate(event = (c(0, cumsum(diff(date_time) > 240))))

gen1 <- gen %>% 
  gather(watershed, level, armctl:worletrt) %>% 
  filter(level != "NA") %>%
  group_by(site, year, event) %>%
  mutate(maxlevel = max(level)) %>%
  filter(maxlevel > 0.035) #%>%
  #filter(nrow(level) > 6)

#####THERES NOTHING IN THE LIST EXCEPT FOR IN ARM
gen1list <- split(gen1, list(gen1$site, gen1$year, gen1$event)) 

gen1list <- gen1list[sapply(gen1list, function(x) dim(x)[1]) > 0]

gen1graphs <- function(data)
  {
    v <- ggplot(data, 
            aes(x = date_time, 
                y = level,
                group = watershed,
                color = watershed)) +
  #group = armctl)) +#, 
  #linetype = treatment,
  #color = treatment)) + 
        geom_line(size = 1) +
        scale_x_datetime(date_labels= "%m/%d/%Y %H:%M:%S") 
  #facet_grid(site~year, scales='free_x')


return(v)
}

map(gen1list, gen1graphs)
