
rm(list=ls(all=TRUE))

library(tidyverse)


load(file = "C:/Users/Chris/Documents/prairiestrips/data/clippedrainandflowdataallyears.rds")
load(file = "C:/Users/Chris/Documents/prairiestrips/data/sed3.rds")


# custom settings for graphs ----------------------------------------------

colorscale <- c(rain = "seagreen", 
                control = "red",
                treatment = "blue")
linescale <- c(rain = "dotted",
               control = "solid",
               treatment = "dashed")


# runoff only (no rain) graph, all years -------------------------------

colorscale1 <- c(control = "red",
                treatment = "blue")
linescale1 <- c(control = "solid",
               treatment = "dashed")


f <- ggplot(d %>% filter(treatment != "rain"), aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line(size = 1) + 
  facet_grid(codes~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative Runoff (inches)') + 
  scale_color_manual(values = colorscale1) +
  scale_linetype_manual(values = linescale1) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

f

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/runoff2019withoutrain.jpg", plot=f, width = 6, height=8)



# runoff and rain graph, all years -------------------------------

g <- ggplot(d, aes(x = date_time, 
                   y = y, 
                   group = watershed, 
                   linetype = treatment,
                   color = treatment)) +
  geom_line(size = 1) + 
  facet_grid(full~year, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative rainfall and runoff (inches)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

g

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/runoff2019.jpg", plot=g, width = 6, height=8)

# create table of final cumulative values for rain and flow in 2016---------------

endtable2016 <- d %>% filter(year=="2016") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e20161 <- endtable2016 %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e20162 <- endtable2016 %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e20161$rain <- e20162$rain
e20161 <- e20161 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e20161, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2016.csv")

# create table of final cumulative values for rain and flow in 2017---------------

endtable2017 <- d %>% filter(year=="2017") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e20171 <- endtable2017 %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e20172 <- endtable2017 %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e20171$rain <- e20172$rain
e20171 <- e20171 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e20171, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2017.csv")


# create table of final cumulative values for rain and flow in 2018---------------

endtable2018 <- d %>% filter(year=="2018") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e20181 <- endtable2018 %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e20182 <- endtable2018 %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e20181$rain <- e20182$rain
e20181 <- e20181 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e20181, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2018.csv")

# create table of final cumulative values for rain and flow in 2019---------------

endtable2019 <- d %>% filter(year=="2019") %>% group_by(full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e20191 <- endtable2019 %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e20192 <- endtable2019 %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e20191$rain <- e20192$rain
e20191 <- e20191 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full, Rain = rain, Control = control, Treatment = treatment)

write.csv(e20191, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2019.csv")

# TABLE OF CUMULATIVE RAIN AND FLOW VALUES FOR YEARS 2016 - PRESENT

allyearstable <- e20161 %>% rbind(e20171, e20181, e20191) %>% group_by(Site) %>% 
  summarise_at(c("Rain", "Control", "Treatment"), sum)
  
allyearstable1 <- left_join(allyearstable, d, by = c("Site" = "full")) %>%
  select(codes, Rain, Control, Treatment) %>%
  unique() %>%
  rename(Site = codes)

write.csv(allyearstable1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoffallyearscumulative.csv")


#MAKING TABLE WITH 3 LETTER CODES INSTEAD OF FULL NAMES OF SITES
endtable <- d %>% filter(year=="2019") %>% group_by(codes, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = codes, Rain = rain, Control = control, Treatment = treatment)

write.csv(e1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/rainrunoff2019sitecodes.csv")



# nitrate graph all years-----------------------------------------------------------

no3graph <- ggplot(sed3 %>% 
         filter(analyte == "Nitrate + nitrite (mg N/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line(size = 1) + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Nitrate - Nitrogen (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/no32019.jpg", plot=no3graph, width = 6, height=8)


# orthophosphate graph all years----------------------------------------------------

orthopgraph <- ggplot(sed3 %>% 
         filter(analyte == "Orthophosphate (mg P/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line(size = 1) +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Dissolved Phosphorus (lbs/ac)') + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/orthop2019.jpg", plot=orthopgraph, width = 6, height=8)


# tss graph all years---------------------------------------------------------------

tssgraph <- ggplot(sed3 %>% 
         filter(analyte == "TSS (mg/L)"), 
       aes(x = date_time, 
           y = cumulative,
           group = treatment,
           color = treatment,
           linetype = treatment)) + 
  geom_line(size = 1) +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(codes ~ year, scales = 'free_x') + 
  labs(x = '',  
       y = 'Runoff Total Suspended Solids (lbs/ac)') + 
  theme_bw() + 
  theme(legend.position = "bottom",
      legend.title    = element_blank(),
      axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/tss2019.jpg", plot=tssgraph, width = 6, height=8)

# create table of final cumulative values for nutrients in 2016 ---------------
load(file = "C:/Users/Chris/Documents/prairiestrips/data/sed2.rds")

nuttable2016 <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2016") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12016 <- nuttable2016 %>% spread(analyte, cumulative)
no3table2016 <- n12016 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2016 <- n12016 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2016 <- n12016 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2016 <- no3table2016 %>%
  left_join(orthotable2016, by = "full") %>%
  left_join(tsstable2016, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable2016, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2016.csv")


# create table of final cumulative values for nutrients in 2017 ---------------

nuttable2017 <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2017") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12017 <- nuttable2017 %>% spread(analyte, cumulative)
no3table2017 <- n12017 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2017 <- n12017 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2017 <- n12017 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2017 <- no3table2017 %>%
  left_join(orthotable2017, by = "full") %>%
  left_join(tsstable2017, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable2017, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2017.csv")

# create table of final cumulative values for nutrients in 2018 ---------------

nuttable2018 <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2018") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12018 <- nuttable2018 %>% spread(analyte, cumulative)
no3table2018 <- n12018 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2018 <- n12018 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2018 <- n12018 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2018 <- no3table2018 %>%
  left_join(orthotable2018, by = "full") %>%
  left_join(tsstable2018, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

#MAKING 2018 TABLE WITH THE SITE CODES INSTEAD OF FULL NAMES

nuttable2018 <- sed2 %>% group_by(codes, treatment, analyte) %>% filter(year=="2018") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12018 <- nuttable2018 %>% spread(analyte, cumulative)
no3table2018 <- n12018 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2018 <- n12018 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2018 <- n12018 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2018codes <- no3table2018 %>%
  left_join(orthotable2018, by = "codes") %>%
  left_join(tsstable2018, by = "codes") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = codes, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable2018codes, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2018sitecodes.csv")


# create table of final cumulative values for nutrients in 2019 ---------------

nuttable2019 <- sed2 %>% group_by(full, treatment, analyte) %>% filter(year=="2019") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12019 <- nuttable2019 %>% spread(analyte, cumulative)
no3table2019 <- n12019 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2019 <- n12019 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2019 <- n12019 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2019 <- no3table2019 %>%
  left_join(orthotable2019, by = "full") %>%
  left_join(tsstable2019, by = "full") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = full, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)


write.csv(allnuttable2019, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2019.csv")


#MAKING 2019 TABLE WITH THE SITE CODES INSTEAD OF FULL NAMES

nuttable2019 <- sed2 %>% group_by(codes, treatment, analyte) %>% filter(year=="2019") %>% summarize_at(c("cumulative"), max, na.rm = T)
n12019 <- nuttable2019 %>% spread(analyte, cumulative)
no3table2019 <- n12019 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable2019 <- n12019 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable2019 <- n12019 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable2019codes <- no3table2019 %>%
  left_join(orthotable2019, by = "codes") %>%
  left_join(tsstable2019, by = "codes") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(site = codes, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment)

write.csv(allnuttable2019codes, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrients2019sitecodes.csv")

# MAKING CUMULATIVE NUTRIENT TABLE WITH SITE CODES FOR YEARS 2016-PRESENT

allyearsnuttable <- allnuttable2016 %>% rbind(allnuttable2017, allnuttable2018, allnuttable2019) %>% group_by(site) %>% 
  summarise_at(c("no3ctl", "no3trt", "orthoctl", "orthotrt", "tssctl", "tsstrt"), sum, na.rm = T) %>%
  mutate_at(vars(tssctl, tsstrt), funs(round(., 0)))

allyearsnuttable1 <- left_join(allyearsnuttable, d, by = c("site" = "full")) %>%
  select(codes, no3ctl, no3trt, orthoctl, orthotrt, tssctl, tsstrt) %>%
  unique() %>%
  rename(Site = codes)

#write.csv(allyearsnuttable1, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/nutrientsallyearscumulative.csv")




# site specific runoff graph --------

siterainrunplot <- ggplot(d %>% filter(site=="arm" & treatment != "rain"), aes(x = date_time,
                   y = y,
                   group = watershed,
                   linetype = treatment,
                   color = treatment)) +
  ggtitle("Surface Runoff") +
  geom_line(size = 1) +
  facet_grid(full~year, scales='free_x') +
  labs(x = '',
       y = 'Cumulative runoff (inches)') +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/runoff/armrunoff2019.jpg", plot=siterainrunplot, width = 6, height=8)


# site specific orthop graph --------

siteorthopgraph <- ggplot(sed3 %>%
                        filter(site=="arm", analyte == "Orthophosphate (mg P/L)"),
                      aes(x = date_time,
                          y = cumulative,
                          group = treatment,
                          color = treatment,
                          linetype = treatment)) +
  ggtitle("Surface Runoff Orthophosphate") +
  geom_line() +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') +
  labs(x = '',
       y = 'Cumulative Dissolved Nitrogen (lbs/ac)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/runoff/armorthop2019.jpg", plot=siteorthopgraph, width = 6, height=8)


# site specific tss graph --------


sitetssgraph <- ggplot(sed3 %>%
                     filter(site=="arm", analyte == "TSS (mg/L)"),
                   aes(x = date_time,
                       y = cumulative,
                       group = treatment,
                       color = treatment,
                       linetype = treatment)) +
  ggtitle("Surface Runoff Sediment") +
  geom_line(size = 1) +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') +
  labs(x = '',
       y = 'Cumulative Total Suspended Solids (lbs/ac)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/runoff/armtss2019.jpg", plot=sitetssgraph, width = 6, height=8)


# site specific no3 graph --------

siteno3graph <- ggplot(sed3 %>%
                          filter(site=="arm", analyte == "Nitrate + nitrite (mg N/L)"),
                        aes(x = date_time,
                            y = cumulative,
                            group = treatment,
                            color = treatment,
                            linetype = treatment)) +
  ggtitle("Surface Runoff Nitrate - Nitrogen") +
  geom_line(size = 1) +
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  facet_grid(full ~ year, scales = 'free_x') +
  labs(x = '',
       y = 'Cumulative Runoff Nitrate - Nitrogen (lbs/ac)') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/runoff/armno32019.jpg", plot=siteno3graph, width = 6, height=8)


# site specific table of everything by year --------

endtable <- d %>% filter(full == "Armstrong") %>% group_by(year, full, treatment) %>% summarize_at(c("cumulative_rain", "cumulative_flow"), max, na.rm = T)
e1 <- endtable %>% select(-cumulative_rain) %>% spread(treatment, cumulative_flow)
e2 <- endtable %>% select(-cumulative_flow) %>% spread(treatment, cumulative_rain)
e1$rain <- e2$rain
e1 <- e1 %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate(rain = round(rain, 1)) %>%
  rename(Year = year, Site = full, Rain = rain, Control = control, Treatment = treatment)


#sed2 <- load(file = "C:/Users/Chris/Documents/prairiestrips/data/sed2.rds")

nuttable <- sed2 %>% filter(full == "Armstrong") %>% group_by(year, full, treatment, analyte) %>% summarize_at(c("cumulative"), max, na.rm = T)
n1<- nuttable %>% spread(analyte, cumulative)
no3table <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Nitrate + nitrite (mg N/L)`)
orthotable <- n1 %>% select(-`Nitrate + nitrite (mg N/L)`, -`TSS (mg/L)`) %>% spread(treatment, `Orthophosphate (mg P/L)`)
tsstable <- n1 %>% select(-`Orthophosphate (mg P/L)`, -`Nitrate + nitrite (mg N/L)`) %>% spread(treatment, `TSS (mg/L)`)

allnuttable <- no3table %>%
  left_join(orthotable, by = "year") %>%
  left_join(tsstable, by = "year") %>%
  mutate_if(is.numeric, round, 2) %>%
  rename(Site = full.x, no3ctl = control.x, no3trt = treatment.x, orthoctl = control.y, 
         orthotrt = treatment.y, tssctl = control, tsstrt = treatment) %>%
  select(-full, -full.y)

completetable <- e1%>% cbind(allnuttable) %>%
  ungroup() %>%
  select(-year, -Site, -Site1)

write.csv(completetable, row.names = F, file = "C:/Users/Chris/Documents/prairiestrips/tables/sitecompletetable.csv")
#this is finished table in graphsandtables****.Rmd

