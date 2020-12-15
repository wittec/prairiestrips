d <- read.csv(file = "C:/......./clippedrainandflowdataallyears.csv")
sed2 <- read.csv(file = "C:/......./sed2.csv")


daysed <- sed2 %>% filter(analyte == "TSS (mg/L)") %>%
  mutate(date = date(date_time),
         sitename = paste(full,treatment,sep=" ")
  ) %>%
  group_by(sitename, date) %>%
  summarise("TSS lbs/ac" = sum(valueload))

#this dataset below is the daily summaries
dayrainflowsed <- d %>%
  mutate(date = date(date_time),
         sitename = paste(full, treatment, sep = " "),
         flowin = flow * 231 * 5 / (acres * 6.273e6)
  ) %>%
  filter(treatment != "rain") %>%
  group_by(sitename, date, treatment) %>%
  summarise("rainday (in)" = sum(rain*39.3701), "flowday (in)" = sum(flowin)) %>%
  select(-treatment) %>%
  left_join(daysed)