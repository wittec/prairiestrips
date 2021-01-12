
# groundwater water quality data manip ------------------------------------

rm(list=ls(all=TRUE))


library("tidyverse")

# function to clean up names and/or replace typeO's -------------------------

fixit <- function (x) {
  correct <- read.csv("~/prairiestrips/data-raw/groundwater/corrections.csv", header = T) %>%
    mutate(bad = as.character(bad),
           good = as.character(good)
    )
  
  testdata <- as.data.frame(x)
  
  left_join(testdata, correct, by = c("x" = "bad"))  %>%
    mutate(x = ifelse(!is.na(good), good, x)) %>%
    select(-ends_with("good"))
  
}


#importing water quality files -----------------------------------------
#
gw2016codes <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2016/2016gwcodes.csv", header = T) %>%
  select(-Sample.Source) %>%
  filter(!is.na(ID.)) %>%
  rename(id = ID., position = Position, site = Site, date = Date)

gw2016 <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2016/2016gwno3results2.csv", skip = 21, header = T)[ , 1:3] %>%
  select(ID..., NOx.result..mg.N.L.) %>%
  rename(id = ID..., no3mgL = NOx.result..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2016a <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2016/2016gwno3results.csv", skip = 20, header = T)[ , 1:3] %>%
  select(ID..., Nitrate.nitrite..mg.N.L.) %>%
  rename(id = ID..., no3mgL = Nitrate.nitrite..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2017codes <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2017/STRIPS2017groundwatercodes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2017 <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2017/STRIPS2017groundwaterresults.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)
#left_join(gw2017codes, by = c("id"))

gw2018codes <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2018/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2018 <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2018/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

gw2019codes <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2019/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") %>%
  mutate(id = as.numeric(as.character(id)))

gw2019 <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2019/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

gw2020codes <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2020/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") %>%
  mutate(id = as.numeric(as.character(id)))

gw2020 <- read.csv("~/prairiestrips/data-raw/groundwater/waterquality/2020/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)


# joining the codes and results files ------------------------------------------------
gw2016 <- gw2016 %>%
  rbind(gw2016a) %>%
  full_join(gw2016codes) %>%
  rename(month = date) %>%
  mutate(month = as.character(month),
         year = "2016")
  
gw2016$year[gw2016$id <5000] <- "2015"

gw2017 <- gw2017 %>%
  full_join(gw2017codes) %>%
  mutate(month = as.character(month),
         year = "2017")

gw2018 <- gw2018 %>%
  full_join(gw2018codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL)),
         year = "2018"
  )

gw2019 <- gw2019 %>%
  full_join(gw2019codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL)),
         year = "2019"
  )

gw2020 <- gw2020 %>%
  full_join(gw2020codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL)),
         year = "2020"
  )

all <- gw2016 %>%
  full_join(gw2017) %>%
  full_join(gw2018) %>%
  full_join(gw2019) %>%
  full_join(gw2020) %>%
  filter(!is.na(id)) %>%
  filter(!is.na(site)) %>%
  rename(sampleID = id)

all$trt[all$position == "CTL" | all$position == "CTL-Bot" | all$position == "CTL-Top"] <- "CTL"
all$trt[is.na(all$trt)] <- "TRT"
all$position[all$position == "CTL"] <- "Bot"
is.na(all$sampleID) <- NULL


# water quality - correcting typo's, wrong codes, etc. ------------------------------------

corrected <- as.data.frame(lapply(all[c(3:5,8)], fixit)) %>%
  rename(site = x, position = x.1, month = x.2, trt = x.3)

all$site <- corrected$site
all$position <- corrected$position
all$month <- corrected$month
all$trt <- corrected$trt
all <- all %>%
  mutate(pos = paste(trt, position, sep = "-"))


all$order <- factor(all$month, levels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

write.csv(all, file = "C:/Users/Chris/Documents/prairiestrips/csvdata/allgwnutrients.csv")
saveRDS(all, file = "C:/Users/Chris/Documents/prairiestrips/data/allgwnutrients.rds")


# gw depth wrangle --------------------------------------------------------

#FILE IMPORTING
setwd("~/prairiestrips/data-raw/groundwater/depth")

gwdepthfileslist <- list.files(pattern = "*.csv", recursive = TRUE, full.names = TRUE)

gwdepth <- map_dfr(gwdepthfileslist, read.csv) %>% 
  mutate(month = as.character(month), uncorrecteddepthft = as.character(uncorrected.depth..ft.)) %>%
  select(-X, -uncorrected.depth..ft.) %>%
  mutate(uncorrecteddepthft = as.numeric(replace(uncorrecteddepthft, uncorrecteddepthft == "dry", 15)))


# correcting typo's  ------------------------------------------------------

correctedtypo <- map_dfc(gwdepth[c(2:5)], fixit)%>%
  rename(month = x...1, site = x...2, trt = x...3, pos = x...4)

gwdepth <- gwdepth %>% select(-c(2:5)) %>% cbind(correctedtypo) %>%
  relocate(c(4:7), .after = year) %>%
  mutate(wellid = paste(site, trt, pos, sep = ""))

# correcting depth values due to well height above ground -----------------

depthadj <- read.csv("~/prairiestrips/data-raw/groundwater/depthadjustment.csv", header = T) %>%
  select(wellid, abovegroundin)

gwdepth <- left_join(gwdepth, depthadj) %>%
  select(-c(10:12)) %>%
  mutate(adjdepthft = round(uncorrecteddepthft - (abovegroundin/12), digits = 2)) %>%
  mutate(negadjdepthft = -(adjdepthft))

gwdepth$order <- factor(gwdepth$month, levels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

write.csv(gwdepth, file = "C:/Users/Chris/Documents/prairiestrips/csvdata/gwdepth.csv")
saveRDS(gwdepth, file = "C:/Users/Chris/Documents/prairiestrips/data/gwdepth.rds")

rm(list=setdiff(ls(), c("all", "gwdepth")))
