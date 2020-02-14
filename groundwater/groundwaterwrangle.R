
# groundwater water quality data manip ------------------------------------

rm(list=ls(all=TRUE))


library("tidyverse")

# function to clean up names and/or replace typeO's -------------------------

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


#importing water quality files -----------------------------------------
#
gw2016codes <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2016/2016gwcodes.csv", header = T) %>%
  select(-Sample.Source) %>%
  filter(!is.na(ID.)) %>%
  rename(id = ID., position = Position, site = Site, date = Date)

gw2016 <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2016/2016gwno3results2.csv", skip = 21, header = T)[ , 1:3] %>%
  select(ID..., NOx.result..mg.N.L.) %>%
  rename(id = ID..., no3mgL = NOx.result..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2016a <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2016/2016gwno3results.csv", skip = 20, header = T)[ , 1:3] %>%
  select(ID..., Nitrate.nitrite..mg.N.L.) %>%
  rename(id = ID..., no3mgL = Nitrate.nitrite..mg.N.L.) %>%
  mutate(id = as.numeric(as.character(id))) %>%
  filter(!is.na(id))

gw2017codes <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2017/STRIPS2017groundwatercodes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2017 <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2017/STRIPS2017groundwaterresults.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)
#left_join(gw2017codes, by = c("id"))

gw2018codes <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2018/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") 

gw2018 <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2018/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

gw2019codes <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2019/groundwater_codes.csv", skip = 2)[ , 1:8] %>%
  select(year, month, site, trt, position, ID.) %>%
  rename(id = ID.) %>%
  filter(id != "NA") %>%
  mutate(id = as.numeric(as.character(id)))

gw2019 <- read.csv("~/prairiestrips/groundwater/data-raw/waterquality/2019/groundwater.csv", skip = 2, header = T) %>%
  select(Sample.ID., NOx.result..mg.N.L., DRP.result..mg.P.L.) %>%
  rename(id = Sample.ID., no3mgL = NOx.result..mg.N.L., drpmgL = DRP.result..mg.P.L.)

# joining the codes and results files ------------------------------------------------
gw2016 <- gw2016 %>%
  rbind(gw2016a) %>%
  full_join(gw2016codes) %>%
  rename(month = date) %>%
  mutate(month = as.character(month))

gw2017 <- gw2017 %>%
  full_join(gw2017codes) %>%
  mutate(month = as.character(month))

gw2018 <- gw2018 %>%
  full_join(gw2018codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL))
  )

gw2019 <- gw2019 %>%
  full_join(gw2019codes) %>%
  mutate(month = as.character(month),
         no3mgL = as.numeric(as.character(no3mgL)),
         drpmgL = as.numeric(as.character(drpmgL))
  )

all <- gw2016 %>%
  full_join(gw2017) %>%
  full_join(gw2018) %>%
  full_join(gw2019) %>%
  filter(!is.na(id)) %>%
  filter(!is.na(site)) %>%
  rename(sampleID = id)

all$trt[all$position == "CTL" | all$position == "CTL-Bot" | all$position == "CTL-Top"] <- "CTL"
all$trt[is.na(all$trt)] <- "TRT"
all$position[all$position == "CTL"] <- "Bot"
is.na(all$sampleID) <- NULL
all$year[all$sampleID <5000] <- "2015"
all$year[all$sampleID >= 5000 & all$sampleID < 6000] <- "2016"
all$year[all$sampleID >= 6000 & all$sampleID < 7000] <- "2017"
all$year[all$sampleID >= 7000 & all$sampleID <8000] <- "2018"
all$year[all$sampleID >= 8000] <- "2019"


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

write.csv(all, file = "C:/Users/Chris/Documents/prairiestrips/groundwater/allgwnutrients.csv")

# gw depth data manip -------------------------------------------

# fixit <- function (x) {
#   correct <- read.csv("~/prairiestrips/groundwater/data-raw/corrections.csv", header = T) %>%
#     mutate(bad = as.character(bad),
#            good = as.character(good)
#     )
#   
#   testdata <- as.data.frame(x)
#   
#   left_join(testdata, correct, by = c("x" = "bad"))  %>%
#     mutate(x = ifelse(!is.na(good), good, x)) %>%
#     select(-ends_with("good"))
#   
# }

gwdepth2016 <- read.csv("~/prairiestrips/groundwater/data-raw/depth/2016/2016strips2gwdepth.csv", header = T)

gwdepth2017 <- read.csv("~/prairiestrips/groundwater/data-raw/depth/2017/2017strips2gwdepth.csv", header = T) 

gwdepth2018 <- read.csv("~/prairiestrips/groundwater/data-raw/depth/2018/2018strips2gwdepth.csv", header = T) 

gwdepth2019 <- read.csv("~/prairiestrips/groundwater/data-raw/depth/2019/2019strips2gwdepth.csv", header = T) 

gwdepth <- rbind(gwdepth2016, gwdepth2017, gwdepth2018, gwdepth2019) %>%
  mutate(month = as.character(month)) %>%
  select(-X)

#changing dry wells to depth of 15
gwdepth$uncorrected.depth..ft. <- as.character(gwdepth$uncorrected.depth..ft.)
gwdepth$uncorrected.depth..ft.[gwdepth$uncorrected.depth..ft.== "dry"] <- 15
gwdepth$uncorrected.depth..ft. <- as.numeric(gwdepth$uncorrected.depth..ft.)


correcteddepth <- as.data.frame(lapply(gwdepth[c(2:5)], fixit)) %>%
  rename(month = x, site = x.1, trt = x.2, pos = x.3)

gwdepth$month <- correcteddepth$month
gwdepth$site <- correcteddepth$site
gwdepth$trt <- correcteddepth$trt
gwdepth$pos <- correcteddepth$pos
gwdepth <- gwdepth %>%
  mutate(wellid = paste(site, trt, pos, sep = ""),
         uncorrected.depth..ft. = as.numeric(as.character(uncorrected.depth..ft.)))

depthadj <- read.csv("./groundwater/data-raw/depth/depthadjustment.csv", header = T) %>%
  select(wellid, abovegroundin)
gwdepth <- left_join(gwdepth, depthadj) %>%
  mutate(adjdepthft = round(uncorrected.depth..ft. - (abovegroundin/12), digits = 2))

gwdepth$negadjdepthft <- -(gwdepth$adjdepthft)

gwdepth$order <- factor(gwdepth$month, levels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec."))

write.csv(gwdepth, file = "C:/Users/Chris/Documents/prairiestrips/groundwater/gwdepth.csv")

rm(list=setdiff(ls(), c("all", "gwdepth")))
