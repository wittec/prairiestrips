
rm(list=ls(all=TRUE))

source("~/prairiestrips/R/groundwaterwrangle.R")

# Customized graph settings ------------------------------------------------------
colorscale <- c(TRT = "blue", 
                CTL = "red",
                MCL = "green")
linescale <- c(Top = "dashed",
               Bot = "solid",
               MCL = "solid")


# graphing no3 data ----------------------------------------------------------------

no3data <- all %>%
  filter(no3mgL!="NA" & year != "2015")

no3 <- ggplot(no3data, aes(x = order, 
                   y = no3mgL, 
                   group = pos,
                  linetype = position,
                 color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  #add in next line for max concentration limit for drinking water 
  #geom_hline(aes(color = "MCL", yintercept = 10)) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Nitrate - Nitrogen (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))


ggsave(filename = "~/prairiestrips/graphs/gwno3.jpg", plot=no3, width = 6, height=8)


# graphing no3 data without ctl----------------------------------------------------------------

colorscalenoctl <- c(Top = "red", 
                Bot = "blue")

no3datanoctl <- all %>%
  filter(no3mgL!="NA" & year != "2015" & trt != "CTL")

no3noctl <- ggplot(no3datanoctl, aes(x = order, 
                           y = no3mgL, 
                           group = pos,
                           #linetype = position,
                           color = position)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  #add in next line for max concentration limit for drinking water 
  #geom_hline(aes(color = "MCL", yintercept = 10)) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Nitrate - Nitrogen (mg/L)') + 
  scale_color_manual(values = colorscalenoctl) +
  #scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))


ggsave(filename = "~/prairiestrips/graphs/gwno3noctl.jpg", plot=no3noctl, width = 6, height=8)


# graphing drp data ----------------------------------------------------------------

drpdata <- filter(all, year>="2017", drpmgL!="NA")
drpdata$drpmgL <- as.numeric(drpdata$drpmgL)

drp <- ggplot(drpdata, aes(x = order, 
                   y = drpmgL, 
                   group = pos,
                   linetype = position,
                   color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Dissolved Phosphorus (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/gwdrp.jpg", plot=drp, width = 6, height=8)


# gwdepth graph -----------------------------------------------------------
gwdepthplot <- ggplot(gwdepth, aes(x = order, 
                           y = negadjdepthft, 
                           group = wellid,
                           linetype = pos,
                           color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscalenoctl) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))


ggsave(filename = "~/prairiestrips/graphs/gwdepth.jpg", plot=gwdepthplot, width = 6, height=8)

# gwdepth graph without top-----------------------------------------------------------

colorscalenotop <- c(CTL = "red", 
                     TRT = "blue")

gwdepthnotop <- gwdepth %>% filter(pos != "Top")

gwdepthplotnotop <- ggplot(gwdepthnotop, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = wellid,
                                   #linetype = pos,
                                   color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscalenotop) +
  #scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))


ggsave(filename = "~/prairiestrips/graphs/gwdepthnotop.jpg", plot=gwdepthplotnotop, width = 6, height=8)


# site specific graphs ----------------------------------------------------

arm <- all %>%
  filter(site=="ARM")

armno3 <- arm %>%
  filter(year == "2016" | year == "2017")

armno3plot <- ggplot(data = subset(armno3, !is.na(no3mgL)), aes(x = order, 
                          y = no3mgL, 
                          group = pos,
                          linetype = position,
                          color = trt)) +
  ggtitle("Groundwater Nitrate - Nitrogen") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Nitrate - Nitrogen (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

armno3plot

ggsave(filename = "~/prairiestrips/graphs/gwarmno3.jpg", plot=armno3plot, width = 6, height=6)

armdrp <- arm %>%
  filter(year ==2017)

armdrpplot <- ggplot(armdrp, aes(x = order, 
                           y = drpmgL, 
                           group = pos,
                           linetype = position,
                           color = trt)) +
  ggtitle("Groundwater Dissolved Reactive Phosphorus") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Dissolved Reactive Phosphorus (mg/L)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "gwarmdrp.jpg", plot=armdrpplot, width = 6, height= 6)


armgwdepth <- gwdepth %>%
  filter(site=="ARM")

armgwdepthplot <- ggplot(armgwdepth, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = wellid,
                                   linetype = pos,
                                   color = trt)) +
  ggtitle("Groundwater Depth from Ground Surface") +
  geom_line() + 
  geom_point() +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

ggsave(filename = "~/prairiestrips/graphs/armgwdepth.jpg", plot=armgwdepthplot, width = 6, height=6)



# year specific gw depth graph --------------------------------------------

y2019 <- gwdepth %>%
  filter(year == "2019")


# Customized colors
colorscale <- c(TRT = "blue", 
                CTL = "red")
linescale <- c(Top = "dashed",
               Bot = "solid")

yeargwdepthplot <- ggplot(y2019, aes(x = order, 
                                   y = negadjdepthft, 
                                   group = wellid,
                                   linetype = pos,
                                   color = trt)) +
  geom_line(size = 1) + 
  geom_point(size= 1.5) +
  facet_grid(site~year, scales='free_x') + 
  labs(x = '',  
       y = 'Groundwater Depth From Ground Surface (ft)') + 
  scale_color_manual(values = colorscale) +
  scale_linetype_manual(values = linescale) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.text.x = element_text(angle=60,hjust=1))

yeargwdepthplot




# testing cowplot ---------------------------------------------------------


library(cowplot)
armno3drplot <- plot_grid(armno3plot, armdrpplot, labels = c("A", "B"))

armno3drplot
