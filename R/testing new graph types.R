
rm(list=ls(all=TRUE))

library(tidyverse)

load(file = "~/prairiestrips/data/clippedrainandflowdataallyears.rds")

e <- d %>%
  group_by(site, treatment, year) %>%
  filter(y == max(y))
  
e <- e[ !duplicated(e$y), ] 

rainsum <- e %>%
  #group_by(site, treatment, year) %>%
  filter(treatment == "rain") %>%
  mutate(y=round(y, digits = 1))
  #filter(y==max(y))


colorscale <- c(control = "red",
                treatment = "blue")

f <- ggplot() +
                                                   
  geom_col(data = e %>% filter(treatment != "rain"),
           aes(x=treatment, y=y, group = watershed, fill = treatment), position = "dodge") +
  facet_grid(codes~year) +#, scales='free_x') + 
  labs(x = '',  
       y = 'Cumulative Runoff (inches)') + 
  scale_fill_manual(values = colorscale) +
  theme(legend.position = "bottom",
        legend.title    = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(data = e %>% filter(treatment == "rain") %>% mutate(y=round(y, digits = 1)),
            aes(x=treatment, y=5, label=y, size = 0.5))
  # annotate(data = rainsum,
  #          "text", x="treatment", y=5, label=rainsum$y,
  #        color="red")
  # 
f


ggsave(filename = "C:/Users/Chris/Documents/prairiestrips/graphs/runoff/runoff2020columnswithoutrain.jpg", plot=f, width = 6, height=8)
