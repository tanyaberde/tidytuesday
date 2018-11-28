# Tidy Tuesday 2018-11-27 Baltimore Bridges
# Constanza de Dios

# Read data
bridges <- read.csv("baltimore_bridges.csv"
                 ,header=T)

dat <- bridges
library(tidyverse)

dat1 <- dat %>% 
  mutate(age = 2018 - yr_built, # Calculate age of each bridge as of year 2018
         time_insp = 18 - inspection_yr # Calculate time since last inspection
)



g <- ggplot(dat1) +
  aes(x = age
      , y = avg_daily_traffic/10^3
      # , alpha = bridge_condition
      , color = factor(time_insp)
      ) +
  geom_point(stat="identity"
             , size = 1.5
             ) +
  # scale_alpha_discrete(breaks=c("Good","Fair","Poor"), breaks=1:3) + # Reorder labels of bridge condition
  scale_color_brewer(type = "qual", palette=3) +
  labs(x = "Age as of 2018", y = "Average Daily Traffic (thousands)"
       # , alpha = ""
       ,color = "Years since last inspection"
       ) +
  facet_grid(vars(bridge_condition), vars(county)) +
  ggtitle("Correlation between traffic and age of bridges across Maryland counties") +
  theme_bw()


print(g)

ggsave("MDbridges.png"
       ,plot = g
       ,width=12
       ,height=4)
