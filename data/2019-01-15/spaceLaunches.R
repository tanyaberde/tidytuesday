require(tidyverse)
require(ggplot2)

# Get the data
url1 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/agencies.csv'
dat1 <- read_csv(url1)
url2 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-15/launches.csv'
dat2 <- read_csv(url2)

# Bin the years in decades (10)

## First make new variables
## Decade
dat2$decade = ifelse(1960 <= launch_year & launch_year <= 1969, 1960
                    ,ifelse(1970 <= launch_year & launch_year <= 1979, 1970
                            ,ifelse(1980 <= launch_year & launch_year <= 1989, 1980
                                    ,ifelse(1990 <= launch_year & launch_year <= 1999, 1990
                                            ,ifelse(2000 <= launch_year & launch_year <= 2009, 2000
                                                    ,2010)))))

## Get average number of successes every decade for each agency, just for state launch providers

### First count number of successes and failures for each agency & decade
categ <- dat2 %>% 
  filter(agency_type %in% c("state")) %>% 
  group_by(state_code, decade, category) %>%
  tally

### Then count total attempts
total <- dat2 %>% 
  filter(agency_type %in% c("state")) %>% 
  group_by(state_code, decade) %>% 
  tally

categ_total <- merge(categ,total,by=c("state_code","decade")) # Join the two tables 

categ_total <- categ_total %>%
  filter(category %in% c("O")) %>%  # Only include the successes
    mutate(success_rate = n.x/n.y)


# Plot
g <- ggplot(categ_total,
  aes(x = decade, y = success_rate, fill = state_code )) +
  geom_bar(stat="identity",size=1.5
            ) +
# scale_fill_brewer(type = "qual", palette=2) +
  facet_wrap(~state_code) +
  labs(x = "Country", y = "Success Rate") +
  ggtitle("Success rate of state-sponsored launches relative to total attempts per decade") +
  theme_minimal()

print(g)
