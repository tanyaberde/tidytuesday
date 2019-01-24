
require(tidyverse)
require(ggplot2)

# Get the data
url1 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-22/incarceration_trends.csv'

incarc_dat <- read_csv(url1)

# Add a row ID for later joining if needed
d2 <- incarc_dat %>% 
  mutate(row_id = row_number())

# Have other_state coded above _state_ or else case_when will mislabel
holds <- d2 %>% 
  select(yfips:total_pop_15to64, urbanicity:land_area, jail_from_state_prison:jail_from_ice, row_id) %>% 
  gather(agency, pop_count, jail_from_state_prison:jail_from_ice) %>% 
  mutate(agency = case_when(str_detect(agency, "other_state_prison") ~ "Out-of-State Prison",
                                str_detect(agency, "from_state_prison") ~ "State Prison",
                                str_detect(agency, "other_state_jail") ~ "Out-of-State Jail",
                                str_detect(agency, "from_state_jail") ~ "State Jail",
                                str_detect(agency, "_fed") ~ "All Federal Authorities",
                                str_detect(agency, "_ice") ~ "ICE or INS",
                                TRUE ~ NA_character_))

holds2 <- holds %>% 
  mutate(ratio = (pop_count/total_pop_15to64)*100)


# Summary of number of individuals held for other agencies, depending on urbanicity and year

holds_summ <- holds2 %>% 
  na.omit() %>% 
  group_by(year, urbanicity, agency) %>% 
  summarize(average_prop = mean(ratio),
            average_total_pop = mean(total_pop_15to64),
            average_pop_count = mean(pop_count)) %>% 
  ungroup()


# Plot
g <- ggplot(holds_summ,
            aes(x = year, y = average_prop, color = agency )) +
  geom_line(stat="identity",size=1.1
  ) +
  scale_color_brewer(type = "div") +
  facet_wrap(~urbanicity) +
  labs(x = "Year", y = "Proportion to facility population aged 15-64", color="Agency") +
  ggtitle("Prisoners being held for in-state or external authorities, per urbanicity category") +
  theme_minimal(base_size = 12)


print(g)

ggsave("holds.png"
       ,plot = g
       ,width=9
       ,height=7)
