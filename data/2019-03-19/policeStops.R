library(tidyverse)

url <- "https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv"
combined_data <- read_csv(url)

# Basic descriptive plots to see trends regardless of location

stops_by_race <- combined_data %>%
  group_by(driver_race) %>% 
  summarize(mean_stop_rate = mean(stop_rate, na.rm=T), sd_stop_rate = sd(stop_rate, na.rm=T),
            mean_search_rate = mean(search_rate, na.rm=T), sd_search_rate = sd(search_rate, na.rm=T),
            mean_consent_search_rate = mean(consent_search_rate, na.rm=T), sd_consent_search_rate = sd(consent_search_rate, na.rm=T),
            mean_arrest_rate = mean(arrest_rate, na.rm=T), sd_arrest_rate = sd(arrest_rate, na.rm=T),
            mean_citation_rate_speeding_stops = mean(citation_rate_speeding_stops, na.rm=T), sd_citation_rate_speeding_stops = sd(citation_rate_speeding_stops, na.rm=T),
            mean_hit_rate = mean(hit_rate, na.rm=T), sd_hit_rate = sd(hit_rate, na.rm=T)
            ) %>%
gather(var,value,mean_stop_rate:sd_hit_rate) %>% 
mutate(stat = case_when(
    str_detect(var, "mean_") ~ "mean",
    str_detect(var, "sd_") ~ "sd",
    TRUE ~ NA_character_
  )) %>% 
mutate(metric = case_when(
  str_detect(var, "stop_rate") ~ "stop_rate",
  str_detect(var, "consent_search_rate") ~ "consent_search_rate", #Do this before str_detect(search_rate) otherwise "consent_search_rate" will be coded as plain "search_rate"
    str_detect(var, "search_rate") ~ "search_rate",
  str_detect(var, "arrest_rate") ~ "arrest_rate",
  str_detect(var, "citation_rate_speeding_stops") ~ "citation_rate_speeding_stops",
  str_detect(var, "hit_rate") ~ "hit_rate",
  TRUE ~ NA_character_
)) %>% 
  select(driver_race,metric,value,stat) %>% 
  spread(stat,value)

stop_rates <- stops_by_race %>%
  filter(metric=="stop_rate")

postStopOutcomes <- stops_by_race %>%
  filter(metric!="stop_rate")

searchArrest <- stops_by_race %>% 
  filter(metric %in% c("consent_search_rate", "search_rate", "arrest_rate"))

speedingContraband <- stops_by_race %>% 
  filter(metric %in% c("citation_rate_speeding_stops", "hit_rate"))

g <- ggplot(data=stop_rates,
            aes(x=driver_race,
                y=mean,
                group=metric
                )) +
  geom_point(stat = "identity", size=1.4) +
  geom_line(stat = "identity", size=2.0, colour="grey30") +
  guides(colour=F) +
  ggtitle("Police stop rates by race") +
  labs(x="Driver's race",y="Rate (%)"
       , caption = "Data from \nThe Stanford Open Policing Project, arXiv:1706.05678") +
  theme_minimal(base_size=14)
print(g)

# Since the stop rates above had a huge range, plot the post-stop outcomes on a separate graph:
h <- ggplot(data=postStopOutcomes,
            aes(x=driver_race,
                y=mean,
                group=metric,
                colour=metric)) +
  geom_point(stat = "identity", size=1.4) +
  geom_line(stat = "identity", size=1.1) +
  scale_colour_discrete(name="Outcome",
                         breaks=c("consent_search_rate", "search_rate", "arrest_rate","citation_rate_speeding_stops","hit_rate"),
                         labels=c("Consent to search", "Search", "Arrest", "Speeding citation", "Contraband found during search")) +
  ggtitle("Rates of post-stop outcomes by race") +
  labs(x="Driver's race",y="Rate (%)"
       , caption = "Data from \nThe Stanford Open Policing Project, arXiv:1706.05678") +
  theme_minimal(base_size=14)
print(h)


## Speeding citations and contraband found rates are disproportionately large compared to the three other outcomes, so plot separately
i <- ggplot(data=searchArrest,
            aes(x=driver_race,
                y=mean,
                group=metric,
                colour=metric)) +
  geom_point(stat = "identity", size=1.4) +
  geom_line(stat = "identity", size=1.1) +
  scale_colour_discrete(name="Outcome",
                        breaks=c("consent_search_rate", "search_rate", "arrest_rate"),
                        labels=c("Consent to search", "Search", "Arrest")) +
  ggtitle("Rates of post-stop outcomes by race") +
  labs(x="Driver's race",y="Rate (%)"
       , caption = "Data from \nThe Stanford Open Policing Project, arXiv:1706.05678") +
  theme_minimal(base_size=14)
print(i)

j <- ggplot(data=speedingContraband,
            aes(x=driver_race,
                y=mean,
                group=metric,
                colour=metric)) +
  geom_point(stat = "identity", size=1.4) +
  geom_line(stat = "identity", size=1.1) +
  scale_colour_discrete(name="Outcome",
                        breaks=c("citation_rate_speeding_stops","hit_rate"),
                        labels=c("Speeding citation", "Contraband found during search")) +
  ggtitle("Rates of post-stop outcomes by race") +
  labs(x="Driver's race",y="Rate (%)"
       , caption = "Data from \nThe Stanford Open Policing Project, arXiv:1706.05678") +
  theme_minimal(base_size=14)
print(j)

ggsave("stopRates.png",g,width=5,height=5)
ggsave("postStopOutcomes.png",h,width=7,height=5)
ggsave("searchArrest.png",i,width=7,height=5)
ggsave("speedingContraband.png",j,width=7,height=5)

