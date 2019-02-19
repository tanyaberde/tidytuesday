require(tidyverse)
require(ggrepel)

url1 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv'

phd_field_dat <- read_csv(url1)



## Get total Phds per year
total_phds <- phd_field_dat %>% 
  group_by(year) %>% 
  summarize(year_n_phds = sum(n_phds, na.rm=T))

## Left join this with the master dataset, and bin the years in case needed later
phd_field_dat2 <- left_join(phd_field_dat, total_phds, by=c("year")) %>% 
  mutate(percent = (n_phds/year_n_phds)*100,
         bin = case_when (
           (year >= 2008 & year <= 2010) ~ 2010,
           (year >= 2011 & year <= 2013) ~ 2013,
           (year >= 2014 & year <= 2016) ~ 2016,
           (year == 2017) ~ 2017,
           TRUE ~ NA_real_
           )
         )

# Quick summary by field and year/bin
broad_summ <- phd_field_dat2 %>% 
  group_by(year,broad_field) %>% 
  rename(fld = broad_field) %>% 
  summarize(n_phds = sum(n_phds, na.rm=T),
            percent = sum(percent, na.rm=T))

major_summ <- phd_field_dat2 %>% 
  group_by(year,major_field,broad_field) %>% 
  summarize(n_phds = sum(n_phds, na.rm=T),
            percent = sum(percent, na.rm=T))
            
socsci_summ <- phd_field_dat2 %>% 
  filter(broad_field=="Psychology and social sciences") %>% 
  group_by(bin,major_field,field) %>% 
  summarize(n_phds = sum(n_phds, na.rm=T),
            percent = sum(percent, na.rm=T))

psyc_summ <- phd_field_dat2 %>% 
  filter(major_field=="Psychology") %>% 
  group_by(year,field) %>% 
  summarize(n_phds = sum(n_phds, na.rm=T),
            percent = sum(percent, na.rm=T))

# Filter required rows for the labels in plot g
plotLabs <- major_summ[major_summ$year == 2008, ] # since 2008 gets plotted on the left
plotLabs$label <- plotLabs$major_field


# Plots

## Broad areas
g <- ggplot(major_summ
            ,aes(x=year
                 ,y=log(percent)
                 ,colour=major_field
                 )) +
  geom_line(size=0.8) +
  geom_point(size=0.8) +
  geom_label_repel(data=plotLabs,
                  aes(label=label,alpha=.9)
                  , size=3.5
                  , direction = "both"
                  ) +
  facet_wrap(~broad_field) +
  scale_x_continuous(breaks=c(2008:2017)) +
  guides(colour=F,alpha=F) +
  ggtitle("Proportion of PhDs in broad areas of study") +
  labs(x="Year",y="Share of total annual PhDs (%, log-transformed)",
       caption = "Data source: NSF National Center for Science and Engineering Statistics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=.5))

print(g)

## Psychology
i <- ggplot(psyc_summ
            ,aes(x=year
                 ,y=log(percent)
                 ,colour=field
            )) +
  geom_line(size=0.8) +
  geom_point(size=0.8) +
  # geom_label_repel(data=plotLabs3,
  #                  aes(label=label,alpha=.9)
  #                  , size=3.5
  #                  , direction = "both"
  # ) +
  facet_wrap(~field) +
  scale_x_continuous(breaks=c(2008:2017)) +
  guides(colour=F) +
  ggtitle("Proportion of PhDs in psychology") +
  labs(x="Year",y="Share of total annual PhDs (%, log-transformed)"
       , caption = "Data source: NSF National Center for Science and Engineering Statistics") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=.5))

print(i)

#============================================================

ggsave("phdsBroadFields.png",g,width=12,height=7)
ggsave("phdsPsyc.png",i,width=12,height=7)
