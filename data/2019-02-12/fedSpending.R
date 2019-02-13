require(tidyverse)

# Get the data
url1 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv'
url3 <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv'

fed.dat <- read_csv(url3)
clim.dat <- read_csv(url1)

# Summarize using 5-year bins
fed.dat <- fed.dat %>% 
  mutate(bin = case_when (
    (year >= 1976 & year <= 1980) ~ "1976-80",
    (year >= 1981 & year <= 1985) ~ "1981-85",
    (year >= 1986 & year <= 1990) ~ "1986-90",
    (year >= 1991 & year <= 1995) ~ "1991-95",
    (year >= 1996 & year <= 2000) ~ "1996-00",
    (year >= 2001 & year <= 2005) ~ "2001-05",
    (year >= 2006 & year <= 2010) ~ "2006-10",
    (year >= 2011 & year <= 2015) ~ "2011-15",
    (year >= 2016 & year <= 2017) ~ "2016-17",
    TRUE ~ NA_character_)
    )

fed.ave.dat <- fed.dat %>% 
  group_by(department,bin) %>% 
  summarise(rd_budget=mean(rd_budget,na.rm=T),
            total_outlays=mean(total_outlays,na.rm=T),
            discretionary_outlays=mean(discretionary_outlays,na.rm=T),
            gdp=mean(gdp,na.rm=T)) %>% 
  na.omit()
  

g <- ggplot(fed.ave.dat,
            aes(x=gdp
                ,y=rd_budget
                ,colour=bin
                )) +
  geom_point() +
  facet_wrap(~department) +
  labs(x = "GDP", y = "R&D Budget",colour="Years",subtitle = "* Note year correlates with GDP") +
  ggtitle("Research & Development budget as a function of GDP, allotted by department") +
  theme_minimal()
g

#============================================================
total.outlays <- fed.dat %>% 
  select(year,total_outlays) %>% 
  distinct()

clim.total.dat  <- left_join(clim.dat,total.outlays,by=c("year")) %>% 
  mutate(prop.spending = (gcc_spending/total_outlays)*100)

h <- ggplot(clim.total.dat,
            aes(x=year
                ,y= prop.spending
                ,fill=department
                )) +
  geom_area(colour="grey50") +
  labs(x = "Year", y = "Percent of total federal spending",fill="Department") +
  ggtitle("Proportion of R&D Climate Change spending to total federal government spending by department") +
  theme_minimal()
h

#============================================================

ggsave("deptBudgets.png",g,width=9,height=7)

ggsave("climateSpending.png",h,width=9,height=7)
