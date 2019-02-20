rm(list=ls())

require(tidyverse)
dat1 <- "cleaned_annual_data/2015_031.csv"
dat2 <- "cleaned_annual_data/2016_031.csv"
dat3 <- "cleaned_annual_data/2017_031.csv"

dat_15 <- read_csv(dat1)
dat_16 <- read_csv(dat2)
dat_17 <- read_csv(dat3)

dat_merge1 <- rbind(dat_15,dat_16)
dat_merge2 <- rbind(dat_merge1, dat_17)

master_dat <- dat_merge2 %>% 
  arrange(year,major_field,time_to_degree)
#=============================================
all_grad <- master_dat %>% 
  filter(time_to_degree %in% c("Since starting graduate school","Since starting doctoral program")) %>% 
  na.omit()
  
socsci_dat <- master_dat %>% 
  filter(major_field=="Psychology and social sciences") %>% 
  arrange(year,time_to_degree)

socsci_grad <- socsci_dat %>% 
  filter(time_to_degree=="Since starting graduate school") %>% 
  na.omit()

# Plots

g <- ggplot(data=socsci_grad,
            aes(x=year
                ,y=md_years
                # ,fill=time_to_degree
                )) +
  geom_bar(stat="identity")
print(g)

h <- ggplot(data=all_grad,
            aes(x=year
                ,y=md_years
                ,colour=time_to_degree
            )) +
  geom_line(stat="identity",size=0.8) +
  facet_wrap(~major_field) +
  scale_color_brewer(type="qual", palette=2) +
  labs(color="Starting point") +
  ggtitle("Time to complete PhD across major fields") +
  labs(x="Year",y="Median number of years"
       , subtitle = "Since beginning graduate school"
       , caption = "Data source: NSF National Center for Science and Engineering Statistics from years 2015-2017") +
  theme_minimal(base_size=12) +
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=.5))

print(h)

ggsave("completionYears.png",h,width=12,height=7)
