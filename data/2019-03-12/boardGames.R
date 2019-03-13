require(tidyverse)

data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")

# Take a peek at specs for Twilight Struggle
tw <- data %>% 
  filter(name=="Twilight Struggle")

# Extract from the data those games that are 2-5 hours long and involve dice rolling
longDiceGames <- data %>% 
  filter(playing_time >= 120) %>% 
  filter(playing_time <= 300) %>% 
  mutate(diceCat = case_when(
    str_detect(mechanic, "Dice Rolling") ~ "Dice",
    TRUE ~ NA_character_
  )) %>% 
  mutate(playing_hours = playing_time / 60) %>% 
  filter(diceCat=="Dice")


g <- ggplot(longDiceGames) +
  aes(x = playing_hours
      ,y = average_rating
      ,color = factor(max_players)
  ) +
  geom_point(alpha = .5) +
  geom_smooth() +
  facet_wrap(~max_players) +
  guides(colour=F) +
  ggtitle("Correlation between game length and rating") +
  labs(x="Average hours played",y="Gamers' rating"
       , caption = "Data source: Board Game Geek Database"
       , subtitle = "Among dice-rolling games lasting 2-5 hours") +
  theme_minimal()
  
g

ggsave("longDiceGames.png",g,width=12,height=7)
