library(ggplot2)
library(reshape)
library(tidyverse)

game_stats <- read.csv("game_stats.csv")

game_stats %>% 
  select(athlete_display_name, fantasy_pts, game_id) %>% 
  filter(athlete_display_name == "Kevin Durant") %>% 
  ggplot(aes(x = game_id, y = fantasy_pts)) +
  geom_line() +
  theme_minimal()

game_stats %>% 
  select(athlete_display_name, fantasy_pts) %>% 
  filter(athlete_display_name == "Kevin Durant") %>% 
  ggplot(aes(x = fantasy_pts)) + 
  geom_histogram(aes(y = ..density..),      # Histogram with density instead of count on y-axis
                 binwidth = 5,
                 color = "black", 
                 fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +  # Overlay with transparent density plot
  geom_vline(aes(xintercept = mean(fantasy_pts, na.rm = TRUE)),   # Ignore NA values for mean
             color = "red", linetype = "dashed", size = 1) +
  theme_minimal()
