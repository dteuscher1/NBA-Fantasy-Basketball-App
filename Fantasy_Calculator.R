library(hoopR)
data <- hoopR::espn_nba_player_box(401360376)
head(data)
library(tidyverse)
data2 <- data %>% 
    separate(fg, c("fgm", "fga"), sep = "-", convert = TRUE) %>%
    separate(ft, c("ftm", "fta"), sep = "-", convert = TRUE) %>%
    separate(fg3, c("fg3m", "fg3a"), sep = "-", convert = TRUE) %>%
    mutate_at(c(12:16, 19), as.numeric) %>%
    mutate(fantasy_pts = 2 * fgm -fga + ftm - fta + fg3m + reb + 2*ast + 4*stl + 4*blk - 2*to + pts)


# Average points over x amount of games (Whole season or x amount of games)
# Variance of fantasy points
# Sharp ratio (???)


# Plot to compare players 
# Set window 

# Moving average

# How many games they have played

