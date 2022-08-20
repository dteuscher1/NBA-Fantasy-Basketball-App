library(hoopR)
data <- hoopR::espn_nba_player_box(401360376)
head(data)
library(tidyverse)
data2 <- data %>% 
    separate(fg, c("fgm", "fga"), sep = "-", convert = TRUE) %>%
    separate(ft, c("ftm", "fta"), sep = "-", convert = TRUE) %>%
    separate(fg3, c("fg3m", "fg3a"), sep = "-", convert = TRUE) %>%
    mutate_at(c(12:16, 19), as.numeric) %>%
    mutate(fantasy_pts = (2*fgm) - fga + ftm - fta + fg3m + reb + 2*ast + 4*stl + 4*blk - 2*to + pts)

games <- seq(from = as.Date("2021-10-19"), to = Sys.Date(), by = 1)
dates <- str_remove_all(games, "-")
game_ids <- c()
for (i in 1:length(dates)) {
    schedule <- hoopR::espn_nba_scoreboard(dates[i])
    game_ids <- c(game_ids, schedule$game_id)
}

game_info <- data.frame()
pb <- txtProgressBar(min = 0, max = length(game_ids), style = 3)
for (i in 1:length(game_ids)) {
    data <- tryCatch(hoopR::espn_nba_player_box(game_ids[i]), error = function(cond){
        return(NA)
    }
        )
    if (is.na(data)) {
        next
    } else {
        
        data2 <- data %>% 
            separate(fg, c("fgm", "fga"), sep = "-", convert = TRUE) %>%
            separate(ft, c("ftm", "fta"), sep = "-", convert = TRUE) %>%
            separate(fg3, c("fg3m", "fg3a"), sep = "-", convert = TRUE) %>%
            mutate_at(c(12:16, 19), as.numeric) %>%
            mutate(fantasy_pts = (2*fgm) - fga + ftm - fta + fg3m + reb + 2*ast + 4*stl + 4*blk - 2*to + pts,
                   game_id = game_ids[i])
        game_info <- game_info %>% bind_rows(data2)
        Sys.sleep(1)
    }
    setTxtProgressBar(pb, i)
}

close(pb)
write_csv(game_info, "game_stats.csv")


# Average points over x amount of games (Whole season or x amount of games)
# Variance of fantasy points
# Sharp ratio (???)


# Plot to compare players 
# Set window 

# Moving average

# How many games they have played

