library(dplyr)
library(hoopR)
library(tidyverse)

fpts <- function(df){
    df$fgm*2 +
    df$fga*(-1) +
    df$ftm +
    df$fta*(-1) +
    df$fg3m +
    df$reb +
    df$ast*2 +
    df$stl*4 +
    df$blk*4 +
    df$to*(-2) +
    df$pts   
}

season_box_scores <- load_nba_player_box(2022) %>% 
  select(athlete_display_name, team_name, team_id, game_date, game_id, fg, fg3, ft, reb, ast, stl, blk, to, pts)

season_box_scores <- season_box_scores %>% 
  separate(fg, c('fgm', 'fga'), sep = "-", convert = TRUE, extra = "drop") %>% 
  separate(fg3, c('fg3m', 'fg3a'), sep = "-", convert = TRUE, extra = "drop") %>% 
  separate(ft, c('ftm', 'fta'), sep = "-", convert = TRUE, extra = "drop") %>% 
  mutate(
    fgm = ifelse(is.na(fgm), 0, fgm),
    fga = ifelse(is.na(fga), 0, fga),
    fg3m = ifelse(is.na(fg3m), 0, fg3m),
    fg3a = ifelse(is.na(fg3a), 0, fg3a),
    ftm = ifelse(is.na(ftm), 0, ftm),
    fta = ifelse(is.na(fta), 0, fta),
    reb = as.numeric(gsub("--", "0", reb)),
    ast = as.numeric(gsub("--", "0", ast)),
    stl = as.numeric(gsub("--", "0", stl)),
    blk = as.numeric(gsub("--", "0", blk)),
    to = as.numeric(gsub("--", "0", to)),
    pts = as.numeric(gsub("--", "0", pts))
  )

season_box_scores <- season_box_scores %>% mutate(fpts = fpts(.))

all_games <- season_box_scores %>% group_by(game_id, team_id) %>% group_split()

filename <- paste0("fantasy_data_", Sys.Date())
write.csv(all_games, file = filename)

# Need to see if we can do this by season...
# update_nba_db(force_rebuild = 2023)

nba_commonteamroster(season = 2021)$CommonTeamRoster %>% View()

# Webscrape full rosters from basketball-reference? 
# It at least lists the inactive players...

