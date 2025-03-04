library(dplyr)
library(hoopR)
library(tidyverse)

# Comment as a test

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

season_box_scores <- load_nba_player_box(2025) %>% 
  select(athlete_display_name, team_name, team_abbreviation, team_id, game_date, game_id,
         field_goals_made, field_goals_attempted, three_point_field_goals_made, 
         three_point_field_goals_attempted, free_throws_made, free_throws_attempted,
         rebounds, assists, steals, blocks, turnovers, points)

season_box_scores <- season_box_scores %>%
  rename(fgm = field_goals_made,
         fga = field_goals_attempted,
         fg3m = three_point_field_goals_made,
         fg3a = three_point_field_goals_attempted,
         ftm = free_throws_made,
         fta = free_throws_attempted,
         reb = rebounds, 
         ast = assists,
         stl = steals,
         blk = blocks,
         to = turnovers,
         pts = points) %>%
  filter(!is.na(fgm), !is.na(fga)) %>%
  mutate(
    fgm = ifelse(is.na(fgm), 0, fgm),
    fga = ifelse(is.na(fga), 0, fga),
    fg3m = ifelse(is.na(fg3m), 0, fg3m),
    fg3a = ifelse(is.na(fg3a), 0, fg3a),
    ftm = ifelse(is.na(ftm), 0, ftm),
    fta = ifelse(is.na(fta), 0, fta),
    reb = ifelse(is.na(reb), 0, reb),
    ast = ifelse(is.na(ast), 0, ast),
    stl = ifelse(is.na(stl), 0, stl),
    blk = ifelse(is.na(blk), 0, blk),
    to = ifelse(is.na(to), 0, to),
    pts = ifelse(is.na(pts), 0, pts)
  )

season_box_scores <- season_box_scores %>% mutate(fpts = fpts(.))

all_games <- season_box_scores %>% group_by(game_id, team_id) %>% group_split()

print(Sys.Date())
filename <- paste0("/Users/davidteuscher/Documents/Statistics/Basketball/NBA-Fantasy-Basketball-App/fantasy_data.csv")
write.csv(season_box_scores, file = filename, row.names = FALSE)

# Need to see if we can do this by season...
# update_nba_db(force_rebuild = 2023)

#nba_commonteamroster(season = 2021)$CommonTeamRoster %>% View()

# Webscrape full rosters from basketball-reference? 
# It at least lists the inactive players...

