game_info <- read.csv("game_stats.csv")

game_info <- game_info %>% 
  select(
    -athlete_jersey, 
    -athlete_headshot_href, 
    -athlete_position_name, 
    -team_logo, 
    -team_alternate_color, 
    -team_color, 
    -team_short_display_name,
    -ejected,
    -starter,
    -athlete_short_name,
    -team_abbreviation,
    -did_not_play
  )

athletes <- game_info %>% select(athlete_display_name, athlete_id) %>% unique()

game_info %>% filter(athlete_display_name == "Kevin Durant") %>% select(fantasy_pts)

game_info %>% filter(athlete_display_name == "Trae Young") %>% select(fantasy_pts)

game_info %>% filter(athlete_display_name == "Kevon Looney") %>% select(fantasy_pts)

OKC_test <- game_info %>% filter(team_name == "Thunder")

OKC_test %>% select(game_id) %>% unique() %>% length()
# Will have to figure out players who changed teams during the season...

thing <- expand.grid(unique(OKC_test$athlete_display_name), unique(OKC_test$game_id))
# Full list of games and roster

unlist(game_info %>% filter(athlete_display_name == "Kevin Durant") %>% select(fantasy_pts)) %>% mean()

x <- seq(1, 99, by = 1)

sum(x / factorial(x + 1))


library(hoopR)

cavs_roster <- nba_commonteamroster(
  league_id = "00",
  season = "2021-22",
  team_id = "1610612739"
)$CommonTeamRoster
