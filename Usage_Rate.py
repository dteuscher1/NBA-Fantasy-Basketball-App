from nba_api.stats.endpoints import boxscoretraditionalv2, boxscoreusagev2, leaguegamefinder
from nba_api.stats.static import  teams
import time
import pandas as pd


# Get games for the season up to this point

n_games = 333
season_data = []
for i in range(1,n_games+1):
    number_str = str(i)
    game_num = number_str.zfill(4)
    game_id_var = f'002230{game_num}'
    game_info = boxscoreusagev2.BoxScoreUsageV2(game_id = game_id_var).get_data_frames()[0]
    game_usage = game_info[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
    season_data.append(game_usage)
    if(i % 10 == 0):
      print(f'Getting game number {i}')
    
season_df = pd.concat(season_data)   
season_df.to_csv('usage_rates.csv')
    
yesterdays_games = leaguegamefinder.LeagueGameFinder(league_id_nullable='00', date_to_nullable='12/15/2023', date_from_nullable = '12/15/2023')
?leaguegamefinder.LeagueGameFinder()
games = yesterdays_games.get_data_frames()[0]

games.info()
game_ids = games['GAME_ID'].sort_values().unique()

def get_game_ids(today, last_day):
  previous_games = leaguegamefinder.LeagueGameFinder(league_id_nullable = '00', date_to_nullable = today, date_from_nullable = last_day).get_data_frames()[0]
  game_ids = previous_games['GAME_ID'].sort_values().unique()
  return game_ids


test = get_game_ids('12/15/2023', '12/14/2023')
usage_rate = []
for game in test:
  game_info = boxscoreusagev2.BoxScoreUsageV2(game_id = game).get_data_frames()[0]
  game_usage = game_info[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
  usage_rate.append(game_usage)
  

usage_rate_df = pd.concat(usage_rate)
usage_rate_df.head()
