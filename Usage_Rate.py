from nba_api.stats.endpoints import boxscoretraditionalv2, boxscoreusagev2, leaguegamefinder
from nba_api.stats.static import  teams
import time
import pandas as pd

nba_teams = teams.get_teams()
jazz = [x for x in nba_teams if x['full_name'] == 'Utah Jazz'][0]['id']
jazz

jazz_games = leaguegamefinder.LeagueGameFinder(team_id_nullable=jazz).get_data_frames()[0]['GAME_ID']

info = boxscoreusagev2.BoxScoreUsageV2(game_id = jazz_games[0])
all = info.get_data_frames()[0]
all.info()
all[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
n_games = 120


yesterdays_games = leaguegamefinder.LeagueGameFinder(league_id_nullable='00', date_to_nullable='12/15/2023', date_from_nullable = '12/15/2023')
?leaguegamefinder.LeagueGameFinder()
games = yesterdays_games.get_data_frames()[0]

games.info()
game_ids = games['GAME_ID'].sort_values().unique()

def get_game_ids(today, last_day):
  previous_games = leaguegamefinder.LeagueGameFinder(league_id_nullable = '00', date_to_nullable = today, date_from_nullable = last_day).get_data_frames()[0]
  game_ids = previous_games[['GAME_ID'].sort_values().unique()
  return game_ids

test = get_game_ids('12/15/2023', '12/14/2023')
usage_rate = []
for game in game_ids:
  game_info = boxscoreusagev2.BoxScoreUsageV2(game_id = game).get_data_frames()[0]
  game_usage = game_info[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
  usage_rate.append(game_usage)
  

usage_rate_df = pd.concat(usage_rate)
usage_rate_df.head()
