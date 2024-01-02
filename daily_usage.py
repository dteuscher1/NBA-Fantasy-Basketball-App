from nba_api.stats.endpoints import boxscoretraditionalv2, boxscoreusagev2, leaguegamefinder
from nba_api.stats.static import  teams
import time
import pandas as pd
import datetime

todays_date = datetime.date.today().strftime('%m/%d/%Y')
yesterdays_date = (datetime.date.today() - datetime.timedelta(days=1)).strftime('%m/%d/%Y')

def get_game_ids(today, yesterday):
  previous_games = leaguegamefinder.LeagueGameFinder(league_id_nullable = '00', date_to_nullable = today, date_from_nullable = yesterday).get_data_frames()[0]
  game_ids = previous_games['GAME_ID'].sort_values().unique()
  return game_ids

previous_day_games = get_game_ids(todays_date, yesterdays_date)

usage_rate = []
for game in previous_day_games:
  game_info = boxscoreusagev2.BoxScoreUsageV2(game_id = game).get_data_frames()[0]
  game_usage = game_info[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
  usage_rate.append(game_usage)
  

usage_rate_df = pd.concat(usage_rate)

season_data = pd.read_csv('usage_rate.csv')
all_season_data = pd.concat([season_data, usage_rate_df])
all_season_data.to_csv('usage_rate.csv')
