from nba_api.stats.endpoints import boxscoretraditionalv2, boxscoreusagev2, leaguegamefinder
from nba_api.stats.static import  teams
import time
import pandas as pd
import datetime
pd.options.mode.chained_assignment = None

todays_date = datetime.date.today().strftime('%m/%d/%Y')
yesterdays_date = (datetime.date.today() - datetime.timedelta(days=1)).strftime('%m/%d/%Y')

#todays_date = (datetime.date.today() - datetime.timedelta(days=1)).strftime('%m/%d/%Y')
#yesterdays_date = (datetime.date.today() - datetime.timedelta(days=2)).strftime('%m/%d/%Y')
def get_game_ids(today, yesterday):
  previous_games = leaguegamefinder.LeagueGameFinder(league_id_nullable = '00', date_to_nullable = today, date_from_nullable = yesterday).get_data_frames()[0]
  game_ids = previous_games['GAME_ID'].sort_values().unique()
  game_date = previous_games['GAME_DATE'].sort_values().unique()
  return [game_ids, game_date]

previous_day_games = get_game_ids(todays_date, yesterdays_date)

usage_rate = []
for game in previous_day_games[0]:
  game_info = boxscoreusagev2.BoxScoreUsageV2(game_id = game).get_data_frames()[0]
  game_usage = game_info[['GAME_ID', 'TEAM_ID','TEAM_ABBREVIATION', 'PLAYER_ID', 'PLAYER_NAME', 'USG_PCT']]
  game_usage['GAME_DATE'] = previous_day_games[1][0]
  usage_rate.append(game_usage)
  

usage_rate_df = pd.concat(usage_rate)

season_data = pd.read_csv('usage_rates.csv')
all_season_data = pd.concat([usage_rate_df, season_data])
all_season_data.to_csv('usage_rates.csv', index = False)
