from nba_api.stats.endpoints import boxscoretraditionalv2, boxscoreusagev2, leaguegamefinder
from nba_api.stats.static import  teams

nba_teams = teams.get_teams()
jazz = [x for x in nba_teams if x['full_name'] == 'Utah Jazz'][0]['id']
jazz

jazz_games = leaguegamefinder.LeagueGameFinder(team_id_nullable=jazz).get_data_frames()[0]['GAME_ID']

info = boxscoreusagev2.BoxScoreUsageV2(game_id = jazz_games[0])
all = info.get_data_frames()[0]
all.info()
all[['PLAYER_NAME', 'USG_PCT', 'TEAM_ABBREVIATION']][all['TEAM_ABBREVIATION'] == 'UTA']
