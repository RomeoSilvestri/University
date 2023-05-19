SELECT NICKNAME AS OPPONENT, GAME_DATE_EST, PTS_home, PTS_away
FROM games g, teams t
WHERE (HOME_TEAM_ID, VISITOR_TEAM_ID) IN (SELECT t1.TEAM_ID, t2.TEAM_ID
                                          FROM teams t1 CROSS JOIN teams t2
                                          WHERE t1.TEAM_ID!=t2.TEAM_ID AND t1.NICKNAME='Lakers' AND HOME_TEAM_WINS=1 AND SEASON=2019)
	   AND g.VISITOR_TEAM_ID=t.TEAM_ID