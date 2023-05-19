SELECT DISTINCT PLAYER_NAME
FROM players2 p, games2 g, (SELECT PLAYER_ID, gd.GAME_ID 
						  FROM games_details2 gd JOIN games2 g ON gd.GAME_ID=g.GAME_ID 
                          WHERE SEASON>=2009 AND SEASON<=2018) AS gd1
WHERE p.PLAYER_ID=gd1.PLAYER_ID AND g.GAME_ID=gd1.GAME_ID AND PTS_home-PTS_away>30