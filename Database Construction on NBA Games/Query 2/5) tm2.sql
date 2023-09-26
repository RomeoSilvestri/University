SELECT CITY, NICKNAME 
FROM teams2 t
WHERE TEAM_ID IN (SELECT TEAM_ID
				  FROM players2
				  WHERE PLAYER_ID IN (SELECT PLAYER_ID
									  FROM games_details2
									  WHERE PTS>=60))
ORDER BY CITY