SELECT PLAYER_NAME, ROUND(AVG(PTS),3) AS AVERAGE_PTS
FROM players2 p JOIN games_details2 gd ON p.PLAYER_ID=gd.PLAYER_ID
GROUP BY p.PLAYER_ID
HAVING AVG(PTS)>=ALL(SELECT AVG(PTS)
                     FROM players2 p JOIN games_details2 gd ON p.PLAYER_ID=gd.PLAYER_ID
                     GROUP BY gd.PLAYER_ID)