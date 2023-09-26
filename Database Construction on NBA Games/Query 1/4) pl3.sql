SELECT PLAYER_NAME, gd.PLAYER_ID, COUNT(*) AS N_MATCHES, SUM(PTS) AS SUM_PTS, ROUND(AVG(PTS),2) AS AVERAGE_PTS 
FROM games_details gd LEFT JOIN (SELECT * FROM players GROUP BY PLAYER_ID) p ON gd.PLAYER_ID=p.PLAYER_ID
GROUP BY gd.PLAYER_ID
HAVING N_MATCHES>300