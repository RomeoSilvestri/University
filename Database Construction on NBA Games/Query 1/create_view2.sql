CREATE VIEW view2(PLAYER_ID,AVERAGE_PTS) AS
	   SELECT PLAYER_ID, ROUND(AVG(PTS),3)
       FROM games_details
       GROUP BY PLAYER_ID