CREATE VIEW view1(PLAYER_NAME,AVERAGE_PTS) AS
	   SELECT PLAYER_NAME, ROUND(AVG(PTS),3)
       FROM players2 p JOIN games_details2 gd ON p.PLAYER_ID=gd.PLAYER_ID
       GROUP BY p.PLAYER_NAME