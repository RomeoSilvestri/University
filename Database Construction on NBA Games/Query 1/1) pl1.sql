SELECT PLAYER_NAME, g.SEASON, PTS
FROM players p, games g, games_details gd
WHERE p.PLAYER_ID=gd.PLAYER_ID AND g.GAME_ID=gd.GAME_ID AND PTS>=60
GROUP BY gd.PLAYER_ID, gd.GAME_ID
ORDER BY PTS DESC