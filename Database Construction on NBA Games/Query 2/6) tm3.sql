SELECT CITY, NICKNAME
FROM teams2 t
WHERE EXISTS (SELECT *
              FROM ranking2 r
              WHERE t.TEAM_ID=r.TEAM_ID AND CONFERENCE='West')
	  AND NOT EXISTS (SELECT *
                      FROM games2 g
                      WHERE TEAM_ID=HOME_TEAM_ID AND PTS_home>=150)