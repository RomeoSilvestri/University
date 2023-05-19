SELECT CITY, NICKNAME
FROM teams t
WHERE EXISTS (SELECT *
              FROM ranking r
              WHERE t.TEAM_ID=r.TEAM_ID AND CONFERENCE='West')
	  AND NOT EXISTS (SELECT *
                      FROM games g
                      WHERE TEAM_ID=HOME_TEAM_ID AND PTS_home>150)