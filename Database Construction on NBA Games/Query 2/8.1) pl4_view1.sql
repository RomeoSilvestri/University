SELECT * 
FROM view1
WHERE AVERAGE_PTS=(SELECT MAX(AVERAGE_PTS) FROM view1)