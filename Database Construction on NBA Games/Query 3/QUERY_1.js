db.games_details.find({PTS:{$gte:60}},
                      {PLAYER_NAME:1,PTS:1,_id:0},
).sort({PTS:-1})