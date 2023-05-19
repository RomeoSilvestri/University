db.games_details.aggregate([
    {$group:{ _id:"$PLAYER_NAME",AVERAGE_PTS:{$avg:"$PTS"}}},
    {$sort:{AVERAGE_PTS:-1}},
    {$limit:1}
])