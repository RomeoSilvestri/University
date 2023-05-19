db.games_details.aggregate([
    {$group:{ _id:"$PLAYER_NAME",N_MATCHES:{$count:{}},SUM_PTS:{$sum:"$PTS"},AVERAGE_PTS:{$avg:"$PTS"}}},
    {$match:{N_MATCHES:{$gt:300}}},
])