db.games_details.aggregate([
    {$match:{PTS:{$gt:30}}},
    {$group:{ _id:"$PLAYER_NAME",SHOTS3:{$sum:"$FG3M"}}}
])