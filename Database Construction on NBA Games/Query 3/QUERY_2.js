db.games_details.aggregate([
    {$match:{PTS:{$gte:60}}},
    {$lookup:{
        from: "games",
        localField:"GAME_ID",
        foreignField:"GAME_ID",
        as:"games"
    }},
    {$unwind:"$games"},
    {$project:{PLAYER_NAME:1,"SEASON":"$games.SEASON",PTS:1,_id:0}},
    {$sort:{PTS:-1}}
])