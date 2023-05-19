db.games.aggregate([
    {$project:{GAME_ID:1,SEASON:1,"DIFF":{$subtract:["$PTS_home","$PTS_away"]}}},
    {$match:{$and:[{SEASON:2021},{DIFF:{$gt:30}}]}},
    {$lookup:{
        from:"games_details",
        localField:"GAME_ID",
        foreignField:"GAME_ID",
        as:"gd"
    }},
    {$unwind:"$gd"},
    {$project:{"PLAYER_NAME":"$gd.PLAYER_NAME"}},
    {$group:{ _id:"$PLAYER_NAME"}}
])