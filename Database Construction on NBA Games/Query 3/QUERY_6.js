db.games_details.aggregate([
    {$match:{PTS:{$gt:60}}},
    {$lookup:{
        from:"players",
        localField:"PLAYER_ID",
        foreignField:"PLAYER_ID",
        as:"players"
    }},
    {$lookup:{
        from:"teams",
        localField:"TEAM_ID",
        foreignField:"TEAM_ID",
        as:"teams"
    }},
    {$unwind:"$teams"},
    {$group:{ _id:["$teams.CITY","$teams.NICKNAME"]}}
])