db.games_details.mapReduce(
    function(){emit(this.PLAYER_NAME,this.FG3M); },
    function(key,values){return Array.sum(values); },
    {
        query:{PTS:{$gt:30}},
        out:"3SHOTS"
    }
)