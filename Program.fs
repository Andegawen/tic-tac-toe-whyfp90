open TicTacToeTypes
open TicTacToe

let rec repeat (func:('a->'a)) (a0:'a) : seq<'a>   = 
    let a1 = func a0;
    seq {
        yield a1
        yield! repeat func a1 
     }

let game = repeat makeMove (rootPosition {X=3;Y=3}) 
                |> Seq.take 1
                |> Seq.map posToString
                |> Seq.toList
module Program = let [<EntryPoint>] main _ = 
    game |> Seq.iter (fun p-> printfn "%s" p)
    0