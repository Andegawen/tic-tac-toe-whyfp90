#load "TicTacToeTypes.fs"
#load "TicTacToe.fs"
open TicTacToeTypes
open TicTacToe


let rec repeat (func:(Position->seq<Position>)) a0 : seq<Position>    = 
    let a1 = func a0;
    Seq.collect (fun x-> repeat func x) a1
let z = repeat moves (rootPosition {X=3;Y=3}) |> Seq.takeWhile (fun x->x=Seq.empty)

z |> Seq.toArray |> Array.iter (fun elem -> printf "%s" (posToString elem))