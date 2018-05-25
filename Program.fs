open TicTacToeTypes
open TicTacToe

let rec repeat (func:('a->'a)) (a0:'a) : seq<'a>   = 
    let a1 = func a0;
    seq {
        yield a1
        yield! repeat func a1 
     }

let game position = repeat makeMove position 
                    |> Seq.takeWhile (fun x -> not (anyWinner x))
                    |> Seq.map posToString

module Program = let [<EntryPoint>] main _ =
    let x = seq{
        yield {X=0;Y=0;Value=None} 
        yield {X=0;Y=1;Value=None}
        yield {X=0;Y=2;Value=None}
        yield {X=1;Y=0;Value=None}
        yield {X=1;Y=1;Value=Some Nought}
        yield {X=1;Y=2;Value=None}
        yield {X=2;Y=0;Value=None}
        yield {X=2;Y=1;Value=None}
        yield {X=2;Y=2;Value=None}
    }
    game x|> Seq.iter (fun p-> printfn "%s\n" p)
    System.Console.ReadKey()
    0