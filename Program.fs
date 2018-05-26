open TicTacToeTypes
open TicTacToe

let rec repeat (func:('a->'a option)) (a0:'a) : seq<'a>   = 
    let a1 = func a0;
    match a1 with
    | None -> Seq.empty
    | Some x -> seq {
                        yield x
                        yield! repeat func x 
                    }

let game position = repeat makeMove position

module Program = let [<EntryPoint>] main _ =
    let position = seq {
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
    game position|> Seq.iter (posToString >> printfn "%s\n\n")
    // game x|> Seq.iter (fun p->printfn "seq"; Seq.iter (fun c -> printfn "%A" c) p)
    printfn "game ended"
    0