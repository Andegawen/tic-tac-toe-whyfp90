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

let gameResultString position = match (whoWons position) with
                                | None -> "None"
                                | Some x -> sprintf "The winner is %A" x

module Program = let [<EntryPoint>] main _ =
    let initialPosition = [
                            {X=0;Y=0;Value=None} 
                            {X=0;Y=1;Value=None}
                            {X=0;Y=2;Value=None}
                            {X=1;Y=0;Value=None}
                            {X=1;Y=1;Value=Some Nought}
                            {X=1;Y=2;Value=None}
                            {X=2;Y=0;Value=None}
                            {X=2;Y=1;Value=None}
                            {X=2;Y=2;Value=None}
                          ]
    game initialPosition |> Seq.iter (fun x-> printfn "%s" (gameResultString x); printfn "%s\n\n" (posToString x); )
    0
    