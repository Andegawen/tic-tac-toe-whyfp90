#load "TicTacToeTypes.fs"
#load "TicTacToe.fs"
open TicTacToeTypes
open TicTacToe


let evaluate2 position (player:Player) =
    position
    |> gametree
    |> prune 3

let interestingPos = seq[
                            {X=0;Y=0;Value=Some Nought} 
                            {X=0;Y=1;Value=Some Cross}
                            {X=0;Y=2;Value=Some Nought}
                            {X=1;Y=0;Value=None}
                            {X=1;Y=1;Value=None}
                            {X=1;Y=2;Value=None}
                            {X=2;Y=0;Value=None}
                            {X=2;Y=1;Value=None}
                            {X=2;Y=2;Value=None}          
                        ]
interestingPos |> posToString
let evalPos = evaluate2 interestingPos Cross;
let evalPosValue = evalPos.value
let ``evaluate interesting position sub x=1 y=1 Cross`` =
    evalPos.sub 
    |> Seq.filter (fun x->
                        let p = x.value
                        p|>Seq.exists (fun c->c.X=1 && c.Y=1 && c.Value = Some Cross))
    |> Seq.head

let ``evaluate interesting position sub x=2 y=1 Empty`` =
    ``evaluate interesting position sub x=1 y=1 Cross``.sub 
    |> Seq.filter (fun x->
                        let p = x.value
                        p|>Seq.exists (fun c->c.X=2 && c.Y=1 && c.Value = None))
    |> Seq.head

let ``evaluate interesting position sub x=2 y=1 Cross`` = 
    ``evaluate interesting position sub x=2 y=1 Empty``.sub 
    |> Seq.filter (fun x->
                        let p = x.value
                        p|>Seq.exists (fun c->c.X=2 && c.Y=1 && c.Value = Some Cross))
    |> Seq.head

``evaluate interesting position sub x=2 y=1 Cross``.value |> posToString
maptree (simpleStaticEval Cross) ``evaluate interesting position sub x=2 y=1 Cross``
//let kiki1 = kiki.sub |> Seq.filter (fun x->x.value<>0) |> Seq.toArray