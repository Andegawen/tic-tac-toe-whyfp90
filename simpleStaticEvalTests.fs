module SimpleStaticEvalTests

open Xunit
open FsUnit.Xunit
open Domain
open Game
open System


let strToPlayer str =
    match str with
    | "X" 
    | "x" -> Some Cross
    | "O" -> Some Nought
    | "o" -> Some Nought
    | _ -> None

let toSeq (str:String) = 
    let rows = str.Split('\n')
    rows 
    |> Seq.mapi (fun y r-> let values = r.Split('|')
                           (y, values |> Array.mapi (fun x el -> (x, strToPlayer el) )))
    |> Seq.collect (fun (y,z)-> z|> Seq.map (fun (x,p)->{X=x;Y=y;Value=p}))
    
[<Fact>]
let ``Position with no win or no lost evals to 0`` () =    
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=3;Value=None}
                                    }
    simpleStaticEval Nought positionWithEmptyCell |> should equal 0


[<Fact>]
let ``Win evals as 1`` () = 
    let positionWithEmptyCell =   "X|O| \n"
                                + "X|O| \n"
                                + " |O| " |> toSeq
    
    simpleStaticEval Nought positionWithEmptyCell |> should equal 1

[<Fact>]
let ``Lost evals to -1`` ()= 
    let positionWithEmptyCell = "X|O|O\n"
                              + "X|O| \n"
                              + "X| | " |> toSeq
     
    simpleStaticEval Nought positionWithEmptyCell |> should equal -1
