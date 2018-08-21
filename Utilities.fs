module Utilities

open Domain
open System

let strToPlayer str =
    match str with
    | "X" 
    | "x" -> Some Cross
    | "O" 
    | "o" -> Some Nought
    | _ -> None

let toSeq (str:String) = 
    let rows = str.Split('\n')
    rows 
    |> Seq.mapi (fun y r-> let values = r.Split('|')
                           (y, values |> Array.mapi (fun x el -> (x, strToPlayer el) )))
    |> Seq.collect (fun (y,z)-> z|> Seq.map (fun (x,p)->{X=x;Y=y;Value=p}))