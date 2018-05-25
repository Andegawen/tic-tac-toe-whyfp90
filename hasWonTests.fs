module TicTacToeTest

open Xunit
open FsUnit.Xunit
open TicTacToeTypes
open TicTacToe


[<Fact>]
let ``For non cells returns false`` () =    
    hasWon [] |> should equal false

[<Fact>]
let ``For just 1 cell returns false`` () =    
    hasWon [{X=3; Y=4; Value=None}] |> should equal false

[<Fact>]
let ``For 2 cells returns false`` () =    
    hasWon [{X=3; Y=4; Value=None}] |> should equal false

[<Fact>]
let ``3 cells in distance of 1 in horizontal should return true`` () = 
    let cell = {X=3;Y=5;Value=None}
    let arg = [1 .. 3] |> List.map (fun x->{cell with X=cell.X+x})
    hasWon arg|> should equal true


[<Fact>]
let ``3 cells in distance of 1 in vertical should return true`` () = 
    let cell = {X=3;Y=5;Value=None}
    let arg = [1 .. 3] |> List.map (fun y->{cell with Y=cell.Y+y})
    hasWon arg|> should equal true

[<Fact>]
let ``3 cells in distance of 1 in diagonal should return true`` () = 
     let cell = {X=3;Y=5;Value=None}
     let arg = [1 .. 3] |> List.map (fun y->{cell with Y=cell.Y+y})
     hasWon arg|> should equal true



[<Fact>]
let ``More than 3 cells on one line but not 3 consecutive ones should return false`` () = 
    let cell = {X=3;Y=5;Value=None}
    let arg1 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y})
    let arg2 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y+5})
    hasWon (arg1 @ arg2)|> should equal false



[<Fact>]
let ``Empty 3x3 board should have no winner`` () = 
    let emptyPosition = rootPosition {X=3;Y=3}
    anyWinner (emptyPosition|>Seq.toList) |> should equal false
