module WhoWonsTests

open Xunit
open FsUnit.Xunit
open TicTacToeTypes
open TicTacToe


[<Fact>]
let ``For non cells nobody wons`` () =    
    whoWons [] |> should equal None

[<Fact>]
let ``For just 1 cell nobody wons`` () =    
    whoWons [{X=3; Y=4; Value=Some Nought}] |> should equal None

[<Fact>]
let ``For 2 cells nobody wons`` () =    
    whoWons (seq { yield {X=3; Y=4; Value=Some Nought}; yield {X=3; Y=5; Value=Some Nought} }) |> should equal None

[<Fact>]
let ``3 cells of the same player in distance of 1 in horizontal should return that player as winner`` () = 
    let cell = {X=3;Y=5;Value=Some Nought}
    let arg = [1 .. 3] |> List.map (fun x->{cell with X=cell.X+x})
    whoWons arg|> should equal (Some Nought)


[<Fact>]
let ``3 cells of the same player in distance of 1 in vertical should return that player as winner`` () = 
    let cell = {X=3;Y=5;Value=Some Nought}
    let arg = [1 .. 3] |> List.map (fun y->{cell with Y=cell.Y+y})
    whoWons arg|> should equal (Some Nought)


let diagonalPositions : obj array seq= 
    seq [
          // | |x|
          //------
          // |x| |
          //------
          //x| | |
          //------
          [|seq[
                {X = 2; Y = 0; Value = None;};
                {X = 2; Y = 1; Value = None;}
                {X = 2; Y = 2; Value = Some Cross;}
                {X = 1; Y = 0; Value = None;}
                {X = 1; Y = 1;  Value = Some Cross;}
                {X = 1; Y = 2; Value = None;}
                {X = 0; Y = 0;  Value = Some Cross;}
                {X = 0; Y = 1; Value = None;}
                {X = 0; Y = 2;  Value = None ;}]|]
          //x| | |
          //------
          // |x| |
          //------
          // | |x|
          //------
          [|seq[
                {X = 2; Y = 0; Value = Some Cross;}
                {X = 2; Y = 1; Value = None;}
                {X = 2; Y = 2; Value = None;}
                {X = 1; Y = 0; Value = None;}
                {X = 1; Y = 1;  Value = Some Cross;}
                {X = 1; Y = 2; Value = None;}
                {X = 0; Y = 0;  Value = None;}
                {X = 0; Y = 1; Value = None;}
                {X = 0; Y = 2;  Value = Some Cross;}]|]
    ]

[<Theory; MemberData("diagonalPositions")>]
let ``diagonals filled with the same mark should return that player as winner`` (position : Position) = 
    whoWons position|> should equal (Some Cross)


[<Fact>]
let ``More than 3 cells on one line but not 3 consecutive shouldn't have a winner`` () = 
    let cell = {X=3;Y=5;Value=Some Nought}
    let arg1 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y})
    let arg2 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y+5})
    whoWons (arg1 @ arg2)|> should equal None


[<Fact>]
let ``Empty 3x3 board should have no winner`` () = 
    let emptyPosition = rootPosition {X=3;Y=3}
    whoWons emptyPosition |> should equal None
