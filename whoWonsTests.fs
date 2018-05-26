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

[<Fact>]
let ``3 cells of the same player in distance of 1 in diagonal should return that player as winner`` () = 
     let cell = {X=3;Y=5;Value=Some Nought}
     let arg = [1 .. 3] |> List.map (fun y->{cell with Y=cell.Y+y})
     whoWons arg|> should equal (Some Nought)



[<Fact>]
let ``More than 3 cells on one line but not 3 consecutive shouldn't have a winner`` () = 
    let cell = {X=3;Y=5;Value=Some Nought}
    let arg1 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y})
    let arg2 = [1 .. 2] |> List.map (fun y->{cell with Y=cell.Y+y+5})
    whoWons (arg1 @ arg2)|> should equal None


///x|o|x|
///------
///o|o|x|
///------
///o| | |
///------
[<Fact>]
let ``last position in the game`` () = 
    let position = seq{
        yield {X = 2; Y = 0; Value = Some Nought;}
        yield {X = 2; Y = 1; Value = None;}
        yield {X = 2; Y = 2; Value = None;}
        yield {X = 1; Y = 0; Value = Some Nought;}
        yield {X = 1; Y = 1;  Value = Some Nought;}
        yield {X = 1; Y = 2; Value = Some Cross;}
        yield {X = 0; Y = 0;  Value = Some Cross;}
        yield {X = 0; Y = 1; Value = Some Nought;}
        yield {X = 0; Y = 2;  Value = Some Cross;}
        
    }
    whoWons position |> should equal None


///x|o|x|
///------
/// |o| |
///------
/// | | |
///------
[<Fact>]
let ``last position in the game2`` () = 
    let position = seq{
        yield {X = 2; Y = 0; Value = None;}
        yield {X = 2; Y = 1; Value = None;}
        yield {X = 2; Y = 2; Value = None;}
        yield {X = 1; Y = 0; Value = None;}
        yield {X = 1; Y = 1;  Value = Some Nought;}
        yield {X = 1; Y = 2; Value = None;}
        yield {X = 0; Y = 0;  Value = Some Cross;}
        yield {X = 0; Y = 1; Value = Some Nought;}
        yield {X = 0; Y = 2;  Value = Some Cross;}
        
    }
    whoWons position |> should equal None

[<Fact>]
let ``Empty 3x3 board should have no winner`` () = 
    let emptyPosition = rootPosition {X=3;Y=3}
    whoWons emptyPosition |> should equal None
