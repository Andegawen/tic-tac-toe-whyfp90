module MovesTests

open Xunit
open FsUnit.Xunit
open TicTacToe


[<Fact>]
let ``empty cells don't generate moves`` () =    
    moves [] |> Seq.length |> should equal 0


[<Fact>]
let ``one empty cell is fullfilled and exchanged with nought`` () =    
    let positionWithEmptyCell = seq{yield {X=0;Y=0;Value=None}}

    let generatedCells = moves positionWithEmptyCell

    generatedCells |> Seq.length |> should equal 1
    let condition = (generatedCells|> Seq.head |> Seq.head) = {X=0;Y=0;Value=Some Nought}
    condition |> should equal true

[<Fact>]
let ``two cells one nought one empty should be fullfilled with cross`` () =    
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=0;Value=Some Nought}
                                    yield {X=0;Y=1;Value=None}
                                    }

    let generatedCells = moves positionWithEmptyCell

    generatedCells |> Seq.length |> should equal 1
    let condition = (generatedCells|> Seq.head |> Seq.head) = {X=0;Y=1;Value=Some Cross}
    condition |> should equal true

[<Fact>]
let ``two cells one cross one empty should be fullfilled with cross`` () =    
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=0;Value=Some Cross}
                                    yield {X=0;Y=1;Value=None}
                                    }

    let generatedCells = moves positionWithEmptyCell

    generatedCells |> Seq.length |> should equal 1
    let condition = (generatedCells|> Seq.head |> Seq.head) = {X=0;Y=1;Value=Some Nought}
    condition |> should equal true