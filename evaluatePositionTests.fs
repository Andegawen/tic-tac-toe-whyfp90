module EvaluatePositionTests

open Xunit
open FsUnit.Xunit
open TicTacToeTypes
open TicTacToe


[<Fact>]
let ``empty 3x3 position evaluates to 0`` () =    
    let node = evaluate (rootPosition {X=3; Y=3})
    node.value |> should equal 0



/// x|o| |
/// x|o| |
///  | | |
[<Fact>]
let ``Win evals as 1`` () = 
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=0;Value=Some Cross} 
                                    yield {X=1;Y=0;Value=Some Nought}
                                    yield {X=2;Y=0;Value=None}
                                    yield {X=0;Y=1;Value=Some Cross}
                                    yield {X=1;Y=1;Value=Some Nought}
                                    yield {X=2;Y=1;Value=None}
                                    yield {X=0;Y=2;Value=None}
                                    yield {X=1;Y=2;Value=None}
                                    yield {X=2;Y=2;Value=None}
                                    }
    (evaluate positionWithEmptyCell).value |> should equal 1
