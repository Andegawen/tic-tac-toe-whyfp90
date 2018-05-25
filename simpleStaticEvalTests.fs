module SimpleStaticEvalTests

open Xunit
open FsUnit.Xunit
open TicTacToeTypes
open TicTacToe


[<Fact>]
let ``Position with no win or no lost evals to 0`` () =    
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=3;Value=None}
                                    }
    simpleStaticEval Nought positionWithEmptyCell |> should equal 0

/// x|o| |
/// x|o| |
///  |o| |
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
                                    yield {X=1;Y=2;Value=Some Nought}
                                    yield {X=2;Y=2;Value=None}
                                    }
    simpleStaticEval Nought positionWithEmptyCell |> should equal 1

/// x|o|o|
/// x|o| |
/// x| | |
[<Fact>]
let ``Lost evals to -1`` ()= 
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=0;Value=Some Cross} 
                                    yield {X=1;Y=0;Value=Some Nought}
                                    yield {X=2;Y=0;Value=Some Nought}
                                    yield {X=0;Y=1;Value=Some Cross}
                                    yield {X=1;Y=1;Value=Some Nought}
                                    yield {X=2;Y=1;Value=None}
                                    yield {X=0;Y=2;Value=Some Cross}
                                    yield {X=1;Y=2;Value=None}
                                    yield {X=2;Y=2;Value=None}
                                    }
    simpleStaticEval Nought positionWithEmptyCell |> should equal -1
