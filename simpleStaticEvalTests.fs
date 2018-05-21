module SimpleStaticEvalTests

open Xunit
open FsUnit.Xunit
open TicTacToe
open TicTacToeTypes


[<Fact>]
let ``Position with one empty evals to 0`` = 
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=3;Value=None}
                                    }
    simpleStaticEval positionWithEmptyCell |> should equal 0

[<Fact>]
let ``Position with one empty evals to 1`` = 
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=3;Value=Some Cross}
                                    }
    simpleStaticEval positionWithEmptyCell |> should equal 1

[<Fact>]
let ``Position with one empty evals to -1`` = 
    let positionWithEmptyCell = seq{
                                    yield {X=0;Y=3;Value=Some Nought}
                                    }
    simpleStaticEval positionWithEmptyCell |> should equal -1