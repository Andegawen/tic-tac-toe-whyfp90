module SimpleStaticEvalTests

open Xunit
open FsUnit.Xunit
open Domain
open Game
open Utilities
    
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
                                + " |O| " |> strBoardToSeq
    
    simpleStaticEval Nought positionWithEmptyCell |> should equal 1

[<Fact>]
let ``Lost evals to -1`` ()= 
    let positionWithEmptyCell = "X|O|O\n"
                              + "X|O| \n"
                              + "X| | " |> strBoardToSeq
     
    simpleStaticEval Nought positionWithEmptyCell |> should equal -1
