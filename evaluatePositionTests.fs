module EvaluatePositionTests

open Xunit
open FsUnit.Xunit
open Domain
open Game
open Utilities

[<Fact>]
let ``empty 3x3 position evaluates to 0`` () =
    let node = evaluate (rootPosition {X=3; Y=3}) Nought
    node.value |> should equal 0



[<Fact>]
let ``Win evals as 1`` () = 
    let positionWithEmptyCell = "X|O| \n"
                              + "X|O| \n"
                              + " |O| " |> strBoardToSeq
    printfn " lubie misie2 %A" (positionWithEmptyCell |> Seq.toList)
    (evaluate positionWithEmptyCell Nought).value |> should equal 1


[<Fact>]
let ``The lost evals as -1`` () = 
    let positionWithEmptyCell = "x|o|o\n"
                              + "x|o| \n"
                              + "x| | " |> strBoardToSeq
    (evaluate positionWithEmptyCell Nought).value |> should equal -1


let evaluations : obj array seq= 
    seq [
/// o|x|o|
/// x|o| |
///  | | |
          [|-1;seq[
                {X=0;Y=0;Value=Some Nought} 
                {X=0;Y=1;Value=Some Cross}
                {X=0;Y=2;Value=Some Nought}
                {X=1;Y=0;Value=Some Cross}
                {X=1;Y=1;Value=Some Nought}
                {X=1;Y=2;Value=None}
                {X=2;Y=0;Value=None}
                {X=2;Y=1;Value=None}
                {X=2;Y=2;Value=None}          
               ]|]
/// o|x|o|
/// x| | |
///  | | |
          [|1;seq[
                {X=0;Y=0;Value=Some Nought} 
                {X=0;Y=1;Value=Some Cross}
                {X=0;Y=2;Value=Some Nought}
                {X=1;Y=0;Value=Some Cross}
                {X=1;Y=1;Value=None}
                {X=1;Y=2;Value=None}
                {X=2;Y=0;Value=None}
                {X=2;Y=1;Value=None}
                {X=2;Y=2;Value=None}          
               ]|]
/// o|x|o|
///  | | |
///  | | |
          [|1;seq[
                {X=0;Y=0;Value=Some Nought} 
                {X=0;Y=1;Value=Some Cross}
                {X=0;Y=2;Value=Some Nought}
                {X=1;Y=0;Value=None}
                {X=1;Y=1;Value=None}
                {X=1;Y=2;Value=None}
                {X=2;Y=0;Value=None}
                {X=2;Y=1;Value=None}
                {X=2;Y=2;Value=None}          
               ]|]
    ]

[<Theory; MemberData("evaluations")>]
let ``Interesting evaluations`` (expected : int, position : Position) = 
    (evaluate position (whoseTurn position)).value |> should equal expected
