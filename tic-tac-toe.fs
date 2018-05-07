module TicTacToe

open System

type Board = {X:int; Y:int}
type Player = Nought|Cross
type Cell = {X:int; Y:int; Value:Player option}
type Position = seq<Cell>

type Node<'a> = {value:'a; sub:seq<'a Node>}

let rootPosition (board:Board) : Position = 
    let cells = seq{ for x in 0 .. board.X-1 do
                        for y in 0 .. board.Y-1 do
                            yield {X=x; Y=y; Value=None}}
    cells

let cellWithMoves (position:Position) = position |> Seq.filter (fun p->p.Value <> None) |> Seq.toList

let whoseTurn (position:Position) : Player =     
    let notEmptyCells = cellWithMoves position
    let noughts = notEmptyCells |> List.countBy (fun c->c.Value = Some Nought)
    let crosses = notEmptyCells |> List.countBy (fun c->c.Value = Some Cross)
    if noughts <= crosses then Nought else Cross

let moves (position:Position) : seq<Position> = 
    let emptyCells = position |> Seq.filter (fun c->c.Value = None)
    let player = whoseTurn position;
    emptyCells 
    |> Seq.map (fun c->
        seq{
            yield {c with Value=Some player}
            yield! emptyCells |> Seq.filter (fun cc->cc<>c)
            })

    
let rec reptree (func:Position->seq<Position>) (position:Position) : Node<Position> = 
    {value=position;sub=(func position |> Seq.map (fun p-> reptree func p))}

let gametree p = reptree moves p


//the simplest 1 - won; -1: lost ; 0 - nothing ---> assume that always 3 consecutive marks means win
let hasWon (cells:list<Cell>) : bool = 
    printfn "%A" cells
    let distance c1 c2 : int=Convert.ToInt32((Math.Sqrt(Math.Pow(float (c2.X-c1.X), 2.0) + Math.Pow(float (c2.Y-c1.Y), 2.0))))
    
    let rec maxContinuesDistanceOnLine' cellsOnLine (acc:list<int>) = 
        match (cellsOnLine,acc) with
        | ([_],_) -> 
                    printfn "acc: %A" acc
                    acc
        | (c1::c2::rest,acc) -> let el = List.last acc
                                let tail = acc.[.. acc.Length-1]
                                if distance c1 c2 = 1 
                                then maxContinuesDistanceOnLine' (c2::rest) (tail @ [el+1])
                                else maxContinuesDistanceOnLine' (c2::rest) (tail @ [el; 0])

    let maxContinuesDistanceOnLine cellsOnLine : list<int> =
        maxContinuesDistanceOnLine' cellsOnLine [0]
    
    let safeListMax list = 
        match list with
        | [] -> 0
        | a -> a |> List.max

    let maxLineConsecutiveMarks groupLine cells = 
        cells 
        |> List.groupBy groupLine
        |> List.map (snd >> maxContinuesDistanceOnLine >> safeListMax)
        |> safeListMax
        
    
    let verticals = cells |> maxLineConsecutiveMarks (fun c->c.X)
    printfn "%A" verticals
    let horizontals = cells |> maxLineConsecutiveMarks (fun c->c.Y)
    printfn "%A" horizontals
    //a=1; -> so not important b=y-x
    let diagonals = cells |> maxLineConsecutiveMarks (fun c->c.Y-c.X)
    printfn "%A" diagonals
    let m = [verticals;horizontals;diagonals] |> List.max
    m=2 // 3 points create 2 distances x - x - x

let staticEval (position:Position) : int = 
    let notEmptyCells = cellWithMoves position
    let hasNoughtsWon = notEmptyCells |> List.filter (fun c->c.Value = Some Nought) |> hasWon
    let hasCrossesWon = notEmptyCells |> List.filter (fun c->c.Value = Some Cross) |> hasWon
    match (hasNoughtsWon, hasCrossesWon) with
    | (false,false) -> 0
    | (true, false) -> 1
    | (false, true) -> -1
    | (true, true) -> failwith "Not possible combination when nought and crosses win at the same time"

//Node<Position> -> Node<int> where int is computed from staticEval
//maptree f= foldtree (Node. f) Cons Nil
let rec maptree (func:('a->'b)) (tree:Node<'a>) : (Node<'b>) = 
    {value=(func tree.value); sub=tree.sub |> Seq.map (maptree func)}



maptree staticEval (gametree (rootPosition {X=3;Y=3}))

// let maximize (Node n sub) = max (map minimize sub)
// let minimize (Node n sub) = min (map maximize sub)
// let evaluate = maximize 

