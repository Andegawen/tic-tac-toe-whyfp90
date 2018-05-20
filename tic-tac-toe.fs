module TicTacToe

open System

type Board = {X:int; Y:int}
type Player = Nought|Cross
type Cell = {X:int; Y:int; Value:Player option}
type Position = seq<Cell>
type Node<'a> = {value:'a; sub:seq<Node<'a>>}

let rootPosition (board:Board) : Position = 
    let cells = seq{ for x in 0 .. board.X-1 do
                        for y in 0 .. board.Y-1 do
                            yield {X=x; Y=y; Value=None}}
    cells

let cellWithMoves (position:Position) = position |> Seq.filter (fun p->p.Value <> None) |> Seq.toList

let moves (position:Position) : seq<Position> = 
    let whoseTurn position =     
            let notEmptyCells = cellWithMoves position
            let noughts = notEmptyCells |> List.countBy (fun c->c.Value = Some Nought)
            let crosses = notEmptyCells |> List.countBy (fun c->c.Value = Some Cross)
            if noughts <= crosses then Nought else Cross

    let emptyCells = position |> Seq.filter (fun c->c.Value = None)
    let nonEmptyCells = position |> Seq.filter (fun c->c.Value <> None)
    let player = whoseTurn position;
    emptyCells |> Seq.map (fun c->
                        seq{
                            yield {c with Value=Some player}
                            yield! emptyCells |> Seq.filter (fun cc->cc<>c)
                            yield! nonEmptyCells
                            }) 
    

    
let rec reptree (f:Position->seq<_>) (position:Position) : Node<_> = 
    {value=position;sub=(f position |> Seq.map (fun p-> reptree f p))}


let gametree p = reptree moves p


let hasWon (cells:list<Cell>) : bool = 
    let distance c1 c2 : int=Convert.ToInt32((Math.Sqrt(Math.Pow(float (c2.X-c1.X), 2.0) + Math.Pow(float (c2.Y-c1.Y), 2.0))))
    
    let rec maxContinuesDistanceOnLine' cellsOnLine (acc:list<int>) = 
        match (cellsOnLine,acc) with
        | ([_],_) -> acc
        | (c1::c2::rest,acc) -> let el = List.last acc
                                let tail = acc.[.. acc.Length-1]
                                if distance c1 c2 = 1 
                                then maxContinuesDistanceOnLine' (c2::rest) (tail @ [el+1])
                                else maxContinuesDistanceOnLine' (c2::rest) (tail @ [el; 0])

    let maxContinuesDistanceOnLine cellsOnLine : int list =
        maxContinuesDistanceOnLine' cellsOnLine [0]
    
    let safeListMax = function
        | [] -> 0
        | a -> List.max a

    let maxLineConsecutiveMarks groupLine cells = 
        cells 
        |> List.groupBy groupLine
        |> List.map (snd >> maxContinuesDistanceOnLine >> safeListMax)
        |> safeListMax
        
    
    let verticals = cells |> maxLineConsecutiveMarks (fun c->c.X)
    let horizontals = cells |> maxLineConsecutiveMarks (fun c->c.Y)
    //a=1; -> so not important b=y-x
    let diagonals = cells |> maxLineConsecutiveMarks (fun c->c.Y-c.X)
    let m = [verticals;horizontals;diagonals] |> List.max
    m=2 // 3 points (x) create distance (-) of 2 x - x - x

///the simplest 1 - won; -1: lost ; 0 - nothing ---> assume that always 3 consecutive marks means win
let staticEval (position:Position) = 
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
let rec maptree (func:('a->'b)) (tree: 'a Node) = 
    {value=(func tree.value); sub=tree.sub |> Seq.map (maptree func)}
 

let rec maximize = function
    | node when Seq.isEmpty node.sub -> node
    | node -> node.sub |> Seq.map minimize |> Seq.maxBy (fun x->x.value)
and minimize = function
    | node when Seq.isEmpty node.sub -> node
    | node -> node.sub |> Seq.map maximize |> Seq.minBy (fun x->x.value)

let rec prune (limit:int) (node:'a Node) =
    match limit with
    | 0 -> {node with sub=Seq.empty}
    | l -> {node with sub=node.sub |> Seq.map (fun x-> prune (l-1) x)}

let evaluate position =
    position  
    |> gametree 
    |> prune 5
    |> maptree staticEval
    |> maximize

let makeMove position = (moves position) |> Seq.map (fun p -> ((evaluate p).value, p)) |> Seq.maxBy fst |> snd

let rec repeat (func:('a->'a)) (a0:'a) : seq<'a>   = 
    let a1 = func a0;
    seq {
        yield a1
        yield! repeat func a1 
     }

let posToString pos = 
    let lines = (pos |> Seq.sortBy (fun c->c.X) |> Seq.groupBy (fun c->c.X))
                |> Seq.map (snd
                    >> (fun c -> c 
                                |> Seq.sortBy (fun x->x.Y) 
                                |> Seq.map (fun x-> match x.Value with
                                               | Some Nought -> "o|"
                                               | Some Cross -> "x|"
                                               | None -> " |") |> String.Concat ))
    ("\n", lines |> Seq.map (fun x -> x + "\n" + String('-', x |> Seq.length))) |> String.Join

let game = repeat makeMove (rootPosition {X=3;Y=3}) 
            |> Seq.takeWhile (fun x->staticEval x<>0)
            |> Seq.map posToString
            |> Seq.toList



    