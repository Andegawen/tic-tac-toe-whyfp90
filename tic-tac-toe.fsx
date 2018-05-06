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

let whoseTurn (position:Position) : Player = 
    let cellWithMoves = position |> Seq.filter (fun p->p.Value <> None) |> Seq.toList
    let noughts = cellWithMoves |> List.countBy (fun c->c.Value = Some Nought)
    let crosses = cellWithMoves |> List.countBy (fun c->c.Value = Some Cross)
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

//the simplest 1 - won; -1: lost ; 0 - nothing
// if higher than better
let staticEval (position:Position) : int = 
    failwith "How to compute who win or loose?!"
    //3/4/5 ? line

//Node<Position> -> Node<int> where int is computed from staticEval
//maptree f= foldtree (Node. f) Cons Nil
let rec maptree (func:('a->'b)) (tree:Node<'a>) : (Node<'b>) = 
    {value=(func tree.value); sub=tree.sub |> Seq.map (maptree func)}



maptree staticEval (gametree (rootPosition {X=3;Y=3}))

// let maximize (Node n sub) = max (map minimize sub)
// let minimize (Node n sub) = min (map maximize sub)
// let evaluate = maximize 

