module Game

open System
open Domain


let cellWithMoves (position:Position) = position |> Seq.filter (fun p->p.Value <> None)

let whoseTurn position =     
            let notEmptyCells = cellWithMoves position
            let noughts = notEmptyCells |> Seq.sumBy (fun c-> if c.Value = Some Nought then 1 else 0)
            let crosses = notEmptyCells |> Seq.sumBy (fun c-> if c.Value = Some Cross then 1 else 0)
            if noughts <= crosses then Nought else Cross
let moves (position:Position) : seq<Position> = 
    let emptyCells = position |> Seq.filter (fun c->c.Value = None)
    let nonEmptyCells = position |> Seq.filter (fun c->c.Value <> None)
    let player = whoseTurn position;
    emptyCells |> Seq.map (fun c->
                        seq{
                            yield {c with Value=Some player}
                            yield! emptyCells |> Seq.filter (fun cc->cc<>c)
                            yield! nonEmptyCells
                            })

let whoWons (position:Position) : Player option  =  
    let hasWon (cells:seq<Cell>) : bool = 
        let distance c1 c2 : int=Convert.ToInt32((Math.Sqrt(Math.Pow(float (c2.X-c1.X), 2.0) + Math.Pow(float (c2.Y-c1.Y), 2.0))))
        
        let rec maxContinuesDistanceOnLine' cellsOnLine (acc:seq<int>) = 
            match (cellsOnLine,acc) with
            | (a,acc) when a |> Seq.length >=2 ->
                                let c1 = a |> Seq.head
                                let r = a |> Seq.skip 1
                                let c2 = r |> Seq.head
                                let rest = r |> Seq.skip 1
                                let el = Seq.last acc
                                let tail = Seq.tail acc
                                if distance c1 c2 = 1 
                                then maxContinuesDistanceOnLine' (seq{ yield c2; yield! rest}) (seq {yield! tail; yield el+1})
                                else maxContinuesDistanceOnLine' (seq{ yield c2; yield! rest}) (seq {yield! tail; yield el; yield 0})
            | (_,_) -> acc

        let maxContinuesDistanceOnLine cellsOnLine : int seq =
            maxContinuesDistanceOnLine' cellsOnLine [0]
        
        let safeSeqMax s = match s with
                            | x when Seq.isEmpty x -> 0
                            | a -> Seq.max a

        let maxLineConsecutiveMarks lineFunc cells = 
            cells 
            |> Seq.groupBy lineFunc
            |> Seq.map (snd >> Seq.sortBy (fun c->(c.X,c.Y)) >> maxContinuesDistanceOnLine >> safeSeqMax)
            |> safeSeqMax
        
        let verticals = cells |> maxLineConsecutiveMarks (fun c->c.X)
        let horizontals = cells |> maxLineConsecutiveMarks (fun c->c.Y)
        //a=1; -> so not important b=y-x
        let diagonals1 = cells |> maxLineConsecutiveMarks (fun c->c.Y-c.X)
        let diagonals2 = cells |> maxLineConsecutiveMarks (fun c->c.Y+c.X)
        let m = seq [verticals;horizontals;diagonals1;diagonals2] |> safeSeqMax
        m=2 // 3 points (x) create distance (-) of 2 x - x - x
    let noughtWin = position |> Seq.filter (fun x->x.Value=Some Nought) |> hasWon
    let crossWin = position |> Seq.filter (fun x->x.Value=Some Cross) |> hasWon
    match (noughtWin,crossWin) with
    | true, true -> failwith "not possible combinantion"
    | true, false -> Some Nought
    | false, true -> Some Cross
    | false, false -> None

///the simplest 1 - won; -1: lost ; 0 - nothing ---> assume that always 3 consecutive marks means win
let simpleStaticEval (player:Player) (position:Position) =
    match (whoWons position) with
    | None -> 0
    | Some x -> if x=player then 1 else -1


let anyWinner position =
    (whoWons position) <> None

let rec reptree (f:Position->seq<_>) (position:Position) : Node<_> = 
    match (anyWinner position) with
    | true -> {value=position;sub=Seq.empty}
    | false -> {value=position;sub=(f position |> Seq.map (fun p-> reptree f p))}


let gametree position = reptree moves position

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

let evaluate position (player:Player) =
    position
    |> gametree
    |> prune 3
    |> maptree (simpleStaticEval player)
    |> maximize

///Create all possible moves and select the best one
let makeMove position : Position option = 
    if anyWinner position then None else
            (moves position)
            |> Seq.map (fun p -> ((evaluate p (whoseTurn position)).value, p)) 
            |> Seq.maxBy fst |> snd |> Some