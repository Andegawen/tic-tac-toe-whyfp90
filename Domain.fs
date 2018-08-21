module Domain

open System

type Board = {X:int; Y:int}
type Player = Nought|Cross
type Cell = {X:int; Y:int; Value:Player option}
type Position = seq<Cell>
type Node<'a> = {value:'a; sub:seq<Node<'a>>}

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

let rootPosition (board:Board) : Position = 
    let cells = seq{ for x in 0 .. board.X-1 do
                        for y in 0 .. board.Y-1 do
                            yield {X=x; Y=y; Value=None}}
    cells