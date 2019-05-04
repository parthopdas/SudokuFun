module SudokuFun

open System.Text

type Cell =
    | Digit of int
    | Candidates of Set<int>

type Board =
    | Board of Cell[,] 

let defaultCellCandidates = Set.ofSeq [ 1..9 ]

let cellIndices =
    seq { for i in 0 .. 8 do
            for j in 0 .. 8 do
                yield (i, j) }
    |> List.ofSeq

let blockIndicesMap =
    cellIndices
    |> Seq.groupBy (fun (i, j) -> (i / 3, j / 3))
    |> Map.ofSeq

///
/// To generate new puzzle:
/// 1. Go to https://qqwing.com/generate.html
/// 2. Select output format as single line
let initBoard str =
    if (String.length str <> 81) then
        failwith "Input must be a string of 81 characters"

    let b = Array2D.create 9 9 (Digit 0)

    cellIndices
    |> Seq.iter (
        fun (i, j) -> 
            let c = (int str.[i * 9 + j] - int '0')
            let s = if c >= 1 && c <= 9 then Digit c else Candidates defaultCellCandidates
            b.[i,j] <- s)
    
    b |> Board

let getSetOfHints : seq<Cell> -> Set<int> =
    Seq.choose (function | Digit d -> Some d | Candidates _ -> None)
    >> Set.ofSeq

let getBlockOccupancy (Board b) i j =
    blockIndicesMap
    |> Map.find (i, j)
    |> Seq.map (fun (i, j) -> b.[i, j])
    |> getSetOfHints

let getRowOccupancy (Board b) i =
    b.[i, *]
    |> getSetOfHints

let getColumnOccupancy (Board b) j =
    b.[*, j]
    |> getSetOfHints

let numberOfHints (Board b) =
    cellIndices
    |> Seq.choose (fun (i, j) -> match b.[i,j] with | Digit d -> Some d | Candidates _ -> None)
    |> Seq.length

/// Strategy #1: Naked single (https://www.youtube.com/watch?v=b123EURtu3I)
let applyNakedSingleStrategy (Board b) =
    let crossHatchCell (Board b) i j =
        match b.[i,j] with
        | Digit _ -> ()
        | Candidates _ -> 
            let n = getBlockOccupancy (Board b) (i / 3) (j / 3)
            let r = getRowOccupancy (Board b) i
            let c = getColumnOccupancy (Board b) j   
            let cs = Set.difference defaultCellCandidates (Set.unionMany [ n; r; c ])
            b.[i,j] <- if Seq.length cs = 1 then Digit <| Seq.exactlyOne cs else Candidates cs
        
    cellIndices
    |> Seq.iter (fun (i, j) -> crossHatchCell (Board b) i j)

let isSolved b = numberOfHints b = 81

let printBoard (Board b) =
    let getProgress (Board b) =
        cellIndices 
        |> Seq.map (fun (i, j) -> match b.[i,j] with | Digit _ -> 1 | Candidates cs -> Seq.length cs) 
        |> Seq.fold (+) 0

    let boardStr = 
        [| "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝" |]
        |> Array.map String.toCharArray

    cellIndices
    |> Seq.iter 
        (fun (i, j) ->
            match b.[i,j] with
            | Digit d ->
                boardStr.[4 * i + 2].[4 * j + 2] <- char (d + int '0')
            | Candidates cs ->
                defaultCellCandidates
                |> Seq.iter 
                    (fun d -> 
                        if Set.contains d cs then 
                            boardStr.[4 * i + (d - 1) / 3 + 1].[4 * j + (d - 1) % 3 + 1] <- char (d + int '0')))

    boardStr
    |> Seq.map String.fromCharArray
    |> Seq.fold StringBuilder.appendLine (StringBuilder())
    |> printf "%A"
    printfn "Progress = %d\n" (getProgress (Board b))

let solve b =
    let rec solveImpl p b =
        if (isSolved b) then
            p
        else
            applyNakedSingleStrategy b
            printBoard b
            solveImpl (p + 1) b
    solveImpl 0 b
