module SudokuFun

open System.Text

type Board =
    | Board of Set<int>[,] 

let defaultCellCandidates = Set.ofSeq [ 1..9 ]

let cellIndices =
    seq { for i in 0 .. 8 do
            for j in 0 .. 8 do
                yield (i, j) }
    |> List.ofSeq

let nonetIndicesMap =
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

    let b = Array2D.create 9 9 Set.empty

    cellIndices
    |> Seq.iter (
        fun (i, j) -> 
            let c = (int str.[i * 9 + j] - int '0')
            let s = if c >= 1 && c <= 9 then Set.singleton c else defaultCellCandidates
            b.[i,j] <- s)
    
    b |> Board

let getSetOfFilledCells : seq<Set<int>> -> Set<int> =
    Seq.filter (fun x -> x.Count = 1)
    >> Seq.map Seq.exactlyOne
    >> Set.ofSeq

let getNonetOccupancy (Board b) i j =
    nonetIndicesMap
    |> Map.find (i, j)
    |> Seq.map (fun (i, j) -> b.[i, j])
    |> getSetOfFilledCells

let getRowOccupancy (Board b) i =
    b.[i, *]
    |> getSetOfFilledCells

let getColumnOccupancy (Board b) j =
    b.[*, j]
    |> getSetOfFilledCells

let crossHatchCell (Board b) i j =
    if b.[i,j].Count = 1 then
        ()
    else
        let nSet = getNonetOccupancy (Board b) (i / 3) (j / 3)
        let rSet = getRowOccupancy (Board b) i
        let cSet = getColumnOccupancy (Board b) j   
        let candidates = Set.difference defaultCellCandidates (Set.unionMany [ nSet; rSet; cSet ])
        b.[i,j] <- candidates
        
let numberOfHints (Board b) =
    cellIndices
    |> Seq.filter (fun (i, j) -> b.[i,j].Count = 1)
    |> Seq.length

let crossHatch (Board b) =
    cellIndices
    |> Seq.iter (fun (i, j) -> crossHatchCell (Board b) i j)
    numberOfHints (Board b)

let isSolved b = numberOfHints b = 81

let printBoard (Board b) =
    let boardStr = 
        [| "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
           "║   │   │   ║   │   │   ║   │   │   ║"
           "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝" |]
        |> Array.map String.toCharArray

    cellIndices
    |> Seq.iter 
        (fun (i, j) -> 
            if (b.[i,j].Count = 1) then 
                (boardStr.[2 * i + 1].[4 * j + 2] <- char ((b.[i,j] |> Seq.exactlyOne) + int '0')))

    boardStr
    |> Seq.map String.fromCharArray
    |> Seq.fold StringBuilder.appendLine (StringBuilder())
    |> printfn "%A"

let solve b =
    let rec solveImpl h0 p b =
        if (isSolved b) then
            p
        else
            let h1 = crossHatch b
            if h0 = h1 then failwithf "Unable to solve any futher after %d passes. Apply other strategy(s)." (p + 1)
            printBoard b
            solveImpl h1 (p + 1) b
    solveImpl (numberOfHints b) 0 b
