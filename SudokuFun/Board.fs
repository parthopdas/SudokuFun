module SudokuFun

open System.Text

type Board =
    | Board of Set<int>[,] 

let cellIndices =
    seq { for i in 0 .. 8 do
            for j in 0 .. 8 do
                yield (i, j) }
    |> List.ofSeq

///
/// To generate new puzzle:
/// 1. Go to https://qqwing.com/generate.html
/// 2. Select output format as single line
let initBoard str: Board =
    if (String.length str <> 81) then
        failwith "Input must be a string of 81 characters"

    let b = Array2D.create 9 9 Set.empty

    cellIndices
    |> Seq.iter (
        fun (i, j) -> 
            let c = (int str.[i * 9 + j] - int '0')
            let s = if c >= 1 && c <= 9 then [ c ] else [1..9]
            b.[i,j] <- Set.ofSeq s)
    
    b |> Board

let printBoard (Board b) =
    let boardStr = 
        [|
          "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
          "║   |   |   ║   |   |   ║   |   |   ║"
          "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝"|]
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
