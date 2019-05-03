module SudokuFun

type Board =
    | Board of Set<int>[,] 

let initBoard (): Board =
    let b = Array2D.create 9 9 Set.empty

    seq { for i in 0 .. 8 do
            for j in 0 .. 8 do
                yield (i, j) }
    |> Seq.iter (fun (i, j) -> b.[i,j] <- Set.ofSeq [1..9])
    
    b |> Board
