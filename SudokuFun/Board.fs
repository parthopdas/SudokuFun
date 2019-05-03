module SudokuFun

type Board =
    | Board of Set<int>[,] 

(*
  To generate new puzzls:
  - Go to https://qqwing.com/generate.html
  - Select output format as single line
 *)
let private getNewCandidates candidates leaveOut =
    candidates
    |> List.filter (fun x -> x <> leaveOut)

let initBoard str: Board =
    if (String.length str <> 81) then
        failwith "Input must be a string of 81 characters"

    let b = Array2D.create 9 9 Set.empty

    seq { for i in 0 .. 8 do
            for j in 0 .. 8 do
                yield (i, j) }
    |> Seq.iter (
        fun (i, j) -> 
            let s = Set.ofSeq [1..9] |> Set.remove (int str.[i * 9 + j] - int '0')
            b.[i,j] <- Set.ofSeq s)
    
    b |> Board
