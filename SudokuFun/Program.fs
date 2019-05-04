(*
  Types:
  - Primitive
  - Algebraic [+, *, ^]
  - Flavors

  Functions:
  - Pure & total
  - Partial application & Currying
  - Higher order
  - Composition techniques

  Tools
  - Monoids
  - Functors/Applicatives/Monads
  - Catamorphisms
  - Actors
 *)


[<EntryPoint>]
let main _ =
    let b = SudokuFun.initBoard "6..8...42..16....7..7.....3.6.5...1..........728..4.6...3.4..8.4..7..3...1..5...."
    SudokuFun.printBoard b
    let passes = SudokuFun.solve b
    printfn "Number of passses = %d" passes    
    SudokuFun.printBoard b

    0
