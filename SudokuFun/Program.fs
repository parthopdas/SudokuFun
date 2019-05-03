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
    let b = SudokuFun.initBoard ()
    printfn "%A" b
    0 // return an integer exit code
