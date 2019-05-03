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
    let b = SudokuFun.initBoard ".47..1.5.9....5.......7....7.5.92.4.4..56..1781..4..2.1....4.....27......6..29..."
    printfn "%A" b
    0 // return an integer exit code
