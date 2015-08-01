module Miner.Blocks

[<StructuralEquality; NoComparison>]
type Block = 
    | Opaque
    | Translucent
    | Transparent

let blockInt b = 
    match b with
    | Opaque -> 0
    | Translucent -> 1
    | Transparent -> 2
