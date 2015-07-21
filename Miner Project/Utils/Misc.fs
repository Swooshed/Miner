module Miner.Utils.Misc

open Pencil.Gaming.MathUtils
open System.Collections.Generic

let zipVectorWith f (v : Vector4) (w : Vector4) = Vector4(f v.X w.X, f v.Y w.Y, f v.Z w.Z, max v.W w.W)
let mapVectorT f (v : Vector4) = (f v.X, f v.Y, f v.Z)
let mapVector f (v : Vector4) = Vector4(f v.X, f v.Y, f v.Z, v.W)
let maxVector (v : Vector4) = max (max v.X v.Y) v.Z
let minVector (v : Vector4) = min (min v.X v.Y) v.Z

let intIf b = 
    if b then 1
    else 0

type Axes = 
    | X
    | Y
    | Z

let axisIx axis = 
    match axis with
    | X -> 0
    | Y -> 1
    | Z -> 2

let isSet n i = n &&& (1 <<< i) <> 0
let octToBools n = (isSet n (axisIx X), isSet n (axisIx Y), isSet n (axisIx Z))
let boolsToOct ((bx, by, bz)) = (intIf bx <<< axisIx X) ||| (intIf by <<< axisIx Y) ||| (intIf bz <<< axisIx Z)

let toChildSpace n = 
    // the matrix will map [1,1.5][1.5,2] to [1,2][1,2]
    // on each axis as specified by n
    let (bx, by, bz) = octToBools n
    
    let f b = 
        if b then -2.f
        else -1.f
    
    let scale = Matrix.CreateScale 2.f
    //let move = Matrix.Transpose (Matrix.CreateTranslation (f bx, f by, f bz)) // FIXME
    let move = Matrix.CreateTranslation(f bx, f by, f bz)
    //printfn "octant: %i\nmove:\n%A\nscale:\n%A\nscale*move:\n%A" n move scale (scale*move)
    scale * move
