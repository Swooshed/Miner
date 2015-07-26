module Miner.Utils.Misc

open Pencil.Gaming.MathUtils
open System.Collections.Generic

let zipVectorWith f (v : Vector4) (w : Vector4) = Vector4(f v.X w.X, f v.Y w.Y, f v.Z w.Z, max v.W w.W)
let mapVectorT f (v : Vector4) = (f v.X, f v.Y, f v.Z)
let mapVector f (v : Vector4) = Vector4(f v.X, f v.Y, f v.Z, v.W)
let maxVector (v : Vector4) = max (max v.X v.Y) v.Z
let minVector (v : Vector4) = min (min v.X v.Y) v.Z

let intIf b = if b then 1 else 0

type Axes = 
    | X
    | Y
    | Z

let axes = [| X; Y; Z |]

let axisIx axis = 
    match axis with
    | X -> 0
    | Y -> 1
    | Z -> 2

let isSet n i = n &&& (1 <<< i) <> 0
let octToBools n = (isSet n (axisIx X), isSet n (axisIx Y), isSet n (axisIx Z))
let boolsToOct ((bx, by, bz)) = (intIf bx <<< axisIx X) ||| (intIf by <<< axisIx Y) ||| (intIf bz <<< axisIx Z)

let childOrigin n = 
    let (bx, by, bz) = octToBools n
    let f b = if b then 0.5f else 0.f
    Vector4 (f bx, f by, f bz, 1.f)

// To use when going from the perspective of the adult to the child (i.e. ray tracing)
// if fx then this matrix will map the x component from [0, 0.5] to [0, 1]
//       else map [0.5, 1] to [0, 1]
// (same for all the other axes)
let toChildSpace n = 
    let scale = Matrix.CreateScale 2.f
    let move = Matrix.CreateTranslation(-(childOrigin n).Xyz)
    move * scale

// To use when going from the perspective of the child to the adult (i.e. drawing)
// if fx then this matrix will map the x component from [0, 1] to [0, 0.5]
//       else map [0, 1] to [0.5, 1]
// (same for all the other axes)
let fromChildSpace n =
    let scale = Matrix.CreateScale 0.5f
    let move = Matrix.CreateTranslation ((childOrigin n).Xyz)
    scale * move
