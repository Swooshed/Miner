module Miner.Utils.Misc

open Pencil.Gaming.MathUtils

open System.Collections.Generic

let zipVector3With f (v:Vector3) (w:Vector3) = Vector3 (f v.X w.X, f v.Y w.Y, f v.Z w.Z)
let mapVector3T f (v:Vector3) = (f v.X, f v.Y, f v.Z)
let mapVector3 f (v:Vector3) = Vector3 (f v.X, f v.Y, f v.Z)

let maxVector3 (v:Vector3) = max (max v.X v.Y) v.Z
let minVector3 (v:Vector3) = min (min v.X v.Y) v.Z

let intIf b = if b then 1 else 0


type Axes = X | Y | Z
let axisIx axis =
    match axis with
    | X -> 0
    | Y -> 1
    | Z -> 2
    
let isSet n i = n &&& (1 <<< i) <> 0

let octToBools n = (isSet n (axisIx X), isSet n (axisIx Y), isSet n (axisIx Z))

let boolsToOct ((bx, by, bz)) =
    (intIf bx <<< axisIx X) ||| (intIf by <<< axisIx Y) ||| (intIf bz <<< axisIx Z)

let originDiff n = 
    let translate b = if b then -1.f else -0.5f
    let (dx, dy, dz) = octToBools n
    Vector3 (translate dx, translate dy, translate dz)

  