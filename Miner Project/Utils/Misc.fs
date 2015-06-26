module Miner.Utils.Misc

open Pencil.Gaming.MathUtils

let zipVector3With f (v:Vector3) (w:Vector3) = Vector3 (f v.X w.X, f v.Y w.Y, f v.Z w.Z)
let mapVector3T f (v:Vector3) = (f v.X, f v.Y, f v.Z)
let mapVector3 f (v:Vector3) = Vector3 (f v.X, f v.Y, f v.Z)

let intIf b = if b then 1 else 0

let xAxisIx = 0
let yAxisIx = 1
let zAxisIx = 2

let octToBools n = 
    let isSet i = n &&& (1 <<< i) <> 0
    (isSet xAxisIx, isSet yAxisIx, isSet zAxisIx)

let boolsToOct ((bx, by, bz)) =
    (intIf bx <<< xAxisIx) ||| (intIf by <<< yAxisIx) ||| (intIf bz <<< zAxisIx)

let originDiff n = 
    let translate b = if b then -1.f else -0.5f
    let (dx, dy, dz) = octToBools n
    Vector3 (translate dx, translate dy, translate dz)
 