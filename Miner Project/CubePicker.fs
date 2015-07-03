module Miner.CubePicker
(*
open Miner.Blocks
open Miner.SparseVoxelOctree
open Miner.Utils.Misc

open FSharpx.Option
open Pencil.Gaming.MathUtils
open System.Linq.Expressions

let rayIntersections (origin       : Vector3)
                     (directionRaw : Vector3)
                     (aabbMin      : Vector3)
                     (aabbMax      : Vector3)
                     (pathSoFar    : List<byte>)
                     (svo          : SparseVoxelOctree<'a>) = 
    // first, do a bunch of initialisation including setting up the constants
    let eps = 2.f ** (- float32 svo.Size)
    let floorEps x = if x < eps then eps * float32 (System.Math.Sign x) else x
    let direction  = mapVector3 floorEps directionRaw

    let (xIsPos, yIsPos, zIsPos) = mapVector3T (fun x -> x > 0.f) direction

    // we'll switch the axis such that all three components of direction is negative
    // the cheapest way to do this is trasform which child we fetch with indexing by
    // xoring it with octantMask
    let octantMask = (intIf xIsPos <<< xAxisIx) |||
                     (intIf yIsPos <<< yAxisIx) |||
                     (intIf zIsPos <<< zAxisIx)

    let tCoeff     = mapVector3 (fun x -> 1.f / System.Math.Abs x) direction
    let tBiasTemp  = zipVector3With (*) tCoeff origin
    let invertIf b x = if b then x else -x
    let tBias      = Vector3 (invertIf xIsPos tBiasTemp.X,
                              invertIf yIsPos tBiasTemp.Y,
                              invertIf zIsPos tBiasTemp.Z)

    let t axis x = tCoeff.[axis] * x + tBias.[axis]

    // does it intersect with this cube?
        // should we stop here?
            // which quadrant?
            // recurse


        
    Seq.empty

//let viewBlocks  (origin       : Vector3)
//                (directionRaw : Vector3)
//                (aabbMin      : Vector3)
//                (aabbMax      : Vector3)
//                (pathSoFar    : List<byte>)
//                (svo          : SparseVoxelOctree<Block>)
//                (newSVO       : SparseVoxelOctree<Block>) =
//
//    let isOpaque b =
//        match b with
//            | Opaque -> true
//            | _      -> false
//    rayIntersections origin directionRaw aabbMin aabbMax pathSoFar svo newSVO
//
//let firstSolidHit (origin       : Vector3)
//                  (directionRaw : Vector3)
//                  (aabbMin      : Vector3)
//                  (aabbMax      : Vector3)
//                  (pathSoFar    : List<byte>)
//                  (svo          : SparseVoxelOctree<Block>)
//                  (newSVO       : SparseVoxelOctree<Block>) = 
//    let isSolid b =
//        match b with
//            | Transparent -> true
//            | _           -> false
//    Seq.head (rayIntersections origin directionRaw aabbMin aabbMax pathSoFar svo newSVO)

*)