module Miner.CubePicker

open Miner.Blocks
open Miner.SparseVoxelOctree
open Miner.Utils.Misc
open FSharpx.Option
open Pencil.Gaming.MathUtils
open System.Linq.Expressions

let rayHit (origin : Vector4) (directionRaw : Vector4) (svo : SparseVoxelOctree<Option<'a>>) = 
    // let origin = Vector4 (0.5f, 0.5f, 5.f, 1.f)
    // let directionRaw = Vector4 (0.f, 0.f, -1.f, 0.f)

    // if the top level is full of nothing, then there can be no collisions
    if svo.Nodes = Full None then None else
    
    // first, do a bunch of initialisation including setting up the constants
    let eps = 0.00001f // 2.f ** (- float32 svo.Size)
        
    let floorEps (x : float32) = 
        if x = 0.f then eps
        else if abs x < eps then eps * float32 (sign x)
        else x
        
    let direction = mapVector floorEps directionRaw

    // The ray is given by x = origin + t * direction
    // => t = (x - origin) / direction
    // => t = x * 1/direction - origin/direction
    // => t = x * tCoeff - tBias
    // => tCoeff := 1/direction; tBias := origin/direction 
    let tCoeff = mapVector (fun x -> 1.f / abs x) direction
    let tBias = zipVectorWith (*) tCoeff origin
    let tSVO axis x = tCoeff.[axisIx axis] * x - tBias.[axisIx axis]
    let tMin, tMax =
        let getMinMax axis = sortPair (tSVO axis 0.f, tSVO axis 1.f)
        let mins, maxes = Array.unzip (Array.map getMinMax axes)
        Array.max mins, Array.min maxes
        
    if tMin > tMax then printfn "No hit in initialisation"; None else 
    printfn "Hit in initialisation"

    let rec rayHitGo (parent : SparseVoxelOctree<Option<'a>>) origin = 
        let tBias = zipVectorWith (*) tCoeff origin
        let tParent axis x = tCoeff.[axisIx axis] * x + tBias.[axisIx axis]

        if parent.Nodes = Full None then None else
        // this is the t value for the hit
        let tHit = 
            let intersection1 axis = tParent axis 0.f
            let intersection2 axis = tParent axis 1.f
            let minMapOnAxes f = Array.min (Array.map f axes)
            min (minMapOnAxes intersection1) (minMapOnAxes intersection2)

        let hitPosition : Vector4 = origin + tHit * direction

        match parent.Nodes with
        | Full _ -> Some hitPosition
        | Subdivided arr ->
            let firstOctant = parent.WhichOctant hitPosition
            let (positiveX, positiveY, positiveZ) = mapVectorT (fun f -> f > 0.f) direction
            // FIXME: find out which other octants the ray passes through

            None
    rayHitGo svo origin
