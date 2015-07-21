module Miner.CubePicker

open Miner.Blocks
open Miner.SparseVoxelOctree
open Miner.Utils.Misc
open FSharpx.Option
open Pencil.Gaming.MathUtils
open System.Linq.Expressions

let rayIntersections (origin : Vector4) (directionRaw : Vector4) (svo : SparseVoxelOctree<Option<'a>>) = 
    // if the top level is full of nothing, then there can be no collisions
    if svo.Nodes = Full None then None
    else 
        // first, do a bunch of initialisation including setting up the constants
        let eps = 0.01f // 2.f ** (- float32 svo.Size)
        
        let floorEps (x : float32) = 
            if abs x < eps then eps * float32 (sign x)
            else if x = 0.f then eps
            else x
        
        let direction = mapVector floorEps directionRaw
        let (xIsPos, yIsPos, zIsPos) = mapVectorT (fun x -> sign x = 1) direction
        // we'll switch the axis such that all three components of direction is negative
        // the cheapest way to do this is trasform which child we fetch with indexing by
        // xoring it with octantMask
        let octantMask = boolsToOct (xIsPos, yIsPos, zIsPos)
        let tCoeff = mapVector (fun x -> 1.f / abs x) direction
        let tBiasTemp = zipVectorWith (*) tCoeff origin
        
        let invertIf b bias coeff = 
            if b then 3.f * coeff - bias
            else bias
        
        let tBiasInitial = 
            Vector4
                (invertIf xIsPos tBiasTemp.X tCoeff.X, invertIf yIsPos tBiasTemp.Y tCoeff.Y, 
                 invertIf zIsPos tBiasTemp.Z tCoeff.Z, 1.f)
        let tMinInitial = max (0.f, maxVector (2.f * tCoeff - tBiasInitial))
        let tSVO axis x = tCoeff.[axisIx axis] * x + tBiasInitial.[axisIx axis]
        
        // The direction of the ray is always downwards so it'll hit x = 2.f before x = 1.f
        let tMin = 
            let getMin axis = tSVO axis 2.f
            Array.max [| getMin X
                         getMin Y
                         getMin Z |]
        
        let tMax = 
            let getMax axis = tSVO axis 1.f
            Array.min [| getMax X
                         getMax Y
                         getMax Z |]
        
        //System.Console.ReadLine() |> ignore
        if tMin <= tMax then 
            printfn "No hit in initialisation"
            None
        else 
            printfn "Hit in initialisation"
            // FIXME: always triggers
            let rec rayIntersectionsGo (parent : SparseVoxelOctree<Option<'a>>) (pathSoFar : List<int>) origin = 
                if parent.Size = 0 then Some pathSoFar
                else 
                    let tBias = zipVectorWith (*) tCoeff origin
                    let tParent axis x = tCoeff.[axisIx axis] * x + tBias.[axisIx axis]
                    match parent.Nodes with
                    | Full None -> None
                    | Subdivided arr -> 
                        if true then None
                        else // FIXME: this is very very temporary
                            let octants = [ //                let firstOctant  = octantMask ^^^ raise (System.NotImplementedException ()) // FIXME:
                                            //                let (bx, by, bz) = octToBools firstOctant
                                            //                let lowerEdge b  = if b then 1.f else 1.5f
                                            //                let otherOctants =  []
                                            //                firstOctant :: otherOctants
                                            7; 6; 5; 4; 3; 2; 1; 0 ]
                            
                            let onOctant octant = 
                                let child = arr.[octant]
                                let newOrigin = origin * toChildSpace octant
                                rayIntersectionsGo child (octant :: pathSoFar) newOrigin
                            List.tryPick onOctant octants
                    | Full(Some element) -> 
                        // Now we're looking at quadrants on the face of this large cube.
                        // We can assume that there IS a collision.
                        let tHit = 
                            let getMin axis = tParent axis -2.f
                            Array.max [| getMin X
                                         getMin Y
                                         getMin Z |]
                        
                        let absoluteHitPosition : Vector4 = origin + tHit * direction
                        printfn "hitPos: (%f, %f, %f)" absoluteHitPosition.X absoluteHitPosition.Y absoluteHitPosition.Z
                        let planeHit = 
                            let distanceFromCube = mapVector (fun f -> abs (2.f - f)) absoluteHitPosition
                            if distanceFromCube.X > distanceFromCube.Y then 
                                if distanceFromCube.Z > distanceFromCube.X then Z
                                else X
                            else if distanceFromCube.Z > distanceFromCube.Y then Z
                            else Y
                        
                        let relativeHitPosition = absoluteHitPosition - Vector4(1.f)
                        let singleVoxelWidth : float32 = 1.f / float32 (parent.Size + 1)
                        let (voxelsX, voxelsY, voxelsZ) = 
                            mapVectorT (fun f -> int (f / singleVoxelWidth)) relativeHitPosition
                        
                        // turn this into a path
                        let findDirection bit = 
                            let xBit = planeHit = X || isSet voxelsX bit
                            let yBit = planeHit = Y || isSet voxelsY bit
                            let zBit = planeHit = Z || isSet voxelsZ bit
                            boolsToOct (xBit, yBit, zBit)
                        
                        // The path cannot be empty, as we know that parent.Size <> 0
                        let path = List.map findDirection [ parent.Size..1 ]
                        Some path
            rayIntersectionsGo svo [] origin |> Option.map (List.map ((^^^) octantMask) >> List.rev)
