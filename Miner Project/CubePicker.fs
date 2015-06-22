module Miner.CubePicker

open Miner.SparseVoxelOctree

open FSharpx.Option
open Pencil.Gaming.MathUtils

let guard b x = if b then Some x else None

let testRayOBBIntersection
        (rayOrigin : Vector3)
        (rayDirection : Vector3)
        (aabbMin : Vector3)
        (aabbMax : Vector3)
        (model : Matrix) = 

    let minLimit = 0.f
    let maxLimit = 10000.f

    let OBBPositionWorldspace = new Vector3 (model.Column3)
    let delta = OBBPositionWorldspace - rayOrigin

    // Test intersection with the 2 planes perpendicular to the OBB's axis
    let findIntersection index =
        let axis = new Vector3 (model.[index, 0], model.[index, 1], model.[index, 2])
        let e = Vector3.Dot (axis, delta)
        let f = Vector3.Dot (rayDirection, axis)
           
        if System.Math.Abs f > 0.001f then
            let intersection1 = (e + aabbMin.[index])/f // intersection with "left" plane
            let intersection2 = (e + aabbMax.[index])/f // intersection with "right" plane

            Some (System.Math.Min (intersection1, intersection2), 
                  System.Math.Max (intersection1, intersection2))

        // the ray is almost parallel to the planes
        else guard (not (aabbMin.[index] - e > 0.f || aabbMax.[index] - e > 0.f)) (minLimit, maxLimit)

    // could parallelise these three calls maybe?
    maybe {
        let! xMin, xMax = findIntersection 0
        let! yMin, yMax = findIntersection 1
        let! zMin, zMax = findIntersection 2
        let minIntersection = Array.max [|xMin, yMin, zMin, minLimit|]
        let maxIntersection = Array.min [|xMax, yMax, zMax, maxLimit|]
        return guard (maxIntersection > minIntersection) minIntersection
    }

