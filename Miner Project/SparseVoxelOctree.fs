module Miner.SparseVoxelOctree

open Pencil.Gaming.MathUtils
(*
Because we're working over discrete voxels, there is a minimum resolution - at node_height 0, the octree is a single (by definition full) voxel.
To avoid unevenly sized nodes, not to mention to make the maths easier, the number of voxels that can possibly exist
in each octree is (2 ** node_height)^3

TODO: implement a 'path to element' coordinate system maybe?
*)

let zipVector3With f (v:Vector3) (w:Vector3) = (f v.X w.X, f v.Y w.Y, f v.Z w.Z)


type SparseVoxelOctree<'a when 'a : equality>(centre  : Vector3, nodeHeight : uint32, nodes_ : SparseVoxelNode<'a>) =
    let mutable nodes = nodes_

    member this.Centre      = centre
    member this.NodeHeight  = nodeHeight
    member this.SetNodes n  = nodes <- n
    member this.Nodes
        with get () = nodes
        and  set nodes_ = nodes <- nodes_

    (*
    Which octant is the point in compared to the tree? There are 8 possibilities: each of the three axes is split into two halves, making 8 total.
    These 8 octants are represented by a bool triple, indicating whether or not the point is ABOVE the three axes respectively. Then that (bool, bool, bool)
    is interpreted as a binary number to give a value between 0 (false, false, false) and 7 (true, true, true).
    *)
    member this.WhichOctant position =
        let d l c = if l > c then 1 else 0
        let (dx, dy, dz) = zipVector3With d position centre
        (dx <<< 2) ||| (dy <<< 1) ||| dz

    member this.inBounds position =
        let centre_to_edge              = 2.f ** (float32 (nodeHeight - 1u))
        let abs_diff ca (a : float32)   = System.Math.Abs(ca - a)
        let (dx, dy, dz)                = zipVector3With abs_diff centre position
        let furthest_distance_to_centre = List.max [dx; dy; dz]
        furthest_distance_to_centre <= centre_to_edge

    member this.Insert position (element : 'a) =
        if not (this.inBounds position) then raise (new System.IndexOutOfRangeException())

        // FIXME: this is failing to work, I think
        match nodes with
            | Full a when a = element -> ()  // it's already there

            | Full a when nodeHeight = 0u -> // at minimum resolution, fill in the voxel
                printf "filling\n"
                this.SetNodes (Full element)

            | Full a -> // need to subdivide
                printf "subdividing\n"
                let octant_to_node n =
                    let adjust o ca = (if n &&& o <> 0 then (+) else (-)) ca (2.f ** float32 (nodeHeight - 2u))
                    let centre = new Vector3 (adjust 4 centre.X, adjust 2 centre.Y, adjust 1 centre.Z)
                    SparseVoxelOctree (centre,  nodeHeight - 1u, Full a)
                let arr = Array.map octant_to_node [| 0..7 |]
                // now we have split the full node into 8 full subnodes we can actually add our point
                arr.[this.WhichOctant position].Insert position element
                this.SetNodes (Subdivided arr)

            | Subdivided arr when nodeHeight > 0u -> // Recurse, and if we fill a node up then replace it with a Full
                printf "recursing\n"
                arr.[this.WhichOctant position].Insert position element
                if Array.forall (function (x : SparseVoxelOctree<'a>) -> x.Nodes = Full element) arr
                    then this.SetNodes (Full element)


             | _ -> // We're in an invalid state
                raise (new System.InvalidOperationException())

    member this.ClosestVoxel position =
        match nodes with
            | Subdivided arr -> arr.[this.WhichOctant position].ClosestVoxel position
            | Full a         -> a

(*
The idea is that there's no such thing as an 'empty' node. To reclaim this, simply use Option<a> for your type, where None represents empty space.
*)
and SparseVoxelNode<'a when 'a : equality> =
    | Full of 'a
    | Subdivided of SparseVoxelOctree<'a>[] // should always have length 8

let minimalSVO =
    let topRight = new SparseVoxelOctree<int>(new Vector3 (0.5f,0.5f,0.5f), 0u, Full 1)
    let empty = new SparseVoxelOctree<int>(new Vector3 (), 1u, Full 0)
    let bottomLeft = new SparseVoxelOctree<int>(new Vector3 (-0.5f,-0.5f,-0.5f), 0u, Full 1)
    let empty = new SparseVoxelOctree<int>(new Vector3 (0.f,0.f,0.f), 1u, Subdivided [| bottomLeft; empty; empty; empty;
                                                                                        empty; empty; empty; topRight; |])
    empty.Insert (new Vector3 (0.9f, 0.9f, 0.9f)) 1
    empty.Insert (new Vector3 (-0.9f, -0.9f, -0.9f)) 1
    empty