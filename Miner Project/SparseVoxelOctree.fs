module Miner.SparseVoxelOctree

open Miner.Blocks
open Miner.Utils.Misc
open Pencil.Gaming.MathUtils
open System.Collections.Generic

(*
Each cube thinks that it is the unit cube between (1,1,1) and (2,2,2).
The 'size' field is the actual size of the cube - size = 0 is a single (Full) voxel.
Size = 1 is either full or a node of 8 full voxels. And so on.
*)

// maybe uint 16 or 8 for size instead? For which n does (2 ** max_uintn) fit into float32?
type SparseVoxelOctree<'a when 'a : equality>(size : int, nodes_ : SparseVoxelNode<'a>) = 
    let mutable nodes = nodes_
    member this.Size = size
    
    member this.Nodes 
        with get () = nodes
        and set nodes_ = nodes <- nodes_
    
    member this.AbsoluteRadius = 2.f ** float32 (size - 1)
    
    member this.InRelativeBounds position = 
        let (bx, by, bz) = mapVectorT (fun x -> 1.f < x && x < 2.f) position
        bx && by && bz
    
    (*
    Which octant is the point in compared to the tree? There are 8 possibilities: each of the three axes is split into two halves, making 8 total.
    These 8 octants are represented by a bool triple, indicating whether or not the point is ABOVE the three axes respectively. Then that (bool, bool, bool)
    is interpreted as a binary number to give a value between 0 (false, false, false), 1 (false, false, true), and 7 (true, true, true).
    *)
    member this.WhichOctant = mapVectorT (fun x -> x > 1.5f) >> boolsToOct
    member this.Insert position element = this.InsertLevel position 0 element
    
    member this.InsertLevel (position : Vector4) height (element : 'a) = 
        printfn "inserting (%f, %f, %f) at size %i" position.X position.Y position.Z size
        if height < 0 then raise (System.ArgumentException "Can't add at a negative height.")
        if size < 0 then raise (System.ArgumentException "Can't have an octree with a negative size.")
        if height > size then raise (System.ArgumentException "Tried to add higher than the height of the octree")
        if not (this.InRelativeBounds position) then 
            raise (System.IndexOutOfRangeException "The position was not contained inside the cube.")
        if height = size then 
            match this.Nodes with
            | Full a -> this.Nodes <- Full element
            | _ -> raise (System.InvalidOperationException "Cubes with size 0 cannot be subdivided")
        else // size > height, need to go smaller until we get to the right octree to insert into
            let insertIntoChild (arr : SparseVoxelOctree<'a> []) = 
                let newQuadrant = this.WhichOctant position
                let newPosition = position * toChildSpace newQuadrant
                arr.[newQuadrant].InsertLevel newPosition height element
            match this.Nodes with
            | Full a when a = element -> () // it's already there
            | Full a -> 
                let arr = Array.init 8 (fun _ -> SparseVoxelOctree(size - 1, Full a))
                // now we have split the full node into 8 full subnodes we can actually add our point
                insertIntoChild arr
                this.Nodes <- Subdivided arr
            | Subdivided arr -> // Recurse, and if we fill a node up then replace it with a Full
                insertIntoChild arr
                if Array.forall (fun (x : SparseVoxelOctree<'a>) -> x.Nodes = Full element) arr then 
                    this.Nodes <- Full element
    
    member this.Item 
        with get (path : int list) = 
            match path with
            | [] -> this
            | head :: path -> 
                match this.Nodes with
                | Full a -> this
                | Subdivided arr -> arr.[head].[path]

(*
The idea is that there's no such thing as an 'empty' node. To reclaim this, simply use Option<a> for your type, where None represents empty space.
*)
and SparseVoxelNode<'a when 'a : equality> = 
    | Full of 'a
    | Subdivided of SparseVoxelOctree<'a> [] // should always have length 8

let emptyWorld = 
    let empty = SparseVoxelOctree<Option<Block>>(2, Full None)
    //let lowerHalf = List.map boolsToOct [(false, false, false); (false, false, true); (false, true, false); (false, true, true)]
    //SparseVoxelOctree<Option<Block>>(2, Full (Some Opaque))
    empty.InsertLevel (Vector4(1.1f, 1.1f, 1.1f, 1.f)) 0 (Some Opaque)
    empty.InsertLevel (Vector4(1.9f, 1.9f, 1.9f, 1.f)) 0 (Some Opaque)
    empty
