module Main

open Miner.SparseVoxelOctree

let test1 =
    let oct = new SparseVoxelOctree<int>((0.,0.,0.), 5, Full 0)
    oct.insert_into_octree (1.,1.,2.) 1
    1 = oct.closest_voxel (1.,1.,1.1)

let tests = List.forall id [test1]

[<EntryPoint>]
let main argv =
    printf "%b\n" tests
    System.Console.ReadKey() |> ignore
    0