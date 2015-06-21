module Miner.RenderSparseVoxelOctree

open Miner.ObjVBO
open Miner.SparseVoxelOctree

open Pencil.Gaming.Graphics
open Pencil.Gaming.MathUtils

type SVORenderer (matrixID) =
    let cube = new ObjVBO ("gl_cube.obj") 
    let textureID = GL.Utils.LoadImage "gl_uvmap.bmp"

    member this.DrawAt (svo : SparseVoxelOctree<int>) (vp : Matrix) (oldM : Matrix) level =
        do  printf "drawing at (%f, %f, %f)\n" svo.Centre.X svo.Centre.Y svo.Centre.Z
        let m = Matrix.CreateTranslation svo.Centre * oldM
        // FIXME: level parameter is quick and dirty
        let mutable mvp = Matrix.CreateScale 0.5f * m * vp
        GL.UniformMatrix4(matrixID, false, &mvp)

        GL.ActiveTexture TextureUnit.Texture0
        GL.BindTexture (TextureTarget.Texture2D, textureID)
        GL.Uniform1 (textureID, 0)

        let drawSubSVO (subSVO : SparseVoxelOctree<int>) =
            this.DrawAt subSVO vp m (level + 1)

        match svo.Nodes with
            | Full      1    -> cube.Draw ()
            | Full      _    -> ()
            | Subdivided arr -> Array.iter drawSubSVO arr

    member this.Draw svo vp =
        this.DrawAt svo vp Matrix.Identity 1

