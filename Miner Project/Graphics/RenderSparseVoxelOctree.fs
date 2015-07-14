module Miner.RenderSparseVoxelOctree

open Miner.Blocks
open Miner.SparseVoxelOctree
open Miner.Utils.Misc
open Miner.Utils.ObjVBO

open Pencil.Gaming.Graphics
open Pencil.Gaming.MathUtils

type SVORenderer (matrixID) =
    let cube = new ObjVBO ("Resources/gl_cube.obj") 
    let textureID = GL.Utils.LoadImage "Resources/gl_uvmap.bmp"

    member this.DrawFrom  (m : Matrix) (svo : SparseVoxelOctree<Option<Block>>) (vp : Matrix) =
        let mutable mvp = m * vp
        GL.UniformMatrix4(matrixID, false, &mvp)

        GL.ActiveTexture TextureUnit.Texture0
        GL.BindTexture (TextureTarget.Texture2D, textureID)
        GL.Uniform1 (textureID, 0)

        let drawSubSVO quadrant subSVO =
            let transform = Matrix.CreateTranslation (originDiff quadrant)
            this.DrawFrom (transform * m) subSVO vp 

        match svo.Nodes with
            | Full None      -> ()
            | Full _         -> cube.Draw ()
            | Subdivided arr -> Array.iteri drawSubSVO arr

    member this.Draw = this.DrawFrom Matrix.Identity

