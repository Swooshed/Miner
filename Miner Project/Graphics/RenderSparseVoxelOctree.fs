module Miner.RenderSparseVoxelOctree

open Miner.Blocks
open Miner.SparseVoxelOctree
open Miner.Utils.Misc
open Miner.Utils.ObjVBO
open Pencil.Gaming.Graphics
open Pencil.Gaming.MathUtils

type SVORenderer(matrixID) = 
    let cube = ObjVBO "Resources/gl_cube.obj"
    let textureID = GL.Utils.LoadImage "Resources/gl_uvmap.bmp"
    
    member this.DrawFrom (svo : SparseVoxelOctree<Option<Block>>) (m : Matrix) (vp : Matrix) = 
        let mutable mvp = m * vp
        GL.UniformMatrix4(matrixID, false, &mvp)
        GL.ActiveTexture TextureUnit.Texture0
        GL.BindTexture(TextureTarget.Texture2D, textureID)
        GL.Uniform1(textureID, 0)

        let drawSubSVO octant subSVO = this.DrawFrom subSVO (fromChildSpace octant * m) vp 
        match svo.Nodes with
        | Full None -> ()
        | Full _ -> cube.Draw ()
        | Subdivided arr -> Array.iteri drawSubSVO arr
    
    // Given a SVO, draw it between the bounds of (0,0,0) and (1,1,1)
    member this.Draw (svo : SparseVoxelOctree<Option<Block>>) (vp : Matrix) =
        this.DrawFrom svo Matrix.Identity vp
