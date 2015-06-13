module ObjectLoader

open Pencil.Gaming.Graphics
open Pencil.Gaming.MathUtils

let loadObject (path : string) =
    let lines = System.IO.File.ReadAllLines path

    let textureID = GL.GenTexture ()
    GL.BindTexture (TextureTarget.Texture2D, textureID)

    ()

