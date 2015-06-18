module Miner.ObjVBO

open Pencil.Gaming.Graphics
open Pencil.Gaming.MathUtils

let bufferSize (arr : 'T[]) = nativeint (sizeof<'T> * arr.Length)

type ObjVBO (path : string) =
    let mutable vertices : Vector4[] = [||]
    let mutable normals  : Vector3[] = [||]
    let mutable uvs      : Vector2[] = [||]
    let mutable indices  : int[]     = [||]

    do  GL.Utils.LoadModel(path, &vertices, &normals, &uvs, &indices, false)
    let modelVBO = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, modelVBO)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize vertices, vertices, BufferUsageHint.StaticDraw)

    let uvVBO = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, uvVBO)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize uvs, uvs, BufferUsageHint.StaticDraw)

    let normalVBO = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, normalVBO)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize normals, normals, BufferUsageHint.StaticDraw)

    let indexVBO = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ElementArrayBuffer, indexVBO)
        GL.BufferData (BufferTarget.ElementArrayBuffer, bufferSize indices, indices, BufferUsageHint.StaticDraw)
    member this.Vertices
        with get () = vertices
    member this.VerticesID
        with get () = modelVBO
    member this.Normals
        with get () = normals
    member this.NormalsID
        with get() = normalVBO
    member this.UVs
        with get () = uvs
    member this.UVsID
        with get() = uvVBO
    member this.Indices
        with get () = indices
    member this.IndicesID
        with get () = indexVBO

    interface System.IDisposable with
        member this.Dispose () =
            let buffers = [| modelVBO; indexVBO; normalVBO; uvVBO; |]
            GL.DeleteBuffers (buffers.Length, buffers)