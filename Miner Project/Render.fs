module Miner.Render

open Miner.SparseVoxelOctree

open System
open System.Drawing
open System.Collections.Generic

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

let windowWidth  = 800
let windowHeight = 600

let cubeVerts =
    [|-1.f;-1.f; 1.f;
       1.f;-1.f; 1.f;
       1.f; 1.f; 1.f;
      -1.f; 1.f; 1.f;
      -1.f;-1.f;-1.f;
       1.f;-1.f;-1.f;
       1.f; 1.f;-1.f;
      -1.f; 1.f;-1.f; |] : float32[]

let cubeNormals = cubeVerts

let rawColours =
    [| Color.Cyan;
       Color.Cyan;
       Color.DarkCyan;
       Color.DarkCyan;
       Color.Cyan;
       Color.Cyan;
       Color.DarkCyan;
       Color.DarkCyan; |] : Color[]

let colorToRGBA32 (c : Color) =
    int ((c.A <<< 24) ||| (c.B <<< 16) ||| (c.G <<< 8) ||| c.R)

let cubeColours = Array.map colorToRGBA32 rawColours

let cubeIndices =
    [| 0; 1; 2; 2; 3; 0;
       3; 2; 6; 6; 7; 3;
       7; 6; 5; 5; 4; 7; 
       4; 0; 3; 3; 7; 4;
       0; 1; 5; 5; 4; 0;
       1; 5; 6; 6; 2; 1; |]

let mutable colourBufferID  = 0
let mutable normalBufferID  = 0
let mutable vertexBufferID  = 0
let mutable indicesBufferID = 0

let mutable angle = 0.

// Render the contents of an octree as interpreted as a cube between (-1,-1,-1) and (0,0,0)
// For now, each cube is assumed to be the colour of that cube.
(*
let rec renderOctreeVoxels (octree : SparseVoxelOctree<(float * float * float)>) = 
    match octree.GetNodes() with
        |Full (r,g,b) -> GL.Color3 (r,g,b)
                         
                         GL.EnableVertexAttribArray (0)
                         GL.BindBuffer(BufferTarget.ArrayBuffer, vertexBuffer)
                         GL.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, false, 0, 0)
                         GL.DrawArrays(PrimitiveType.Quads, 0, 24)
                         GL.DisableVertexAttribArray(0)
                         
                         (*
                         GL.Begin(PrimitiveType.Quads)
                         Array.map (function i -> let (x, y, z) = cubeVertsTriples.[i] in GL.Vertex3 (x, y, z)) cubeFaceIxs |> ignore
                         
                         GL.End()
                         ()

                         *)
                         
                         

        |Subdivided arr -> () // subdivide and recurse
*)    
let testOctree = SparseVoxelOctree((0.,0.,0.), 3u, Full (0.,0.,0.))




type Game() =
    inherit GameWindow(windowWidth, windowHeight, GraphicsMode.Default, "Miner")

    do base.VSync <- VSyncMode.On

    override o.OnLoad e =
        base.OnLoad e
        GL.ClearColor (Color.White)

        GL.GenBuffers(1, &colourBufferID)
        GL.BindBuffer(BufferTarget.ArrayBuffer, colourBufferID)
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint (cubeColours.Length * sizeof<int>), cubeColours, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
        
        GL.GenBuffers(1, &normalBufferID)
        GL.BindBuffer(BufferTarget.ArrayBuffer, normalBufferID)
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint (cubeNormals.Length * sizeof<float32>), cubeNormals, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        GL.GenBuffers(1, &vertexBufferID)
        GL.BindBuffer(BufferTarget.ArrayBuffer, vertexBufferID)
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint (cubeVerts.Length * sizeof<float32>), cubeVerts, BufferUsageHint.DynamicDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)

        GL.GenBuffers(1, &indicesBufferID)
        GL.BindBuffer(BufferTarget.ArrayBuffer, indicesBufferID)
        GL.BufferData(BufferTarget.ArrayBuffer, nativeint (cubeIndices.Length * sizeof<int>), cubeIndices, BufferUsageHint.StaticDraw)
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
    (*
    override o.OnResize e =
        base.OnResize e
        GL.Viewport(base.ClientRectangle.X,
                    base.ClientRectangle.Y,
                    base.ClientRectangle.Width,
                    base.ClientRectangle.Height)
        let fovy = float32 (Math.PI / 4.)
        let aspect = float32 base.Width / float32 base.Height
        let mutable projection =  Matrix4.CreatePerspectiveFieldOfView(fovy, aspect, 1.f, 64.f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)
    *)
    override o.OnUpdateFrame e =
        base.OnUpdateFrame e
        if Keyboard.GetState().[Key.Escape] then base.Close()
        angle <- angle + Math.PI/16.
        
    override o.OnRenderFrame(e) =
       base.OnRenderFrame e
       GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
       GL.Enable EnableCap.DepthTest
       GL.Viewport (0, 0, windowWidth, windowHeight)

       GL.MatrixMode MatrixMode.Projection
       GL.LoadIdentity ()
       let ratio = float32 windowWidth / float32 windowHeight
       let mutable perspective = OpenTK.Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI/4.), ratio, 1.f, 200.f)
       GL.LoadMatrix &perspective

       GL.Translate(0., 0., -5.)
       GL.Rotate (angle, Vector3d.UnitY)

       if vertexBufferID <> 0 && indicesBufferID <> 0 then
        if colourBufferID <> 0 then
            GL.BindBuffer (BufferTarget.ArrayBuffer, colourBufferID)
            GL.ColorPointer (4, ColorPointerType.UnsignedByte, sizeof<int>, nativeint 0)
            GL.EnableClientState ArrayCap.ColorArray

        GL.BindBuffer(BufferTarget.ArrayBuffer, vertexBufferID)
        GL.VertexPointer (3, VertexPointerType.Float, 0, nativeint 0)
        GL.EnableClientState ArrayCap.VertexArray

        GL.BindBuffer (BufferTarget.ElementArrayBuffer, indicesBufferID)
        GL.DrawElements (PrimitiveType.Triangles, cubeIndices.Length, DrawElementsType.UnsignedInt, nativeint 0)


       // renderOctreeVoxels testOctree
       base.SwapBuffers()

