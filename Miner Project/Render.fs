module Miner.Render

open System
open System.Drawing
open System.Collections.Generic

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open OpenTK.Input

type Game() =
    inherit GameWindow(800, 600, GraphicsMode.Default, "Miner")

    do base.VSync <- VSyncMode.On

    override o.OnLoad e =
        base.OnLoad e
        GL.ClearColor (0.1f, 0.2f, 0.5f, 0.0f)
        GL.Enable EnableCap.DepthTest

    override o.OnResize e =
        base.OnResize e
        GL.Viewport(base.ClientRectangle.X, base.ClientRectangle.Y, base.ClientRectangle.Width, base.ClientRectangle.Height)
        let mutable projection = Matrix4.CreatePerspectiveFieldOfView(float32 (Math.PI / 4.), float32 base.Width / float32 base.Height, 1.f, 64.f)
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(&projection)

    override o.OnUpdateFrame e =
        base.OnUpdateFrame e
        if Keyboard.GetState().[Key.Escape] then base.Close()
        
    override o.OnRenderFrame(e) =
       base.OnRenderFrame e
       GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
       let mutable modelview = Matrix4.LookAt(Vector3.Zero, Vector3.UnitZ, Vector3.UnitY)
       GL.MatrixMode(MatrixMode.Modelview)
       GL.LoadMatrix(&modelview)

       GL.Begin(BeginMode.Triangles)
       GL.Color3(1.f, 1.f, 0.f); GL.Vertex3(-1.f, -1.f, 4.f)
       GL.Color3(1.f, 0.f, 0.f); GL.Vertex3(1.f, -1.f, 4.f)
       GL.Color3(0.2f, 0.9f, 1.f); GL.Vertex3(0.f, 1.f, 4.f)
       GL.End()

       base.SwapBuffers()