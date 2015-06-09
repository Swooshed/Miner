module Miner.Render

open Miner.SparseVoxelOctree

open Pencil.Gaming
open Pencil.Gaming.Graphics 

type Game () =
    member this.Run () =
        if not (Glfw.Init ()) then raise (new System.Exception "GLFW initialisation failed")
        let mutable window = Glfw.CreateWindow(800, 600, "Miner test", GlfwMonitorPtr.Null, GlfwWindowPtr.Null)
        if window = GlfwWindowPtr.Null then raise (new System.Exception "Window creation failed")

        Glfw.MakeContextCurrent window
        Glfw.SwapInterval 1

        // set error callback
        // set input callback

        while not (Glfw.WindowShouldClose window) do
            let mutable width = 0
            let mutable height = 0
            Glfw.GetFramebufferSize (window, &width, &height)
            let ratio = float width / float height

            GL.Viewport (0, 0, width, height)
            GL.Clear ClearBufferMask.ColorBufferBit

            GL.MatrixMode MatrixMode.Projection
            GL.LoadIdentity ()
            GL.Ortho(-ratio, ratio, -1., 1., 1., -1.)

            GL.MatrixMode MatrixMode.Modelview

            GL.LoadIdentity ()
            GL.Rotate(float32 (Glfw.GetTime()) * 50.f, 0.f, 0.f, 1.f)

            GL.Begin (BeginMode.Triangles)
            GL.Color3 (1.f, 0.f, 0.f)
            GL.Vertex3 (-0.6f, -0.4f, 0.f)
            GL.Color3 (0.f, 1.f, 0.f)
            GL.Vertex3 (0.6f, -0.4f, 0.f)
            GL.Color3 (0.f, 0.f, 1.f)
            GL.Vertex3(0.f, 0.6f, 0.f)
            GL.End()




            Glfw.SwapBuffers(window)
            Glfw.PollEvents()

        Glfw.DestroyWindow window
        Glfw.Terminate ()