module Miner.Render

open Miner.SparseVoxelOctree

open Pencil.Gaming
open Pencil.Gaming.Graphics 

type Game () =
    // Initalise GLFW and context
    do if not (Glfw.Init ()) then raise (new System.Exception "GLFW initialisation failed")
    let window = Glfw.CreateWindow(800, 600, "Miner test", GlfwMonitorPtr.Null, GlfwWindowPtr.Null)
    do  if window = GlfwWindowPtr.Null then
            raise (new System.Exception "Window creation failed")
        Glfw.MakeContextCurrent window
        Glfw.SwapInterval 1

    // Set up VBOs
    let mutable vertexArrayID = 0
    let mutable vertexBufferID = 0
    let vertexBufferData = [|
            -1.f; -1.f; 0.f;
             1.f; -1.f; 0.f;
             0.f;  1.f; 0.f; |] 
    do  GL.GenVertexArrays (1, &vertexArrayID)
        GL.BindVertexArray vertexArrayID

        GL.GenBuffers (1, &vertexBufferID)
        GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
        let bufferSize = nativeint (sizeof<float32> * vertexBufferData.Length)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize, vertexBufferData, BufferUsageHint.StaticDraw)

    interface System.IDisposable with
        member this.Dispose() =
            Glfw.DestroyWindow window
            Glfw.Terminate ()

    member this.Run () =


        // set error callback
        // set input callback

        while not (Glfw.WindowShouldClose window) do
            GL.EnableVertexAttribArray 0
            GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
            GL.VertexAttribPointer (0, 3, VertexAttribPointerType.Float, false, 0, nativeint 0)

            GL.DrawArrays (BeginMode.Triangles, 0, 3)
            GL.DisableVertexAttribArray 0


            Glfw.SwapBuffers window
            Glfw.PollEvents()

