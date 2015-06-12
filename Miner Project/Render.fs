module Miner.Render

open Miner.SparseVoxelOctree
open Miner.LoadShaders

open Pencil.Gaming
open Pencil.Gaming.Graphics 

type Game () =
    // Initalise GLFW and context
    do  if not (Glfw.Init ()) then raise (new System.Exception "GLFW initialisation failed")
        Glfw.WindowHint(WindowHint.Samples, 4)
        Glfw.WindowHint(WindowHint.ContextVersionMajor, 3)
        Glfw.WindowHint(WindowHint.ContextVersionMinor, 3)
        Glfw.WindowHint(WindowHint.OpenGLForwardCompat, 1)
        // FIXME: Glfw.WindowHint(WindowHint.OpenGLProfile, 1) XXX


    let window = Glfw.CreateWindow(800, 600, "Miner test", GlfwMonitorPtr.Null, GlfwWindowPtr.Null)
    do  if window = GlfwWindowPtr.Null then
            raise (new System.Exception "Window creation failed")
        Glfw.MakeContextCurrent window
        Glfw.SwapInterval 1

        GL.ClearColor Color4.DarkBlue

    // Set up VBOs
    let vertexArrayID = GL.GenVertexArray ()
    let vertexBufferID = GL.GenBuffer ()
    let programID = loadShaders "SimpleVertexShader.glsl" "SimpleFragmentShader.glsl"
    let vertexBufferData = [|
             0.f;  1.f; 0.f; 
             1.f; -1.f; 0.f;
            -1.f; -1.f; 0.f;|] 
    do  GL.BindVertexArray vertexArrayID
        GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
        let bufferSize = nativeint (sizeof<float32> * vertexBufferData.Length)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize, vertexBufferData, BufferUsageHint.StaticDraw)

        
    let mutable mvp =
        let projection = MathUtils.Matrix.CreatePerspectiveFieldOfView (
                            float32 System.Math.PI/4.f,
                            4.f / 3.f,
                            0.1f,
                            100.f)

        let view = MathUtils.Matrix.LookAt (new MathUtils.Vector3 (4.f, 4.f, 3.f),
                                            new MathUtils.Vector3 (0.f, 0.f, 0.f),
                                            new MathUtils.Vector3 (0.f, 1.f, 0.f))

        let model = MathUtils.Matrix.Identity
        model * view * projection // somehow this works where projection * view * model doesn't???

    let matrixID = GL.GetUniformLocation (programID, "MVP")

    interface System.IDisposable with
        member this.Dispose () =
            GL.DeleteBuffer vertexBufferID
            GL.DeleteProgram programID
            GL.DeleteVertexArray vertexArrayID
            Glfw.DestroyWindow window
            Glfw.Terminate ()

    member this.Run () =

        while not (Glfw.WindowShouldClose window) do
            GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            GL.UseProgram programID

            GL.UniformMatrix4(matrixID, false, &mvp)
            GL.EnableVertexAttribArray 0
            GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
            GL.VertexAttribPointer (0, 3, VertexAttribPointerType.Float, false, 0, nativeint 0)

            GL.DrawArrays (BeginMode.Triangles, 0, 3)
            GL.DisableVertexAttribArray 0

            Glfw.SwapBuffers window
            Glfw.PollEvents()

