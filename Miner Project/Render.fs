module Miner.Render

open Miner.SparseVoxelOctree
open Miner.LoadShaders

open Pencil.Gaming
open Pencil.Gaming.Graphics 

let bufferSize (arr : float32[]) = nativeint (sizeof<float32> * arr.Length)

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
        GL.Enable(EnableCap.DepthTest)
        GL.DepthFunc(DepthFunction.Less)

    // Set up VBOs
    let vertexArrayID = GL.GenVertexArray ()
    let vertexBufferID = GL.GenBuffer ()
    let programID = loadShaders "SimpleVertexShader.glsl" "SimpleFragmentShader.glsl"
    let vertexBufferData = [|
            -1.0f;-1.0f;-1.0f; // triangle 1 : begin
            -1.0f;-1.0f; 1.0f;
            -1.0f; 1.0f; 1.0f; // triangle 1 : end
             1.0f; 1.0f;-1.0f; // triangle 2 : begin
            -1.0f;-1.0f;-1.0f;
            -1.0f; 1.0f;-1.0f; // triangle 2 : end
             1.0f;-1.0f; 1.0f;
            -1.0f;-1.0f;-1.0f;
             1.0f;-1.0f;-1.0f;
             1.0f; 1.0f;-1.0f;
             1.0f;-1.0f;-1.0f;
            -1.0f;-1.0f;-1.0f;
            -1.0f;-1.0f;-1.0f;
            -1.0f; 1.0f; 1.0f;
            -1.0f; 1.0f;-1.0f;
             1.0f;-1.0f; 1.0f;
            -1.0f;-1.0f; 1.0f;
            -1.0f;-1.0f;-1.0f;
            -1.0f; 1.0f; 1.0f;
            -1.0f;-1.0f; 1.0f;
             1.0f;-1.0f; 1.0f;
             1.0f; 1.0f; 1.0f;
             1.0f;-1.0f;-1.0f;
             1.0f; 1.0f;-1.0f;
             1.0f;-1.0f;-1.0f;
             1.0f; 1.0f; 1.0f;
             1.0f;-1.0f; 1.0f;
             1.0f; 1.0f; 1.0f;
             1.0f; 1.0f;-1.0f;
            -1.0f; 1.0f;-1.0f;
             1.0f; 1.0f; 1.0f;
            -1.0f; 1.0f;-1.0f;
            -1.0f; 1.0f; 1.0f;
             1.0f; 1.0f; 1.0f;
            -1.0f; 1.0f; 1.0f;
             1.0f;-1.0f; 1.0f |] : float32[]
    let colourBufferData = [|
        0.583f;  0.771f;  0.014f;
        0.609f;  0.115f;  0.436f;
        0.327f;  0.483f;  0.844f;
        0.822f;  0.569f;  0.201f;
        0.435f;  0.602f;  0.223f;
        0.310f;  0.747f;  0.185f;
        0.597f;  0.770f;  0.761f;
        0.559f;  0.436f;  0.730f;
        0.359f;  0.583f;  0.152f;
        0.483f;  0.596f;  0.789f;
        0.559f;  0.861f;  0.639f;
        0.195f;  0.548f;  0.859f;
        0.014f;  0.184f;  0.576f;
        0.771f;  0.328f;  0.970f;
        0.406f;  0.615f;  0.116f;
        0.676f;  0.977f;  0.133f;
        0.971f;  0.572f;  0.833f;
        0.140f;  0.616f;  0.489f;
        0.997f;  0.513f;  0.064f;
        0.945f;  0.719f;  0.592f;
        0.543f;  0.021f;  0.978f;
        0.279f;  0.317f;  0.505f;
        0.167f;  0.620f;  0.077f;
        0.347f;  0.857f;  0.137f;
        0.055f;  0.953f;  0.042f;
        0.714f;  0.505f;  0.345f;
        0.783f;  0.290f;  0.734f;
        0.722f;  0.645f;  0.174f;
        0.302f;  0.455f;  0.848f;
        0.225f;  0.587f;  0.040f;
        0.517f;  0.713f;  0.338f;
        0.053f;  0.959f;  0.120f;
        0.393f;  0.621f;  0.362f;
        0.673f;  0.211f;  0.457f;
        0.820f;  0.883f;  0.371f;
        0.982f;  0.099f;  0.879f; |] : float32[]

    do  GL.BindVertexArray vertexArrayID
        GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize vertexBufferData, vertexBufferData, BufferUsageHint.StaticDraw)
    let colourBufferID = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, colourBufferID)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize colourBufferData, colourBufferData, BufferUsageHint.StaticDraw)

        
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
        let mutable even = false
        while not (Glfw.WindowShouldClose window) do
            GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            GL.UseProgram programID

            GL.UniformMatrix4(matrixID, false, &mvp)
            GL.EnableVertexAttribArray 0
            GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
            GL.VertexAttribPointer (0, 3, VertexAttribPointerType.Float, false, 0, nativeint 0)

            GL.EnableVertexAttribArray 1
            GL.BindBuffer (BufferTarget.ArrayBuffer, colourBufferID)
            GL.VertexAttribPointer (1, 3, VertexAttribPointerType.Float, false, 0, nativeint 0)

            GL.DrawArrays (BeginMode.Triangles, 0, 12*3)
            GL.DisableVertexAttribArray 0
            GL.DisableVertexAttribArray 1

            Glfw.SwapBuffers window
            Glfw.PollEvents()

