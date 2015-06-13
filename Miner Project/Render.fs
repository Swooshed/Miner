module Miner.Render

open Miner.SparseVoxelOctree
open Miner.LoadShaders
open Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.Graphics 

let bufferSize (arr : 'T[]) = nativeint (sizeof<'T> * arr.Length)
let pi = float32 System.Math.PI

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
        1.0f;-1.0f; 1.0f; |] : float32[]

let uvBufferData = [|
    0.000059f; 1.0f-0.000004f;
    0.000103f; 1.0f-0.336048f;
    0.335973f; 1.0f-0.335903f;
    1.000023f; 1.0f-0.000013f;
    0.667979f; 1.0f-0.335851f;
    0.999958f; 1.0f-0.336064f;
    0.667979f; 1.0f-0.335851f;
    0.336024f; 1.0f-0.671877f;
    0.667969f; 1.0f-0.671889f;
    1.000023f; 1.0f-0.000013f;
    0.668104f; 1.0f-0.000013f;
    0.667979f; 1.0f-0.335851f;
    0.000059f; 1.0f-0.000004f;
    0.335973f; 1.0f-0.335903f;
    0.336098f; 1.0f-0.000071f;
    0.667979f; 1.0f-0.335851f;
    0.335973f; 1.0f-0.335903f;
    0.336024f; 1.0f-0.671877f;
    1.000004f; 1.0f-0.671847f;
    0.999958f; 1.0f-0.336064f;
    0.667979f; 1.0f-0.335851f;
    0.668104f; 1.0f-0.000013f;
    0.335973f; 1.0f-0.335903f;
    0.667979f; 1.0f-0.335851f;
    0.335973f; 1.0f-0.335903f;
    0.668104f; 1.0f-0.000013f;
    0.336098f; 1.0f-0.000071f;
    0.000103f; 1.0f-0.336048f;
    0.000004f; 1.0f-0.671870f;
    0.336024f; 1.0f-0.671877f;
    0.000103f; 1.0f-0.336048f;
    0.336024f; 1.0f-0.671877f;
    0.335973f; 1.0f-0.335903f;
    0.667969f; 1.0f-0.671889f;
    1.000004f; 1.0f-0.671847f;
    0.667979f; 1.0f-0.335851f |] : float32[]


type Game () =
    // Initalise GLFW and context
    do  if not (Glfw.Init ()) then raise (new System.Exception "GLFW initialisation failed")
        Glfw.WindowHint(WindowHint.Samples, 4)
        Glfw.WindowHint(WindowHint.ContextVersionMajor, 3)
        Glfw.WindowHint(WindowHint.ContextVersionMinor, 3)
//      Glfw.WindowHint(WindowHint.OpenGLForwardCompat, 1)


    let window = Glfw.CreateWindow(800, 600, "Miner test", GlfwMonitorPtr.Null, GlfwWindowPtr.Null)
    do  if window = GlfwWindowPtr.Null then
            Glfw.Terminate ()
            raise (new System.Exception "Window creation failed")
        Glfw.MakeContextCurrent window
        Glfw.SwapInterval 1

        GL.ClearColor Color4.DarkBlue
        GL.Enable EnableCap.DepthTest
        GL.DepthFunc DepthFunction.Less
        GL.Enable EnableCap.CullFace
    let camera = new ViewCamera.ViewCamera()

    // Set up VBOs
    let vertexArrayID = GL.GenVertexArray ()
    do  GL.BindVertexArray vertexArrayID

    let programID = loadShaders "SimpleVertexShader.glsl" "SimpleFragmentShader.glsl"
    let matrixID = GL.GetUniformLocation (programID, "MVP")

    let textureID = GL.Utils.LoadImage "gl_uvmap.bmp"

    let vertexBufferID = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize vertexBufferData, vertexBufferData, BufferUsageHint.StaticDraw)    

    let uvBufferID = GL.GenBuffer ()
    do  GL.BindBuffer (BufferTarget.ArrayBuffer, uvBufferID)
        GL.BufferData (BufferTarget.ArrayBuffer, bufferSize uvBufferData, uvBufferData, BufferUsageHint.StaticDraw)

    interface System.IDisposable with
        member this.Dispose () =
            GL.DeleteBuffer vertexBufferID
            GL.DeleteProgram programID
            GL.DeleteTexture textureID
            GL.DeleteVertexArray vertexArrayID
            Glfw.DestroyWindow window
            Glfw.Terminate ()

    member this.Run () =
        while not (Glfw.WindowShouldClose window) do
            GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            GL.UseProgram programID
            let mutable mvp =
                let (projection, view) = camera.handleInput window

                let model = MathUtils.Matrix.Identity
                model * view * projection // somehow this works where projection * view * model doesn't???

            GL.UniformMatrix4(matrixID, false, &mvp)

            GL.ActiveTexture TextureUnit.Texture0
            GL.BindTexture (TextureTarget.Texture2D, textureID)
            GL.Uniform1 (textureID, 0)

            // vertices
            GL.EnableVertexAttribArray 0
            GL.BindBuffer (BufferTarget.ArrayBuffer, vertexBufferID)
            GL.VertexAttribPointer (0, 3, VertexAttribPointerType.Float, false, 0, nativeint 0)

            // UVs
            GL.EnableVertexAttribArray 1
            GL.BindBuffer (BufferTarget.ArrayBuffer, uvBufferID)
            GL.VertexAttribPointer (1, 2, VertexAttribPointerType.Float, false, 0, nativeint 0)

            //GL.BindTexture (TextureTarget.Texture2D, textureID)

            GL.DrawArrays (BeginMode.Triangles, 0, 12*3)
            GL.DisableVertexAttribArray 0
            GL.DisableVertexAttribArray 1

            Glfw.SwapBuffers window
            Glfw.PollEvents()

