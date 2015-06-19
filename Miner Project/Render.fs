module Miner.Render

open Miner.SparseVoxelOctree
open Miner.LoadShaders
open Miner.ObjVBO
open Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.Graphics 

let bufferSize (arr : 'T[]) = nativeint (sizeof<'T> * arr.Length)
let pi = float32 System.Math.PI

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
        //GL.Enable EnableCap.CullFace // FIXME: this seems to cull the wrong face
    let camera = new ViewCamera(window)

    // Set up VBOs
    let vertexArrayID = GL.GenVertexArray ()
    do  GL.BindVertexArray vertexArrayID

    let programID = loadShaders "SimpleVertexShader.glsl" "SimpleFragmentShader.glsl"
    let matrixID = GL.GetUniformLocation (programID, "MVP")
    let textureID = GL.Utils.LoadImage "gl_uvmap.bmp"

    // FIXME: does this call dispose() when it goes out of scope or does this need the use keyword?
    let cube = new ObjVBO ("gl_cube.obj") 

    interface System.IDisposable with
        member this.Dispose () =
            GL.DeleteProgram programID
            GL.DeleteTexture textureID
            GL.DeleteVertexArray vertexArrayID
            Glfw.Terminate ()

    member this.Run () =
        while not (Glfw.WindowShouldClose window) do
            GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            GL.UseProgram programID
            let (projection, view) = camera.handleInput ()
            let vp = view * projection

            // draw first cube
            let model = MathUtils.Matrix.Identity
            let mutable mvp =
                model * vp // somehow this works where projection * view * model doesn't???
            GL.UniformMatrix4(matrixID, false, &mvp)

            GL.ActiveTexture TextureUnit.Texture0
            GL.BindTexture (TextureTarget.Texture2D, textureID)
            GL.Uniform1 (textureID, 0)

            cube.Draw()


            // draw second cube
            let model = MathUtils.Matrix.CreateTranslation(-3.f, 0.f, 0.f)
            let mutable mvp =
                model * vp // somehow this works where projection * view * model doesn't???
            GL.UniformMatrix4(matrixID, false, &mvp)

            cube.Draw ()

            Glfw.SwapBuffers window
            Glfw.PollEvents()

