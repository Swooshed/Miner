module Miner.Graphics.Game


open Miner.CubePicker
open Miner.RenderSparseVoxelOctree
open Miner.SparseVoxelOctree
open Miner.Utils.DoubleEndedList
open Miner.Utils.LoadShaders
open Miner.Utils.Misc
open Miner.Utils.ObjVBO
open Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.Graphics 
open Pencil.Gaming.MathUtils

type Game () =
    let mutable disposed = false
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
        //GL.Enable EnableCap.CullFace // FIXME: the winding of the cube is wrong
    

    // Set up VBOs
    let vertexArrayID = GL.GenVertexArray ()
    do  GL.BindVertexArray vertexArrayID

    let programID = loadShaders "Resources/SimpleVertexShader.glsl" "Resources/SimpleFragmentShader.glsl"
    let matrixID = GL.GetUniformLocation (programID, "MVP")

    let renderer = new SVORenderer (matrixID)
    let mutable svo = minimalSVO
    let camera = new ViewCamera(window, svo)

// FIXME: this is crashing when called
//    interface System.IDisposable with
//        member this.Dispose () =
//            GL.DeleteProgram programID
//            GL.DeleteVertexArray vertexArrayID
//            Glfw.Terminate ()
//            disposed <- true
//
//    override this.Finalize () =
//        if not disposed then (this :> System.IDisposable).Dispose()

    member this.Run () =
        while not (Glfw.WindowShouldClose window) do
            GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
            GL.UseProgram programID
            let (projection, view) = camera.handleInput ()
            let vp = view * projection
            let initialModel = MathUtils.Matrix.Identity
            renderer.Draw svo vp

            let pathO = camera.SelectedVoxel
            Option.iter (fun (path:DoubleEndedList<int>) ->
                printf "path = "

                path.iter (fun n -> let (bx, by, bz) = octToBools n
                                    printf "(%i, %i, %i) " (intIf bx) (intIf by) (intIf bz))
                printfn "") pathO


            Glfw.SwapBuffers window
            Glfw.PollEvents ()
