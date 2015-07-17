module Miner.ViewCamera

// This is the third person camera for overhead viewing of the world.

open Pencil.Gaming
open Pencil.Gaming.MathUtils

open Miner.Blocks
open Miner.CubePicker
open Miner.SparseVoxelOctree
open Miner.Utils.Misc

let pi = float32 System.Math.PI

type ViewCamera (origin, rho, theta, fov, window, svo : SparseVoxelOctree<Option<Block>>) =
    let mutable origin         = origin
    let mutable rho            = rho
    let mutable theta          = theta
    let mutable fov            = fov
    let mutable view           = Matrix.Identity
    let mutable projection     = Matrix.Identity
    let mutable stack          = None : Option<List<int>>
    let mutable savedCursorPos = None
    let mutable lastTime       = Glfw.GetTime ()

    // constants
    let rotateSpeed   = 1.f
    let pitchSpeed    = 2.f
    let panSpeed      = 3.f
    let scrollSpeed   = 1.f
    let cameraMoveKey = MouseButton.RightButton

    // scroll to move up and down
    let scrollCallback window_ dx dy =
        if window_ = window then
            origin <- origin + Vector4.UnitY * float32 dy * scrollSpeed
    do  Glfw.SetScrollCallback (window, GlfwScrollFun scrollCallback) |> ignore

    // on cameraMoveKey press, save cursor position and set it to the center of the screen.
    // on cameraMoveKey release, restore cursor position
    let mouseButtonCallback window_ button action =
        if button = cameraMoveKey && window_ = window then        
            let width, height = Glfw.GetWindowSize window

            // save cursor position, move to center of the screen
            if action = KeyAction.Press then 
                let xpos, ypos = Glfw.GetCursorPos window
                savedCursorPos <- Some (xpos, ypos)

                // TODO: hide cursor
                Glfw.SetCursorPos (window, float (width/2), float (height/2))

            // restore cursor position
            if action = KeyAction.Release then
                match savedCursorPos with
                    | Some (xpos, ypos) -> Glfw.SetCursorPos (window, xpos, ypos)
                    | None              -> raise (System.InvalidOperationException "cameraMoveKey released before it is pressed")
    do  Glfw.SetMouseButtonCallback (window, GlfwMouseButtonFun mouseButtonCallback) |> ignore

    let mousePosCallback window_ mouseX mouseY =
        if window_ = window && not (Glfw.GetMouseButton (window, cameraMoveKey)) then

            // Calculate the ray that goes between the view and the mouse cursor
            let rayDirection =
                let width, height  = Glfw.GetWindowSize window
                let relativeMouseX = (float32 mouseX/ float32 width - 0.5f) * 2.f
                let relativeMouseY = (float32 mouseY/ float32 height - 0.5f) * 2.f
                let rayStartModel  = Vector4 (relativeMouseX, relativeMouseY, -1.f, 1.f)
                let rayEndModel    = Vector4 (relativeMouseX, relativeMouseY,  0.f, 1.f)
                let normaliseW (v : Vector4) = v / v.W
                let invVP          = Matrix.Invert (view * projection)
                let rayStartWorld  = normaliseW (rayStartModel * invVP)
                let rayEndWorld    = normaliseW (rayEndModel * invVP)
                Vector4.Normalize (Vector4 (rayEndWorld - rayStartWorld))
            printfn "(%f, %f, %f) (%f, %f, %f)" origin.X origin.Y origin.Z rayDirection.X rayDirection.Y rayDirection.Z
            stack <- rayIntersections origin rayDirection svo
    do Glfw.SetCursorPosCallback (window, GlfwCursorPosFun mousePosCallback) |> ignore

    // defaults for testing
    new (window, svo) = ViewCamera (Vector4 (0.f, 0.f, 5.f, 1.f), pi/2.f, pi/2.f, pi/4.f, window, svo)
    new (window, svo, position) = ViewCamera (position, pi/2.f, pi/2.f, pi/4.f, window, svo)

    member this.EyePosition
        with get () = this.Origin + this.EyeOffset
    member this.EyeOffset
        with get () = Vector4 (sin rho * cos theta, cos rho, sin rho * sin theta, 1.f)
    member this.Origin
        with get () = origin
    member this.SelectedVoxel
        with get () = stack
    member this.ViewMatrix
        with get () = view
    member this.ProjectionMatrix
        with get () = projection

    member this.handleInput () =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        // when right mouse is held:
        //    horizontal mouse movement will rotate the view
        //    vertical mouse movement will tilt the view
        if Glfw.GetMouseButton (window, cameraMoveKey) then
            let xpos, ypos = Glfw.GetCursorPos window
            let width, height = Glfw.GetWindowSize window
            Glfw.SetCursorPos (window, float (width/2), float (height/2))

            let newRho = rho + pitchSpeed * dt * (float32 height/2.f - float32 ypos)
            if 0.f < newRho && newRho < pi then rho <- newRho
            theta <- (theta - rotateSpeed * dt * (float32 width/2.f - float32 xpos)) % (2.f*pi)

        let moveForward = Vector4.Normalize(Vector4 (-this.EyeOffset.X, 0.f, -this.EyeOffset.Z, 0.f))
        let slideLeft = moveForward * Matrix.CreateRotationY (pi/2.f)

        // respond to key presses
        let panMult = dt * panSpeed
        if Glfw.GetKey (window, Key.Up) then
            origin <- origin + moveForward * panMult
        if Glfw.GetKey (window, Key.Down) then
            origin <- origin - moveForward * panMult
        if Glfw.GetKey (window, Key.Left) then
            origin <- origin + slideLeft * panMult
        if Glfw.GetKey (window, Key.Right) then
            origin <- origin - slideLeft * panMult
        if Glfw.GetKey (window, Key.PageUp) then
            origin <- origin + Vector4.UnitY * panMult
        if Glfw.GetKey (window, Key.PageDown) then
            origin <- origin - Vector4.UnitY * panMult
                   
        // finish up
        view <- MathUtils.Matrix.LookAt (this.EyePosition.Xyz, this.Origin.Xyz, Vector3.UnitY)
        projection <- MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        lastTime <- currentTime

        (projection, view)



