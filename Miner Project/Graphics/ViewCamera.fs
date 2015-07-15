module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

open Miner.Blocks
open Miner.CubePicker
open Miner.SparseVoxelOctree
open Miner.Utils.DoubleEndedList
open Miner.Utils.Misc

let pi = float32 System.Math.PI

type ViewCamera (origin, offset, fov, window, svo : SparseVoxelOctree<Option<Block>>) =
    let mutable origin         = origin
    let mutable offset         = offset
    let mutable fov            = fov
    let mutable view           = Matrix.Identity
    let mutable projection     = Matrix.Identity
    let mutable selected       = None : Option<DoubleEndedList<int>>
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
            origin <- origin + Vector3.UnitY * float32 dy * scrollSpeed
    do  Glfw.SetScrollCallback (window, new GlfwScrollFun (scrollCallback)) |> ignore

    // on cameraMoveKey press, save cursor position and set it to the center of the screen.
    // on cameraMoveKey release, restore cursor position
    let mouseButtonCallback window_ button action =
        if button = cameraMoveKey && window_ = window then        
            let width, height = Glfw.GetWindowSize window

            if action = KeyAction.Press then // save cursor position, move to center of the screen
                let xpos, ypos = Glfw.GetCursorPos window // TODO: add a convience method to library
                savedCursorPos <- Some (xpos, ypos)

                // TODO: hide cursor
                Glfw.SetCursorPos (window, float (width/2), float (height/2))

            if action = KeyAction.Release then  // restore cursor position
                match savedCursorPos with
                    | Some (xpos, ypos) -> Glfw.SetCursorPos (window, xpos, ypos)
                    | None              -> raise (new System.InvalidOperationException("cameraMoveKey released before it is pressed"))
    do  Glfw.SetMouseButtonCallback (window, new GlfwMouseButtonFun (mouseButtonCallback)) |> ignore

    let mousePosCallback window_ mouseX mouseY =
        if window_ = window && not (Glfw.GetMouseButton (window, cameraMoveKey)) then
            // Calculate the ray that goes between the view and the mouse cursor
            let rayDirection =
                let width, height = Glfw.GetWindowSize window
                let relativeMouseX = (float32 mouseX/ float32 width - 0.5f) * 2.f
                let relativeMouseY = (float32 mouseY/ float32 height - 0.5f) * 2.f
                let rayStartModel = Vector4 (relativeMouseX, relativeMouseY, -1.f, 1.f)
                let rayEndModel   = Vector4 (relativeMouseX, relativeMouseY,  0.f, 1.f)

                let divByW (v : Vector4) = v / v.W
                let invVP                = Matrix.Invert (view * projection)
                let rayStartWorld = divByW (rayStartModel * invVP)
                let rayEndWorld   = divByW (rayEndModel * invVP)
        

                Vector3.Normalize (Vector3 (rayEndWorld - rayStartWorld))

            printfn "(%f, %f, %f) (%f, %f, %f)" origin.X origin.Y origin.Z rayDirection.X rayDirection.Y rayDirection.Z

            selected <- rayIntersections origin rayDirection svo
    do  Glfw.SetCursorPosCallback (window, new GlfwCursorPosFun (mousePosCallback)) |> ignore

    // defaults for testing
    new (window_, svo) =
        ViewCamera (new Vector3 (0.f, 0.f, 5.f),
                    new Vector3 (0.f, 1.f, 3.f),
                    pi/4.f,
                    window_,
                    svo)

    member this.Origin
        with get () = origin + offset

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
            let hDelta = rotateSpeed * dt * (float32 width/2.f - float32 xpos)
            let vDelta = pitchSpeed * dt * (float32 height/2.f - float32 ypos)

            offset <- Vector3.TransformVector (offset, Matrix.CreateRotationX vDelta)
            offset <- Vector3.TransformVector (offset, Matrix.CreateRotationY hDelta)
            
            // TODO: rotate viewOffset around (rotated) the X axis instead.
            //   OR: maintain a distance of 1 from
            
        let moveForward = Vector3.Normalize(new Vector3 (-offset.X, 0.f, -offset.Z))
        let slideLeft = Vector3.TransformVector (moveForward, Matrix.CreateRotationY (pi/2.f))

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
            origin <- origin + Vector3.UnitY * panMult
        if Glfw.GetKey (window, Key.PageDown) then
            origin <- origin - Vector3.UnitY * panMult


        view <- MathUtils.Matrix.LookAt (origin + offset, origin, Vector3.UnitY)
        projection <- MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        lastTime <- currentTime

        (projection, view)

    member this.SelectedVoxel
        with get () = selected

    member this.ViewMatrix
        with get () = view

    member this.ProjectionMatrix
        with get () = projection

