module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

let pi = float32 System.Math.PI
let cos = float >> System.Math.Cos >> float32
let sin = float >> System.Math.Sin >> float32

type ViewCamera (position_, viewOffset_, fov_, window) =
    let mutable position       = position_
    let mutable viewOffset     = viewOffset_
    let mutable fov            = fov_

    let mutable savedCursorPos = None
    let mutable lastTime       = Glfw.GetTime ()


    // constants
    let rotateSpeed = 0.5f
    let pitchSpeed = 2.f
    let panSpeed = 3.f
    let scrollSpeed = 1.f
    let cameraMoveKey = MouseButton.RightButton

    

    // scroll to move up and down
    let scrollCallback window_ dx dy =
        if window_ = window then
            position <- position + Vector3.UnitY * float32 dy * scrollSpeed
    do  Glfw.SetScrollCallback (window, new GlfwScrollFun (scrollCallback)) |> ignore

    // on cameraMoveKey press, save cursor position and set it to the center of the screen.
    // on cameraMoveKey release, restore cursor position
    let mouseButtonCallback window_ button action =

        if button = cameraMoveKey && window_ = window then        
            let mutable width = 0
            let mutable height = 0
            Glfw.GetWindowSize(window, &width, &height) // TODO: add a convience method to library

            if action = KeyAction.Press then // save cursor position, move to center of the screen
                let mutable xpos = 0.
                let mutable ypos = 0.
                Glfw.GetCursorPos (window, &xpos, &ypos) // TODO: add a convience method to library
                savedCursorPos <- Some (xpos, ypos)

                // TODO: hide cursor
                Glfw.SetCursorPos (window, float (width/2), float (height/2))

            if action = KeyAction.Release then  // restore cursor position
                match savedCursorPos with
                    | Some (xpos, ypos) -> Glfw.SetCursorPos (window, xpos, ypos)
                    | None              -> raise (new System.InvalidOperationException("cameraMoveKey released before it is pressed"))
    do  Glfw.SetMouseButtonCallback (window, new GlfwMouseButtonFun (mouseButtonCallback)) |> ignore

    let mousePosCallback window_ xpos ypos =
        if window_ = window && not (Glfw.GetMouseButton (window, cameraMoveKey)) then
            () // TODO: mouse picking code goes here
    do  Glfw.SetCursorPosCallback (window, new GlfwCursorPosFun (mousePosCallback)) |> ignore

    // defaults for testing
    new window_ =
        ViewCamera (new Vector3 (0.f, 0.f, 5.f),
                    new Vector3 (0.f, 1.f, 3.f),
                    pi/4.f,
                    window_)

    member this.handleInput () =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        // when right mouse is held:
        //    horizontal mouse movement will rotate the view
        //    vertical mouse movement will tilt the view
        if Glfw.GetMouseButton (window, cameraMoveKey) then
            let mutable xpos = 0.
            let mutable ypos = 0.
            Glfw.GetCursorPos (window, &xpos, &ypos)

            let mutable width = 0
            let mutable height = 0
            Glfw.GetWindowSize(window, &width, &height)

            Glfw.SetCursorPos (window, float (width/2), float (height/2))
            let hDelta = rotateSpeed * dt * (float32 width/2.f - float32 xpos)
            let vDelta = pitchSpeed * dt * (float32 height/2.f - float32 ypos)

            viewOffset <- Vector3.TransformVector (viewOffset, Matrix.CreateRotationY hDelta)
            viewOffset.Y <- viewOffset.Y - vDelta
            
        let moveForward = Vector3.Normalize(new Vector3 (-viewOffset.X, 0.f, -viewOffset.Z))
        let slideLeft = Vector3.TransformVector (moveForward, Matrix.CreateRotationY (pi/2.f))

        let panMult = dt * panSpeed
        if Glfw.GetKey (window, Key.Up) then
            position <- position + moveForward * panMult
        if Glfw.GetKey (window, Key.Down) then
            position <- position - moveForward * panMult
        if Glfw.GetKey (window, Key.Left) then
            position <- position + slideLeft * panMult
        if Glfw.GetKey (window, Key.Right) then
            position <- position - slideLeft * panMult
        if Glfw.GetKey (window, Key.PageUp) then
            position <- position + Vector3.UnitY * panMult
        if Glfw.GetKey (window, Key.PageDown) then
            position <- position - Vector3.UnitY * panMult


        let proj = MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        let view = MathUtils.Matrix.LookAt (position + viewOffset, position, Vector3.UnitY)


        lastTime <- currentTime

        (proj, view)

