module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

let pi = float32 System.Math.PI
let cos = float >> System.Math.Cos >> float32
let sin = float >> System.Math.Sin >> float32

type ViewCamera (position_, viewOffset_, fov, rotateSpeed, pitchSpeed, panSpeed, savedCursorPos_, window) =
    // FIXME: is this really the best way in F#?
    let mutable position       = position_
    let mutable viewOffset     = viewOffset_
    let mutable savedCursorPos = savedCursorPos_

    let mutable lastTime = Glfw.GetTime ()
    let mouseSpeed = 0.5f

    // TODO: set scrollwheel callback to move up and down
    // TODO: on middle mouse press, save cursor position and set it to the center of the screen.
    // TODO: on middle mouse release, restore cursor position

    new window_ =
        ViewCamera (new Vector3 (0.f, 0.f, 5.f),
                    new Vector3 (0.f, 1.f, 3.f),
                    pi/4.f,
                    3.f,
                    9.f,
                    3.f,
                    None,
                    window_)


    member this.handleInput () =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        // hold right mouse to spin camera horizontally
        // TODO: change vertical angle with vertical mouse movement

        if Glfw.GetMouseButton (window, MouseButton.RightButton) then
            let mutable xpos = 0.
            let mutable ypos = 0.
            Glfw.GetCursorPos (window, &xpos, &ypos)

            let mutable width = 0
            let mutable height = 0
            Glfw.GetWindowSize(window, &width, &height)

            Glfw.SetCursorPos (window, float (width/2), float (height/2))
            let hDelta = mouseSpeed * dt * (float32 width/2.f - float32 xpos)
            let vDelta = mouseSpeed * dt * (float32 height/2.f - float32 ypos)

            viewOffset <- Vector3.TransformVector (viewOffset, Matrix.CreateRotationY hDelta)
            viewOffset.Y <- viewOffset.Y - vDelta
        else
            ()
            
        let moveForward = Vector3.Normalize(new Vector3 (-viewOffset.X, 0.f, -viewOffset.Z))
        let slideLeft = Vector3.TransformVector (moveForward, Matrix.CreateRotationY (pi/2.f))


        if Glfw.GetKey (window, Key.Up) then
            position <- position + moveForward * dt * panSpeed
        if Glfw.GetKey (window, Key.Down) then
            position <- position - moveForward * dt * panSpeed
        if Glfw.GetKey (window, Key.Left) then
            position <- position + slideLeft * dt * panSpeed
        if Glfw.GetKey (window, Key.Right) then
            position <- position - slideLeft * dt * panSpeed
        if Glfw.GetKey (window, Key.Q) then
            position <- position + Vector3.UnitY * dt * panSpeed
        if Glfw.GetKey (window, Key.A) then
            position <- position - Vector3.UnitY * dt * panSpeed

        

        // TODO: scroll wheel to move the camera up or down

        let proj = MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        let view = MathUtils.Matrix.LookAt (position + viewOffset, position, Vector3.UnitY)


        lastTime <- currentTime

        (proj, view)

