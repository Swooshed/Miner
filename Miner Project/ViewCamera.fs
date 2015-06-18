module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

let pi = float32 System.Math.PI
let cos = float >> System.Math.Cos >> float32
let sin = float >> System.Math.Sin >> float32

type ViewCamera (position_, viewOffset_, fov_, speed_) =
    let mutable position   = position_
    let mutable viewOffset = viewOffset_
    let mutable fov        = fov_
    let mutable speed      = speed_

    // FIXME: rotating still not quite working right

    let mutable lastTime = Glfw.GetTime ()
    let mouseSpeed = 0.5f

    member this.Position
        with get()         = position
        and  set position_ = position <- position_
    member this.FOV
        with get()         = fov
        and  set fov_      = fov <- fov_
    member this.Speed
        with get()         = speed
        and  set speed_    = speed <- speed_
    
    new() = ViewCamera (new Vector3 (0.f, 0.f, 5.f),
                        new Vector3 (0.f, 1.f, 3.f),
                        pi/4.f,
                        3.f)

    member this.handleInput window =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        // hold middle mouse to spin camera
        // TODO: on middle mouse press, save cursor position and set it to the center of the screen.
        // TODO: on middle mouse release, restore cursor position
        if Glfw.GetMouseButton (window, MouseButton.RightButton) then
            let mutable xpos = 0.
            let mutable ypos = 0.
            Glfw.GetCursorPos (window, &xpos, &ypos)

            let mutable width = 0
            let mutable height = 0
            Glfw.GetWindowSize(window, &width, &height)

            Glfw.SetCursorPos (window, float (width/2), float (height/2))
            let hAngleDelta = mouseSpeed * dt * (float32 width/2.f - float32 xpos)
            viewOffset <- Vector3.TransformVector (viewOffset, Matrix.CreateRotationY hAngleDelta)
            
        let moveForward = Vector3.Normalize(new Vector3 (-viewOffset.X, 0.f, -viewOffset.Z))
        let moveRight = Vector3.TransformVector (moveForward, Matrix.CreateRotationY (pi/2.f))


        if Glfw.GetKey (window, Key.Up) then
            position <- position + moveForward * dt * speed
        if Glfw.GetKey (window, Key.Down) then
            position <- position - moveForward * dt * speed
        if Glfw.GetKey (window, Key.Left) then
            position <- position + moveRight * dt * speed
        if Glfw.GetKey (window, Key.Right) then
            position <- position - moveRight * dt * speed
        if Glfw.GetKey (window, Key.Q) then
            position <- position + Vector3.UnitY * dt * speed
        if Glfw.GetKey (window, Key.A) then
            position <- position - Vector3.UnitY * dt * speed

        

        // TODO: scroll wheel to move the camera up or down

        let proj = MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        let view = MathUtils.Matrix.LookAt (position + viewOffset, position, Vector3.UnitY)


        lastTime <- currentTime

        (proj, view)

