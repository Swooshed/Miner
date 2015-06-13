module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

let pi = float32 System.Math.PI
let cos = float >> System.Math.Cos >> float32
let sin = float >> System.Math.Sin >> float32

type ViewCamera (position_, vertAngle_, horizAngle_, fov_, speed_) =
    let mutable position = position_
    let mutable vertAngle = vertAngle_
    let mutable horizAngle = horizAngle_
    let mutable fov      = fov_
    let mutable speed    = speed_

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
                        0.f,
                        0.f,
                        pi/4.f, 3.f)

    member this.handleInput window =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        // TODO: hold middle mouse to spin camera
        // TODO: on middle mouse press, save cursor position and set it to the center of the screen.
        // TODO: on middle mouse release, restore cursor position
        if Glfw.GetMouseButton (window, MouseButton.MiddleButton) then
            let mutable xpos = 0.
            let mutable ypos = 0.
            Glfw.GetCursorPos (window, &xpos, &ypos)

            let mutable width = 0
            let mutable height = 0
            Glfw.GetWindowSize(window, &width, &height)

            Glfw.SetCursorPos (window, float (width/2), float (height/2))
            // work out horizontal movement since last dt
            // spin the camera around the vertical axis
            vertAngle <- vertAngle + mouseSpeed * dt * (float32 height/2.f - float32 ypos)
            horizAngle <- horizAngle + mouseSpeed * dt * (float32 width/2.f - float32 xpos)
            
            
        let rotAroundY = Matrix.CreateRotationY horizAngle
        let rotAroundX = Matrix.CreateRotationX vertAngle
            
        let moveForward = Vector3.TransformVector(new Vector3 (0.f, 0.f, -1.f), rotAroundY)
        let lookForward = Vector3.TransformVector(moveForward, rotAroundX)
        let up = Vector3.UnitY
        let moveRight = Vector3.Normalize (Vector3.Cross (moveForward, up))


        if Glfw.GetKey (window, Key.Up) then
            position <- position + moveForward * dt * speed
        if Glfw.GetKey (window, Key.Down) then
            position <- position - moveForward * dt * speed
        if Glfw.GetKey (window, Key.Left) then
            position <- position - moveRight * dt * speed
        if Glfw.GetKey (window, Key.Right) then
            position <- position + moveRight * dt * speed
        if Glfw.GetKey (window, Key.Q) then
            position <- position + up * dt * speed
        if Glfw.GetKey (window, Key.A) then
            position <- position - up * dt * speed

        

        // TODO: scroll wheel to move the camera up or down

        let proj = MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        let view = MathUtils.Matrix.LookAt (position, position + lookForward, up)


        lastTime <- currentTime

        (proj, view)

