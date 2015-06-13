module Miner.ViewCamera

open Pencil.Gaming
open Pencil.Gaming.MathUtils

let pi = float32 System.Math.PI
let cos = float >> System.Math.Cos >> float32
let sin = float >> System.Math.Sin >> float32

type ViewCamera (position_, hAngle_, vAngle_, fov_, speed_) =
    let mutable position = position_
    let mutable hAngle   = hAngle_
    let mutable vAngle   = vAngle_
    let mutable fov      = fov_
    let mutable speed    = speed_

    let mutable lastTime = Glfw.GetTime ()

    member this.Position
        with get()         = position
        and  set position_ = position <- position_
    member this.HAngle
        with get()         = hAngle
        and  set hAngle_   = hAngle <- hAngle_
    member this.VAngle
        with get()         = vAngle
        and  set vAngle_   = vAngle <- vAngle_
    member this.FOV
        with get()         = fov
        and  set fov_      = fov <- fov_
    member this.Speed
        with get()         = speed
        and  set speed_    = speed <- speed_
    
    new() = ViewCamera (new Vector3 (0.f, 0.f, 5.f), 3.f, 0.f, pi/4.f, 3.f)

    member this.handleInput window =
        let currentTime = Glfw.GetTime ()
        let dt = float32 (currentTime - lastTime)

        let direction = new Vector3 ( vAngle * sin hAngle, sin vAngle, cos vAngle * cos hAngle)
        let right     = new Vector3 (sin (hAngle - pi/2.f), 0.f, cos (hAngle - pi/2.f))
        let up        = Vector3.Cross (right, direction)

        if Glfw.GetKey (window, Key.Up) then
            position <- position + direction * dt * speed
        if Glfw.GetKey (window, Key.Down) then
            position <- position - direction * dt * speed
        if Glfw.GetKey (window, Key.Left) then
            position <- position - right * dt * speed
        if Glfw.GetKey (window, Key.Right) then
            position <- position + right * dt * speed

        // TODO: hold middle mouse to spin camera
        if Glfw.GetMouseButton (window, MouseButton.MiddleButton) then
            // work out horizontal movement since last dt
            // spin the camera around the vertical axis
            ()

        // TODO: scroll wheel to move the camera up or down

        let proj = MathUtils.Matrix.CreatePerspectiveFieldOfView (fov, 4.f/3.f, 0.1f, 100.f)
        let view = MathUtils.Matrix.LookAt (position, position+direction, up)


        lastTime <- currentTime

        (proj, view)

