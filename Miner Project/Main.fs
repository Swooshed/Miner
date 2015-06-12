module Main

open Miner.Render

open Pencil.Gaming

[<EntryPoint>]
let main argv =
    let game = new Game()
    do game.Run()
    0