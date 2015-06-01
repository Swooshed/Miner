module Main

open Miner.Render

[<EntryPoint>]
let main argv =
    let game = new Game()
    do game.Run(30.)
    0