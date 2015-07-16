module Main

open Miner.Graphics.Game

open Pencil.Gaming

[<EntryPoint>]
let main argv =
    let game = Game()
    do game.Run()
    0