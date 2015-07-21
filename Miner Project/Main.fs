module Main

open Miner.Graphics.Game
open Miner.Utils.Misc
open Pencil.Gaming.MathUtils

[<EntryPoint>]
let main argv = 
    let game = Game()
    do game.Run()
    //System.Console.ReadLine () |> ignore
    0
