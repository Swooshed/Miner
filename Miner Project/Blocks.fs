module Miner.Blocks

// TODO: override equality
type Block =
    | Opaque 
    | Translucent 
    | Transparent

let blockInt b = match b with
                   | Opaque -> 0
                   | Translucent -> 1
                   | Transparent -> 2

