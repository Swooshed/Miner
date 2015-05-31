// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

// this would probably be better done as subtyping
type World<'a> =
    | Dense of 'a[][]
    | Sparse
    | QuadTree

let makeDense xss = Dense xss

let makeSparse xss =
    match world with
        | Sparse -> Sparse
        | Dense arr -> Sparse


type ICharRepresentable =
    abstract member ToChar : unit -> char

type BlockType =
    | Air
    | Stone

type Block(id : BlockType, metadata : unit) =
    interface ICharRepresentable with
        member this.ToChar() =
            match id with
                | Air   -> ' ' // air
                | Stone -> '.' // stone

let air = Block (Air, ())
let stone = Block (Stone, ())
let testWorld =
    Dense [|
            [| air  ; air  ; air   |]
            [| air  ; stone; stone |]
            [| stone; stone; stone |]
          |]



// print a world
let printWorld (world : World<#ICharRepresentable>) =
    match world with
        | Dense arr ->
            let printChar (c:#ICharRepresentable) = c.ToChar() |> printf "%c"
            let printLine lines = do Collections.Array.iter printChar lines
                                     printf "\n"
            Collections.Array.iter printLine arr



[<EntryPoint>]
let main argv = 
    printWorld testWorld
    0 // return an integer exit code

