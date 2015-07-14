module Miner.Utils.DoubleEndedList

type DoubleEndedList<'a> (front : 'a list, back : 'a list) =
    let mutable front = front
    let mutable back  = back

    let mergeToFront () = front <- List.append front (List.rev back)
    let mergeToBack  () = back  <- List.append back (List.rev front)

    member this.fromFront () = mergeToFront (); front
    member this.fromBack  () = mergeToBack  (); back

    member this.head () =
        match front with 
        | []   -> mergeToFront (); List.head front
        | x::_ -> x
    member this.last () =
        match back with 
        | []   -> mergeToBack (); List.head back
        | x::_ -> x

    member this.cons x = DoubleEndedList (x::front, back)
    member this.snoc x = DoubleEndedList (front, x::back)

    // Could probably make this a member of IEumerable or IList if needed.
    member this.map f = DoubleEndedList (List.map f front, List.map f back)
    member this.iter f =
        List.iter f front
        List.iter f (List.rev back)

    member this.empty () = front.IsEmpty && back.IsEmpty
    
    new ()     = DoubleEndedList ([]   , [])
    new front  = DoubleEndedList (front, [])

    