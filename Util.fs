[<AutoOpen>]
module Util

/// A time or difference of times in seconds.
type Time = float

/// Compares two objects by reference.
let (=*) a b = obj.ReferenceEquals (a, b)

/// Compares two objects by reference.
let (<>*) a b = not (obj.ReferenceEquals (a, b))

/// Determines whether an object is null.
let isNull x = obj.ReferenceEquals (x, null)

/// The option-coalescing operator.
let (|?) a b =
    match a with
    | Some x -> x
    | None -> b

/// The option-coalescing operator.
let (|?|) a b =
    match a with
    | Some x -> Some x
    | None -> b

/// A random source.
let random = System.Random ()