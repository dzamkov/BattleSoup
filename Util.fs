module BattleSoup.Util

/// An absolute time in seconds.
type Time = float

/// A difference of times in seconds.
type TimeDelta = float

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