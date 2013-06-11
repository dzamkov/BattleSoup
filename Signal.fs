namespace global

/// A reference to a time-varying value.
type [<AbstractClass>] Signal<'a> () =

    /// Gets the current value of the signal.
    abstract Current : 'a

/// A reference to a time-varying value.
type 'a signal = Signal<'a>

/// Contains functions and types related to signals.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Signal =

    /// A signal with a constant value.
    type Const<'a> (value : 'a) =
        inherit Signal<'a> ()
        override this.Current = value

    /// Creates a signal with a constant value.
    let ``const`` value = Const value :> 'a signal

    /// Creates a signal which gets its value from the given generator
    /// function.
    let source gen : 'a signal = 
        { new Signal<'a> () with 
            override this.Current = gen () }

    /// Creates a signal which maps values from a source signal. The output of
    /// the mapping should depend solely on the input.
    let map map (source : 'a signal) : 'b signal =
        match source with
        | :? Const<'a> -> ``const`` (map source.Current)
        | _ -> { new Signal<'b> () with 
            override this.Current = map source.Current }

    /// Creates a signal which maps values from two source signals. The output of
    /// the mapping should depend solely on the input.
    let map2 map (a : 'a signal) (b : 'b signal) : 'c signal = 
        { new Signal<'c> () with 
            override this.Current = map a.Current b.Current }

    /// Creates a signal which maps values from three source signals. The output of
    /// the mapping should depend solely on the input.
    let map3 map (a : 'a signal) (b : 'b signal) (c : 'b signal) : 'c signal = 
        { new Signal<'c> () with 
            override this.Current = map a.Current b.Current c.Current }