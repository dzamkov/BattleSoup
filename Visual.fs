module BattleSoup.Visual

open BattleSoup.Util
open BattleSoup.Geometry
open BattleSoup.Drawing
open BattleSoup.Sprite

/// A context used for drawing.
type Context (transform : Transform) =

    /// Gets the current transform (from worldspace to viewspace) for this context.
    member this.Transform = transform

    /// Creates a transformed form of this context.
    member this.ApplyTransform trans =
        Context (trans * transform)

/// A purely visual effect in a world.
type [<AbstractClass>] Visual () =

    /// Draws this visual to the current GL context.
    abstract Draw : Context -> unit

    /// Updates the state of this visual by the given amount of time. The given
    /// visual reference can be used to defer to another visual.
    abstract Update : TimeDelta * Visual byref -> unit
    default this.Update (_, _) = ()

    /// Gets the null visual, a visual which does nothing upon drawing.
    static member Null = NullVisual.Instance :> Visual

    /// Composes two visuals.
    static member (+) (under : Visual, over : Visual) =
        if under = Visual.Null then over
        elif over = Visual.Null then under
        else ComposeVisual (under, over) :> Visual

    /// Applies a static transform to the given visual.
    static member (*) (transform : Transform, inner : Visual) =
        if inner = Visual.Null then Visual.Null
        else TransformVisual (transform, inner) :> Visual

/// A compound visual created by drawing one visual over another.
and ComposeVisual (under : Visual, over : Visual) =
    inherit Visual ()
    let mutable under = under
    let mutable over = over

    /// Gets the first visual drawn.
    member this.Under = under

    /// Gets the second visual drawn.
    member this.Over = over

    override this.Draw context =
        under.Draw context
        over.Draw context

    override this.Update (time, visual) =
        under.Update (time, &under)
        over.Update (time, &over)
        if under = Visual.Null then visual <- over
        elif over = Visual.Null then visual <- under

/// A visual that draws an inner visual with the given static transform applied.
and TransformVisual (transform : Transform, inner : Visual) =
    inherit Visual ()
    let mutable inner = inner

    /// Gets the transform applied by this visual.
    member this.Transform = transform

    /// Gets the inner visual for this visual.
    member this.Inner = inner

    override this.Draw context = inner.Draw (context.ApplyTransform transform)

    override this.Update (time, visual) =
        inner.Update (time, &inner)
        if inner = Visual.Null then visual <- Visual.Null

/// A visual that does nothing upon drawing.
and NullVisual private () =
    inherit Visual ()
    static let instance = NullVisual ()

    /// Gets the only instance of this class.
    static member Instance = instance

    override this.Draw _ = ()
    
/// A visual that displays a static sprite from a sprite reference.
type SpriteVisual (sprite : SpriteReference) =
    inherit Visual ()
    new (spriteSource) = SpriteVisual (SpriteReference spriteSource)
    override this.Draw context = sprite.Sprite.Draw (Paint.White, context.Transform)