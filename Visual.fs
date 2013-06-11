namespace global

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// A persistent visual effect in two-dimensional space that can respond to signals.
type [<AbstractClass>] Visual () =

    /// Renders this visual, in its current state, to the current GL context.
    /// The projection transform (from worldspace to viewspace) is given as
    /// an argument. The second parameter can be used to defer to another
    /// visual for future rendering.
    abstract Render : Transform * Visual byref -> unit

/// Contains functions and types related to visual effects.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Visual =

    /// A visual that appears transparent and requires no rendering.
    type Null private () =
        inherit Visual ()
        override this.Render (_, _) = ()

        /// The only instance of this class.
        static member Instance = Null ()

    /// A visual representing the composition of two visuals rendered one after
    /// the other.
    and Compose (under : Visual, over : Visual) =
        inherit Visual ()
        let mutable under = under
        let mutable over = over
        override x.Render (trans, this) =
            under.Render (trans, &under)
            over.Render (trans, &over)
            if under =* Null.Instance then this <- over
            elif over =* Null.Instance then this <- under

        /// The component of this composite visual that is rendered first.
        member this.Under = under

        /// The component of this composite visual that is rendered second.
        member this.Over = over

    /// A visual that appears as a transformed form of the given visual.
    and Transform (transform : global.Transform signal, inner : Visual) =
        inherit Visual ()
        let mutable inner = inner
        override x.Render (trans, this) =
            let transform = transform.Current
            inner.Render (transform * trans, &inner)
            if inner =* Null.Instance then this <- Null.Instance

        /// Gets the transform signal for this visual.
        member this.Transform = transform

    /// A visual that displays a static sprite from a texture.
    type Sprite (texture : Texture, sprite : global.Sprite) =
        inherit Visual ()
        override x.Render (trans, this) =
            GL.Color4 (1.0, 1.0, 1.0, 1.0)
            GL.Begin BeginMode.Quads
            sprite.Output trans
            GL.End ()

        /// The texture for this sprite visual.
        member this.Texture = texture

        /// The sprite information for this sprite visual.
        member this.Sprite = sprite

    /// A visual that appears completely transparent.
    let ``null`` = Null.Instance :> Visual

    /// Constructs a composite visual.
    let compose under over = Compose (under, over) :> Visual

    /// Applies a transform from a signal to a visual.
    let transform transform inner = Transform (transform, inner) :> Visual

    /// Applies a static transform to a visual.
    let transformStatic transform inner = transform (Signal.``const`` transform) inner

    /// Constructs a visual for a sprite.
    let sprite texture sprite = Sprite (texture, sprite) :> Visual