namespace global

open System.Drawing
open System.Drawing.Drawing2D
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
            GL.LoadMatrix trans
            GL.BindTexture2D texture
            GL.Begin BeginMode.Quads
            sprite.Output ()
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

    /// Constructs a visual for a static drawing created using System.Drawing.Graphics. The
    /// drawing will be bounded in the given region in world space and have the specified
    /// texture size. 
    let drawing (draw : Graphics -> unit) (region : global.Rectangle) size =
        use bitmap = new Bitmap (size, size)
        use g = Graphics.FromImage bitmap
        g.SmoothingMode <- SmoothingMode.HighQuality
        g.CompositingQuality <- CompositingQuality.HighQuality
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAliasGridFit
        g.Transform <-
            let xScale = float32 size / float32 region.Width
            let yScale = float32 size / float32 region.Height
            let xOffset = -float32 region.Min.X * xScale
            let yOffset = -float32 region.Min.Y * yScale
            new Matrix (xScale, 0.0f, 0.0f, yScale, xOffset, yOffset)
        draw g
        let texture = Texture.Create bitmap
        Texture.CreateMipmap GenerateMipmapTarget.Texture2D
        Texture.SetFilterMode (TextureTarget.Texture2D, 
            TextureMinFilter.LinearMipmapLinear, 
            TextureMagFilter.Linear)
        sprite texture (new global.Sprite (Rectangle.Unit, region))