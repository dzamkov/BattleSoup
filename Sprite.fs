module BattleSoup.Sprite

open System
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Drawing
open System.Drawing.Drawing2D
open BattleSoup.Geometry
open BattleSoup.Render

/// A renderable fragment from a texture with included positioning and sizing information.
type Sprite (texture : Texture, source : Rectangle, destination : Rectangle) =
    
    /// Gets the texture for this sprite.
    member this.Texture = texture

    /// Gets the source quadrilateral for this sprite in the texture space.
    member this.Source = source

    /// Gets the destination quadrilateral for this sprite in view space.
    member this.Destination = destination

/// A method of creating a specific sprite that does not directly reference a texture.
type [<ReferenceEquality>] SpriteSource =
    | Draw of Rectangle * (Graphics -> unit)
    | Sample of Rectangle * (Point -> Paint)

/// Contains information and methods for creating sprites from sprite sources.
type [<AbstractClass>] SpriteFactory () =

    /// Creates a single independent sprite from a sprite source.
    abstract Create : SpriteSource -> Sprite

/// Gets the graphics transformation matrix needed in order to transform drawings from the
/// destination rectangle to the given source rectangle on a bitmap of the given size.
let getTransform (size : int) (source : Rectangle) (destination : Rectangle) =
    let xScale = float size * (source.Max.X - source.Min.X) / (destination.Max.X - destination.Min.X)
    let yScale = float size * (source.Max.Y - source.Min.Y) / (destination.Max.Y - destination.Min.Y)
    let sourceCenter = source.Center * float size
    let destinationCenter = destination.Center
    let xOffset = sourceCenter.X - destinationCenter.X / xScale
    let yOffset = sourceCenter.Y - destinationCenter.Y / yScale
    new Matrix (float32 xScale, 0.0f, 0.0f, float32 yScale, float32 xOffset, float32 yOffset)

/// A simple sprite factory that gives each sprite its own texture with a specific size.
type SimpleSpriteFactory (padding : float, size : int) =
    inherit SpriteFactory ()

    /// Gets the size (width and height) of the textures created with this factory.
    member this.Size = size

    /// Gets the amount of padding applied to a sprite texture.
    member this.Padding = padding

    override this.Create source =
        match source with
        | Draw (destination, draw) ->
            let source = Rectangle (padding * 0.5, padding * 0.5, 1.0 - padding * 0.5, 1.0 - padding * 0.5)
            let b = new Bitmap (size, size)
            use g = Graphics.FromImage b
            g.SmoothingMode <- SmoothingMode.HighQuality
            g.CompositingQuality <- CompositingQuality.HighQuality
            g.TextRenderingHint <- Text.TextRenderingHint.AntiAliasGridFit
            g.Transform <- getTransform size source destination
            draw g
            let texture =Texture.Create b
            Texture.CreateMipmap GenerateMipmapTarget.Texture2D
            Texture.SetFilterMode (TextureTarget.Texture2D, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear)
            Sprite (texture, source, destination)
        | _ -> NotImplementedException () |> raise
