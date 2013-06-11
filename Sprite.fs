namespace global

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System
open System.Collections.Generic
open System.Drawing
open System.Drawing.Drawing2D

/// A method of creating a specific sprite that does not directly reference a texture.
type [<ReferenceEquality>] SpriteSource =
    | Draw of Rectangle * (Graphics -> unit)
    | Sample of Rectangle * (Point -> Paint)

/// Contains information and methods for creating sprites from sprite sources.
type [<AbstractClass>] SpriteFactory () =

    /// Creates a single independent sprite from a sprite source and returns a function to
    /// later delete it by removing its associated texture data.
    abstract Create : SpriteSource -> (Sprite * (unit -> unit))

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
            Sprite (texture, source, destination), texture.Delete
        | _ -> NotImplementedException () |> raise

/// A sprite source with an associated sprite created using a statically-defined sprite factory.
type SpriteReference (source : SpriteSource) =
    let mutable info = None
    static let all = List<SpriteReference> ()
    static let mutable factory = SimpleSpriteFactory (0.1, 256)

    /// Gets or sets the sprite factory used with sprite references.
    static member Factory
        with get () = factory
        and set value = 
            for ref in all do ref.Invalidate ()
            factory <- value

    /// Gets a collection of all defined sprite references.
    static member All = all :> seq<SpriteReference>

    /// Gets the source sprite for this sprite reference.
    member this.Source = source

    /// Gets the sprite for this reference, creating it if needed.
    member this.Sprite =
        match info with
        | Some (sprite, _) -> sprite
        | None ->
            let sprite, delete = factory.Create source
            info <- Some (sprite, delete)
            sprite

    /// Forces a new sprite to be created for this sprite reference the next time
    /// it is used.
    member this.Invalidate () = 
        match info with
        | Some (_, delete) ->
            delete ()
            info <- None
        | None -> ()

    /// Deletes this sprite reference.
    member this.Delete () =
        this.Invalidate ()
        all.Remove this