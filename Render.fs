module BattleSoup.Render

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open BattleSoup.Geometry

/// An interface to a OpenGL texture.
type Texture (id : int) =

    /// Gets the null texture.
    static member Null = Texture 0

    /// Creates and binds a 2d texture with no associated image data.
    static member Create () =
        let id = GL.GenTexture ()
        GL.BindTexture (TextureTarget.Texture2D, id)
        GL.TexEnv (TextureEnvTarget.TextureEnv, TextureEnvParameter.TextureEnvMode, int TextureEnvMode.Modulate);
        Texture (id)

    /// Creates and binds a 2d texture for the given bitmap.
    static member Create (bitmap : Bitmap) =
        let texture = Texture.Create ()
        let width = bitmap.Width
        let height = bitmap.Height
        let area = System.Drawing.Rectangle (0, 0, width, height)
        let bd = bitmap.LockBits (area, ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        GL.TexImage2D (TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bd.Scan0)
        bitmap.UnlockBits bd
        texture

    /// Creates and binds a 2d texture using the given drawing function.
    static member Create (width : int, height : int, draw) =
        use bitmap = new Bitmap (width, height)
        use graphics = Graphics.FromImage bitmap
        graphics.CompositingQuality <- CompositingQuality.HighQuality
        graphics.SmoothingMode <- SmoothingMode.HighQuality
        draw graphics
        bitmap.Save "test.png"
        Texture.Create bitmap

    /// Sets the wrap mode for the currently-bound texture.
    static member SetWrapMode (target, horizontal : TextureWrapMode, vertical : TextureWrapMode) =
        GL.TexParameter (target, TextureParameterName.TextureWrapS, int horizontal)
        GL.TexParameter (target, TextureParameterName.TextureWrapT, int vertical)

    /// Sets the filter mode for the currently-bound texture.
    static member SetFilterMode (target, min : TextureMinFilter, mag : TextureMagFilter) =
        GL.TexParameter (target, TextureParameterName.TextureMinFilter, int min)
        GL.TexParameter (target, TextureParameterName.TextureMagFilter, int mag)

    /// Creates a mipmap for the currently-bound texture.
    static member CreateMipmap (target) =
        GL.Ext.GenerateMipmap target

    /// Gets the ID for this texture.
    member this.ID = id

    /// Deletes this texture.
    member this.Delete () = GL.DeleteTexture id

/// Defines useful extension functions for the GL class.
type GL with
    static member Vertex2 (point : Point) =
        GL.Vertex2 (point.X, point.Y)

    static member TexCoord2 (point : Point) =
        GL.TexCoord2 (point.X, point.Y)

    static member BindTexture2D (texture : Texture) =
        GL.BindTexture (TextureTarget.Texture2D, texture.ID)

    static member MultMatrix (transform : Transform) =
        let a = transform.X.X
        let b = transform.X.Y
        let c = transform.Y.X
        let d = transform.Y.Y
        let e = transform.Offset.X
        let f = transform.Offset.Y
        let mutable mat = Matrix4d (a, b, 0.0, 0.0, c, d, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, e, f, 0.0, 1.0)
        GL.MultMatrix &mat

/// Normalizes the given view transform so that there is no stretching or skewing when applied to a
/// viewport of the given aspect ratio.
let normalizeView aspectRatio (view : Transform) =
    if aspectRatio < 1.0 then Transform.Scale (aspectRatio, 1.0) * view
    else Transform.Scale (1.0, 1.0 / aspectRatio) * view