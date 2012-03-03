module BattleSoup.Render

open System.Drawing
open System.Drawing.Imaging
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open BattleSoup.Geometry
open BattleSoup.Drawing

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

    /// Creates and binds a 2d texture from the given BGRA32 bitmap.
    static member Create (bitmap : Bitmap) =
        let texture = Texture.Create ()
        let width = bitmap.Width
        let height = bitmap.Height
        let area = System.Drawing.Rectangle (0, 0, width, height)
        let bd = bitmap.LockBits (area, ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb)
        GL.TexImage2D (TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, bd.Scan0)
        bitmap.UnlockBits bd
        texture

    /// Creates and binds a 2d texture from the given BGRA32 pixel data.
    static member Create (width : int, height : int, data : byte[]) =
        let texture = Texture.Create ()
        GL.TexImage2D (TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, PixelFormat.Bgra, PixelType.UnsignedByte, data)
        texture

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

    static member Color3 (color : Color) =
        GL.Color3 (color.R, color.G, color.B)

    static member Color4 (paint : Paint) =
        let color = paint.Color
        GL.Color4 (paint.Alpha, color.R, color.G, color.B)

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