module BattleSoup.Render

open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open BattleSoup.Geometry

/// Represents a color (with no transparency information).
type [<Struct>] Color (r : float, g : float, b : float) =

    /// Gets a completely white color.
    static member White = new Color (1.0, 1.0, 1.0)

    /// Gets a completely black color.
    static member Black = new Color (0.0, 0.0, 0.0)

    /// Creates a color based on its RGB representation.
    static member RGB (r : float, g : float, b : float) =
        Color (r, g, b)

    /// Creates a color based on its RGB representation.
    static member RGB (r : int, g : int, b : int) =
        Color (float r / 256.0, float g / 256.0, float b / 256.0)

    /// Creates a color based on its RGB representation.
    static member RGB (r : byte, g : byte, b : byte) =
        Color (float r / 256.0, float g / 256.0, float b / 256.0)

    /// Blends two colors using the given amount (between 0.0 and 1.0) to determine
    /// what portion of the final color is the second color.
    static member Blend (a : Color, b : Color, amount : float) =
        let am = 1.0 - amount
        let bm = amount
        Color (a.R * am + b.R * bm, a.G * am + b.G * bm, a.B * am + b.B * bm)

    /// Scales all components in a color by a certain amount.
    static member (*) (a : Color, b : float) =
        Color (a.R * b, a.G * b, a.B * b)

    /// Modulates a color with another color.
    static member (*) (a : Color, b : Color) =
        new Color (a.R * b.R, a.G * b.G, a.B * b.B)

    /// Gets the red component of this color.
    member this.R = r

    /// Gets the green component of this color.
    member this.G = g

    /// Gets the blue component of this color.
    member this.B = b

    /// Gets the byte representation of the red component of this color.
    member this.RByte = byte (r * 255.99)

    /// Gets the byte representation of the green component of this color.
    member this.GByte = byte (g * 255.99)

    /// Gets the byte representation of the blue component of this color.
    member this.BByte = byte (b * 255.99)

    /// Gets the relative lightness of this color between 0.0 and 1.0.
    member this.Lightness = (r + g + b) / 3.0

/// Represents a color with transparency information.
type [<Struct>] Paint (alpha : float, pre : Color) =

    /// Gets a completely white paint.
    static member White = Paint (1.0, Color.White)

    /// Gets a completely black paint.
    static member Black = Paint (1.0, Color.Black)

    /// Gets a completely transparent paint.
    static member Transparent = Paint (0.0, Color.White)

    /// Creates a paint based on its post-multiplied argb representation.
    static member ARGB (a : float, r : float, g : float, b : float) =
        Paint (a, Color (a * r, a * g, a * b))

    /// Creates a paint based on its post-multiplied argb representation.
    static member ARGB (a : int, r : int, g : int, b : int) =
        let a = float a / 256.0
        Paint (a, Color (a * float r / 256.0, a * float g / 256.0, a * float b / 256.0))

    /// Creates a paint based on its post-multiplied argb representation.
    static member ARGB (a : byte, r : byte, g : byte, b : byte) =
        let a = float a / 256.0
        Paint (a, Color (a * float r / 256.0, a * float g / 256.0, a * float b / 256.0))

    /// Creates an opaque paint based on its rgb representation.
    static member RGB (r : float, g : float, b : float) =
        Paint (1.0, Color (r, g, b))

    /// Creates an opaque paint based on its rgb representation.
    static member RGB (r : int, g : int, b : int) =
        Paint (1.0, Color (float r / 256.0, float g / 256.0, float b / 256.0))

    /// Creates an opaque paint based on its rgb representation.
    static member RGB (r : byte, g : byte, b : byte) =
        Paint (1.0, Color (float r / 256.0, float g / 256.0, float b / 256.0))

    /// Creates an opaque paint for the given color.
    static member Opaque (source : Color) =
        Paint (1.0, source)

    /// Creates a paint with the given (post-multiplied) color and alpha.
    static member Make (alpha : float, source : Color) =
        Paint (alpha, source * alpha)

    /// Modulates a paint with a color.
    static member (*) (a : Paint, b : Color) =
        Paint (a.Alpha, a.AdditiveColor * b)

    // Modulates a paint with another paint.
    static member (*) (a : Paint, b : Paint) =
        Paint (a.Alpha * b.Alpha, a.AdditiveColor * b.AdditiveColor)

    /// Blends two paints using the given amount (between 0.0 and 1.0) to determine
    /// what portion of the final paint is the second paint.
    static member Blend (a : Paint, b : Paint, amount : float) =
        let am = 1.0 - amount
        let bm = amount
        let ac : Color = a.AdditiveColor
        let bc : Color = b.AdditiveColor
        Paint (a.Alpha * am + b.Alpha * bm, Color (ac.R * am + bc.R * bm, ac.G * am + bc.G * bm, ac.B * am + bc.B * bm))

    /// Gets the color added by this paint when composited, that is, the actual color multiplied by alpha.
    member this.AdditiveColor = pre

    /// Gets the color this paint appears as, or white if this paint is completely transparent.
    member this.Color = 
        if alpha = 0.0 
        then Color (1.0, 1.0, 1.0)
        else pre * (1.0 / alpha)

    /// Gets the transparency of this paint, with 1.0 indicating fully opaque and 0.0 indicating fully transparent.
    member this.Alpha = alpha

    /// Gets the byte representation of the alpha component of this paint.
    member this.AlphaByte = byte (alpha * 255.99)

    /// Gets wether this paint is fully opaque.
    member this.IsOpaque = (alpha = 1.0)

    /// Gets wether this paint is fully transparent.
    member this.IsTransparent = (alpha = 0.0)

/// Converts a color into a form usable by system graphics operations.
let systemColor (color : Color) = System.Drawing.Color.FromArgb (int color.RByte, int color.GByte, int color.BByte)

/// Converts a paint into a form usable by system graphics operations.
let systemPaint (paint : Paint) =
    let color = paint.Color
    System.Drawing.Color.FromArgb (int paint.AlphaByte, int color.RByte, int color.GByte, int color.BByte)

/// Finds the system font family with the given name, or None if it is not available.
let systemFont (name : string) =
    FontFamily.Families |> Array.tryFind (fun ff -> ff.Name = name)

/// A generic sans serif font.
let genericFont = FontFamily.GenericSansSerif

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

    /// Creates and binds a 2d texture using the given drawing function.
    static member Create (width : int, height : int, draw) =
        use bitmap = new Bitmap (width, height)
        use graphics = Graphics.FromImage bitmap
        graphics.CompositingQuality <- CompositingQuality.HighQuality
        graphics.SmoothingMode <- SmoothingMode.HighQuality
        draw graphics
        let tex = Texture.Create bitmap
        Texture.CreateMipmap (GenerateMipmapTarget.Texture2D)
        Texture.SetFilterMode (TextureTarget.Texture2D, TextureMinFilter.LinearMipmapLinear, TextureMagFilter.Linear)
        tex

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