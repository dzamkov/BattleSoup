namespace global

type Bitmap = System.Drawing.Bitmap
open System.Drawing.Imaging
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL


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

/// Contains extensions and functions related to rendering.
[<AutoOpen>]
module Render =

    /// Defines rendering-oriented functions for Transform2.
    type Transform2 with

        /// Gets an OpenTK 4x4 matrix representation of this transform.
        member this.ToGLMatrix =
            let a = this.Linear.X.X
            let b = this.Linear.X.Y
            let c = this.Linear.Y.X
            let d = this.Linear.Y.Y
            let e = this.Offset.X
            let f = this.Offset.Y
            Matrix4d (a,    b,    0.0,  0.0, 
                      c,    d,    0.0,  0.0, 
                      0.0,  0.0,  1.0,  0.0, 
                      e,    f,    0.0,  1.0)

    /// Defines useful extension functions for the GL class.
    type GL with
        static member Vertex2 (point : Math.Vector2) =
            GL.Vertex2 (point.X, point.Y)

        static member TexCoord2 (point : Math.Vector2) =
            GL.TexCoord2 (point.X, point.Y)

        static member Color3 (color : Color) =
            GL.Color3 (color.R, color.G, color.B)

        static member Color4 (paint : Paint) =
            let color = paint.Color
            GL.Color4 (paint.Alpha, color.R, color.G, color.B)

        static member BindTexture2D (texture : Texture) =
            GL.BindTexture (TextureTarget.Texture2D, texture.ID)

        static member MultMatrix (transform : Transform2) =
            let mutable mat = transform.ToGLMatrix
            GL.MultMatrix &mat

        static member LoadMatrix (transform : Transform2) =
            let mutable mat = transform.ToGLMatrix
            GL.LoadMatrix &mat

open Render

/// A renderable fragment from a texture which includes positioning and sizing information.
type Sprite (source : global.Rectangle, destination : global.Rectangle) =

    /// Gets the source quadrilateral for this sprite in the texture space.
    member this.Source = source

    /// Gets the destination quadrilateral for this sprite in view space.
    member this.Destination = destination

    /// Outputs the geometry information (as Quads) for this sprite.
    member this.Output () =
        GL.TexCoord2 (source.Min.X, source.Min.Y)
        GL.Vertex2 (destination.Min.X, destination.Max.Y)
        GL.TexCoord2 (source.Min.X, source.Max.Y)
        GL.Vertex2 (destination.Min.X, destination.Min.Y)
        GL.TexCoord2 (source.Max.X, source.Max.Y)
        GL.Vertex2 (destination.Max.X, destination.Min.Y)
        GL.TexCoord2 (source.Max.X, source.Min.Y)
        GL.Vertex2 (destination.Max.X, destination.Max.Y)

    /// Outputs the geometry information (as Quads) for this sprite, applying the given
    /// transform to the vertex data.
    member this.Output (transform : Transform2) =
        GL.TexCoord2 (source.Min.X, source.Min.Y)
        GL.Vertex2 (transform * vec2 destination.Min.X destination.Max.Y)
        GL.TexCoord2 (source.Min.X, source.Max.Y)
        GL.Vertex2 (transform * vec2 destination.Min.X destination.Min.Y)
        GL.TexCoord2 (source.Max.X, source.Max.Y)
        GL.Vertex2 (transform * vec2 destination.Max.X destination.Min.Y)
        GL.TexCoord2 (source.Max.X, source.Min.Y)
        GL.Vertex2 (transform * vec2 destination.Max.X destination.Max.Y)