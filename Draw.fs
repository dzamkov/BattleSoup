namespace global

open System.Drawing

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
        Color (a.R * b.R, a.G * b.G, a.B * b.B)

    /// Adds the components of two colors together to make a new color.
    static member (+) (a : Color, b : Color) =
        Color (a.R + b.R, a.G + b.G, a.B + b.B)

    /// Adds a certain factor to each component in this color.
    static member (+) (a : Color, b : float) =
        Color (a.R + b, a.G + b, a.B + b)

    /// Finds the difference of two colors.
    static member (-) (a : Color, b : Color) =
        Color (a.R - b.R, a.G - b.G, a.B - b.B)

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

    /// Gets the relative lightness of this color between 0.0 and 1.0 adjusted to take
    /// into account the way a human perceives the brightness of different color components.
    member this.AdjustedLightness = (r * 1.3 + g * 1.6 + b) / 5.9

    /// Desaturates this color by scaling all of the color components into the range specified
    /// by the two given colors.
    member this.Desaturate (min : Color, max : Color) =
       this * (max - min) + min

    /// Gets the System.Drawing.Color form of this color.
    member this.ToSystemColor = 
        System.Drawing.Color.FromArgb (int this.RByte, int this.GByte, int this.BByte)

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

    /// Gets the System.Drawing.Color form of this paint.
    member this.ToSystemColor =
        let color = this.Color
        System.Drawing.Color.FromArgb (int this.AlphaByte, int color.RByte, int color.GByte, int color.BByte)

/// Contains functions related to fonts.
module Font =

    /// Gets the font family with the given name, or None if it is not available.
    let byName (name : string) =
        FontFamily.Families |> Array.tryFind (fun ff -> ff.Name = name)

    /// A generic sans serif font family.
    let generic = FontFamily.GenericSansSerif