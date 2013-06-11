namespace global

open System
open System.Collections.Generic

/// A vector in two-dimensional space.
type [<Struct>] Vector =
    val public X : float
    val public Y : float
    new (x, y) = { X = x; Y = y }

    /// Adds two vectors.
    static member (+) (a : Vector, b : Vector) =
        Vector (a.X + b.X, a.Y + b.Y)

    /// Subtracts two vectors.
    static member (-) (a : Vector, b : Vector) =
        Vector (a.X - b.X, a.Y - b.Y)

    /// Gets the dot product of two vectors.
    static member (*) (a : Vector, b : Vector) =
        a.X * b.X + a.Y * b.Y

    /// Multiplies a vector by a scalar.
    static member (*) (a : Vector, b : float) =
        Vector (a.X * b, a.Y * b)

    /// Scales a vector by another.
    static member Scale (a : Vector, b : Vector) =
        Vector (a.X * b.X, a.Y * b.Y)

    /// Gets the length of this vector.
    member this.Length = sqrt (this.X * this.X + this.Y * this.Y)

    /// Gets the square of the length of this vector.
    member this.SquareLength = this.X * this.X + this.Y * this.Y

/// A point in two-dimensional space.
type Point = Vector

/// An axis-aligned rectangle in two-dimensional space.
type [<Struct>] Rectangle =
    val public Min : Point
    val public Max : Point
    new (min, max) = { Min = min; Max = max }
    new (minX, minY, maxX, maxY) = { Min = Point (minX, minY); Max = Point (maxX, maxY) }

    /// The unit square with its minimum point at (0, 0).
    static member Unit = Rectangle (0.0, 0.0, 1.0, 1.0)

    /// Gets the width of this rectangle.
    member this.Width = this.Max.X - this.Min.X

    /// Gets the height of this rectangle.
    member this.Height = this.Max.Y - this.Min.Y

    /// Gets a vector representation of the size of this rectangle along both axies.
    member this.Size = Vector (this.Width, this.Height)

    /// Gets the total area of this rectangle.
    member this.Area = (this.Max.X - this.Min.X) * (this.Max.Y - this.Min.Y)

    /// Gets the center of this rectangle.
    member this.Center = Point ((this.Min.X + this.Max.X) / 2.0, (this.Min.Y + this.Max.Y) / 2.0)

    /// Determines whether this rectangle contains the given point.
    member this.Contains (point : Point) =
        point.X >= this.Min.X && point.X <= this.Max.X &&
        point.Y >= this.Min.Y && point.Y <= this.Max.Y

/// An affline transform in two-dimensional space.
type [<Struct>] Transform (offset : Point, x : Vector, y : Vector) =

    /// Gets the identity transform.
    static member Identity = Transform (Point (0.0, 0.0), Vector (1.0, 0.0), Vector (0.0, 1.0))

    /// Composes two transforms to be applied in the order they are given.
    static member (*) (a : Transform, b : Transform) =
        Transform (b.Apply a.Offset, b.ApplyVector a.X, b.ApplyVector a.Y)

    /// Applies a transform to a point.
    static member (*) (a : Transform, b : Point) = a.Apply b

    /// Creates a rotation transform for a certain angle in radians.
    static member Rotate (angle : float) =
        Transform (Point (0.0, 0.0), Vector (cos angle, sin angle), Vector (-(sin angle), cos angle))

    /// Creates a scale transform with independant scale factors for each axis.
    static member Scale (horizontal : float, vertical : float) =
        Transform (Point (0.0, 0.0), Vector (horizontal, 0.0), Vector (0.0, vertical))

    /// Creates a scale transform with the given scale factor.
    static member Scale (amount : float) =
        Transform (Point (0.0, 0.0), Vector (amount, 0.0), Vector (0.0, amount))

    /// Creates a tranlation transform with the given offset.
    static member Translate (offset : Point) =
        Transform (offset, Vector (1.0, 0.0), Vector (0.0, 1.0))

    /// Gets the offset of this transform (the position of the origin when transformed).
    member this.Offset = offset

    /// Gets the x component of this transform (the amount that the horizontal component is multiplied by when transformed).
    member this.X = x

    /// Gets the y component of this transform (the amount that the vertical component is multiplied by when transformed).
    member this.Y = y

    /// Applies this transform to a point.
    member this.Apply (point : Point) = offset + (x * point.X) + (y * point.Y)

    /// Applies this transform to a vector.
    member this.ApplyVector (vector : Vector) = (x * vector.X) + (y * vector.Y)

    /// Gets the determinant of this transform.
    member this.Determinant = (x.X * y.Y) - (x.Y * y.X)

    /// Gets the inverse of this transform.
    member this.Inverse =
        let idet = 1.0 / this.Determinant
        Transform (
            Point ((y.Y * offset.X - y.X * offset.Y) * -idet, (x.Y * offset.X - x.X * offset.Y) * idet),
            Vector (y.Y * idet, x.Y * -idet),
            Vector (y.X * -idet, x.X * idet))

    /// Normalizes this given transform so that there is no stretching or skewing when applied 
    /// as a projection transform to a viewport of the given aspect ratio.
    member this.Normalize (aspectRatio : float) =
        if aspectRatio < 1.0 then Transform.Scale (aspectRatio, 1.0) * this
        else Transform.Scale (1.0, 1.0 / aspectRatio) * this