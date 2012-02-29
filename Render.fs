﻿module BattleSoup.Render

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open BattleSoup.Geometry

/// Defines useful extension functions for the GL class.
type GL with
    static member Vertex2 (point : Point) =
        GL.Vertex2 (point.X, point.Y)

    static member TexCoord2 (point : Point) =
        GL.TexCoord2 (point.X, point.Y)

    static member MultMatrix (transform : Transform) =
        let a = transform.X.X
        let b = transform.X.Y
        let c = transform.Y.X
        let d = transform.Y.Y
        let e = transform.Offset.X
        let f = transform.Offset.Y
        let mutable mat = Matrix4d (a, b, 0.0, e, c, d, 0.0, f, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
        GL.MultMatrix &mat