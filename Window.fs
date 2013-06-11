namespace global

open System
open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL


/// Main program window.
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "BattleSoup")
    let camera = Camera (Point (0.0, 0.0), -4.0)
    let mutable visual =
        let getDrawingColors (primary : Color) =
            let highlight = primary.Desaturate (Color (0.6, 0.6, 0.6), Color (1.0, 1.0, 1.0))
            let body = primary.Desaturate (Color (0.4, 0.4, 0.4), Color (0.7, 0.7, 0.7))
            (highlight, body)
        let color = Color (0.0, 0.0, 1.0)
        let radius = 0.5
        let borderWidth = 0.1
        let elementFont = Font.byName "Verdana" |? Font.generic
        let symbol = "H"
        let draw (g : System.Drawing.Graphics) =
            let highlight, body = getDrawingColors color
            let highlight, body = highlight.ToSystemColor, body.ToSystemColor
            let radius = float32 radius
            let diameter = radius * 2.0f
            let borderWidth = float32 borderWidth
            use p = new System.Drawing.Pen (highlight, float32 borderWidth)
            use b = new System.Drawing.SolidBrush (body)
            use t = new System.Drawing.SolidBrush (highlight)
            use f = new System.Drawing.Font (elementFont, diameter * 0.3f, System.Drawing.FontStyle.Bold)
            let stringSize = g.MeasureString (symbol, f)
            g.FillEllipse (b, -radius, -radius, diameter, diameter)
            g.DrawEllipse (p, -radius + borderWidth * 0.5f, -radius + borderWidth * 0.5f, diameter - borderWidth, diameter - borderWidth)
            g.DrawString (symbol, f, t, -stringSize.Width * 0.5f, -stringSize.Height * 0.5f)
        Visual.drawing draw (Rectangle (-1.2, -1.2, 1.2, 1.2)) 256

    /// Gets the current transform from viewspace to worldspace coordinates for this window.
    member this.Transform = camera.Transform.Normalize (float this.Width / float this.Height)

    /// Gets the worldspace point at the given window coordinates.
    member this.Project (x : int, y : int) =
        let view = this.Transform
        view.Apply (Point (float x / float this.Width * 2.0 - 1.0, 1.0 - float y / float this.Height * 2.0))

    override this.OnLoad args =
        this.MakeCurrent ()
        this.VSync <- VSyncMode.On
        this.Mouse.WheelChanged.Add (fun args ->
            camera.ZoomTo (float args.DeltaPrecise, this.Project (args.X, args.Y)))

        GL.Enable EnableCap.Texture2D
        GL.Enable EnableCap.ColorMaterial
        GL.Enable EnableCap.CullFace
        GL.Enable EnableCap.Blend
        GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)

    override this.OnRenderFrame args =
        GL.Clear ClearBufferMask.ColorBufferBit
        visual.Render (this.Transform.Inverse, &visual)
        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        let updateTime = 1.0 / 60.0
        camera.Update updateTime