module BattleSoup.Window

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Drawing
open BattleSoup.World
open BattleSoup.Geometry
open BattleSoup.Render

/// Main program window.
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "BattleSoup")
    let world = World ()
    let mutable view = Transform (Point (0.0, 0.0), Vector (10.0, 0.0), Vector (0.0, 10.0))
    let mutable texture = Texture.Null

    override this.OnLoad args =
        for i = 0 to 9 do
            for j = 0 to 9 do
                world.Spawn {
                        Position = Point ((float i - 4.5) * 2.0, (float j - 4.5) * 2.0)
                        Velocity = Vector ((float i - 4.5) * -2.5, (float j - 4.5) * -2.5)
                        Angle = 0.0
                        Rotation = 0.0
                        Mass = 1.0
                        Radius = 0.5
                    }

        this.MakeCurrent ()

        GL.Enable EnableCap.Texture2D
        GL.Enable EnableCap.ColorMaterial
        GL.Enable EnableCap.CullFace
        GL.Enable EnableCap.Blend
        GL.BlendFunc (BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha)
        let draw width height (g : Graphics) =
            g.Clear (Color.Transparent)
            g.FillEllipse (Brushes.White, 0, 0, width, height)
        texture <- Texture.Create (256, 256, draw 256 256)
        Texture.SetFilterMode (TextureTarget.Texture2D, TextureMinFilter.Linear, TextureMagFilter.Linear)


    override this.OnRenderFrame args =
        GL.Clear ClearBufferMask.ColorBufferBit
        let view = normalizeView (float this.Width / float this.Height) view

        GL.BindTexture2D texture
        GL.LoadIdentity ()
        GL.MultMatrix view.Inverse
        GL.Begin BeginMode.Quads
        for atom in world.Atoms do
            let position = atom.Position
            let radius = atom.Radius
            GL.TexCoord2 (0.0, 0.0)
            GL.Vertex2 (position.X - radius, position.Y - radius)
            GL.TexCoord2 (1.0, 0.0)
            GL.Vertex2 (position.X + radius, position.Y - radius)
            GL.TexCoord2 (1.0, 1.0)
            GL.Vertex2 (position.X + radius, position.Y + radius)
            GL.TexCoord2 (0.0, 1.0)
            GL.Vertex2 (position.X - radius, position.Y + radius)
        GL.End ()

        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        world.Update args.Time
        view <- view * Transform.Rotate (args.Time * 0.1)