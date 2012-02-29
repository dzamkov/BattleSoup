module BattleSoup.Window

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open BattleSoup.World
open BattleSoup.Geometry
open BattleSoup.Render

/// Main program window.
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "BattleSoup")
    let world = World ()
    let view = Transform (Point (0.0, 0.0), Vector (10.0, 0.0), Vector (0.0, 10.0))
    let mutable theta = 0.0

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
        GL.Enable EnableCap.CullFace

    override this.OnRenderFrame args =
        GL.Clear ClearBufferMask.ColorBufferBit

        GL.LoadIdentity ()
        GL.MultMatrix (view * Transform.Rotate (theta * 0.1)).Inverse
        GL.Begin BeginMode.Quads
        for atom in world.Atoms do
            let position = atom.Position
            let radius = atom.Radius
            GL.Vertex2 (position.X - radius, position.Y - radius)
            GL.Vertex2 (position.X + radius, position.Y - radius)
            GL.Vertex2 (position.X + radius, position.Y + radius)
            GL.Vertex2 (position.X - radius, position.Y + radius)
        GL.End ()

        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        world.Update args.Time
        theta <- theta + args.Time