module BattleSoup.Window

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

/// Main program window.
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "BattleSoup")

    override this.OnRenderFrame args =
        GL.Clear ClearBufferMask.ColorBufferBit
        this.SwapBuffers ()

    override this.OnUpdateFrame args =
        ()