module BattleSoup.Window

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System
open System.Collections.Generic
open System.Drawing
open BattleSoup.Geometry
open BattleSoup.Camera
open BattleSoup.Drawing
open BattleSoup.Render
open BattleSoup.Visual
open BattleSoup.Atom
open BattleSoup.Element
open BattleSoup.Nucleus


/// Main program window.
type Window () =
    inherit GameWindow (640, 480, GraphicsMode.Default, "BattleSoup")
    let world = World ()
    let camera = Camera (Point (0.0, 0.0), -4.0)
    let visuals = Dictionary<Atom, Visual> ()

    /// Gets the current transform from viewspace to worldspace coordinates for this window.
    member this.ViewTransform = normalizeView (float this.Width / float this.Height) camera.Transform

    /// Gets the worldspace point at the given window coordinates.
    member this.Project (x : int, y : int) =
        let view = this.ViewTransform
        view.Apply (Point (float x / float this.Width * 2.0 - 1.0, 1.0 - float y / float this.Height * 2.0))

    override this.OnLoad args =
        let random = Random ()
        let randomElement () = Seq.nth (random.Next 4) [hydrogen; oxygen; carbon; nitrogen]
        for i = 0 to 9 do
            for j = 0 to 9 do
                let position = Point ((float i - 4.5) * 2.0, (float j - 4.5) * 2.0)
                let velocity = Vector ((float i - 4.5) * -2.5, (float j - 4.5) * -2.5)
                world.Spawn (Atom (position, velocity, randomElement ()))
        world.Spawn (Atom (Point (-11.0, 0.0), Vector (0.0, 0.0), NucleusType 0))
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
        
        let context = Context this.ViewTransform.Inverse
        for atom in world.Atoms do
            let mutable visual = Unchecked.defaultof<Visual>
            if visuals.TryGetValue (atom, &visual) then 
                visual.Draw context
            else 
                let visual = atom.Type.GetVisual atom
                visuals.[atom] <- visual
                visual.Draw context

        this.SwapBuffers ()

    override this.OnResize args =
        GL.Viewport (0, 0, this.Width, this.Height)

    override this.OnUpdateFrame args =
        let updateTime = 1.0 / 60.0
        world.Update updateTime
        camera.Update updateTime