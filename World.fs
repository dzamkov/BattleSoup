module BattleSoup.World


open System
open System.Collections.Generic
open BattleSoup.Geometry

/// An absolute time in seconds.
type Time = float

/// A difference of times in seconds.
type TimeDelta = float

/// A circular object in the game world that participates in physical interactions and collides and links with
/// other atoms.
type Atom = {
        
    /// The position of the atom in the game world.
    mutable Position : Point

    /// The velocity of the atom, in units per second.
    mutable Velocity : Vector

    /// The angle of the atom, in radians.
    mutable Angle : float

    /// The rotation of the atom, in radians per second.
    mutable Rotation : float

    /// The radius of the atom.
    Radius : float

    /// The mass of the atom.
    Mass : float

    }

/// A game world that includes physical and visual content.
type World () =
    let atoms = List<Atom> ()
    let mutable drag = 1.0
    let mutable rotationDamping = 0.9

    /// Gets the atoms in this world.
    member this.Atoms = atoms :> seq<Atom>

    /// Spawns the given atom in this world.
    member this.Spawn atom = atoms.Add atom

    /// Gets or sets the drag coefficient for this world. This is used in determining the drag force 
    /// felt by atoms, given by the equation F = radius * velocity ^ 2 * drag.
    member this.Drag 
        with get () = drag
        and set value = drag <- value

    /// Gets or sets the rotation damping factor for this world. This is the relative amount of rotational velocity
    /// that persists each second. 
    member this.RotationDamping
        with get () = rotationDamping
        and set value = rotationDamping <- value

    /// Updates the state of this world by the given amount of time.
    member this.Update (time : TimeDelta) =

        // Update atoms.
        for atom in atoms do
            let drag = drag * atom.Radius * atom.Velocity.Length * time / atom.Mass
            atom.Position <- atom.Position + atom.Velocity * time
            atom.Velocity <- atom.Velocity * (1.0 - drag)
            atom.Angle <- atom.Angle + atom.Rotation * time
            atom.Rotation <- atom.Rotation * (rotationDamping ** time)
