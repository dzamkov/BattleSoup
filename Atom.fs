module BattleSoup.Atom

open System
open System.Collections.Generic
open BattleSoup.Util
open BattleSoup.Geometry

/// Defines the immutable behavior of a certain type of atom.
type AtomType (radius : float, mass : float) =
    
    /// Gets the normal radius of an atom of this type.
    member this.Radius = radius

    /// Gets the normal mass of an atom of this type.
    member this.Mass = mass

/// A circular object in the game world that participates in physical interactions and collides and links with
/// other atoms.
type Atom (position : Point, velocity : Vector, atomType : AtomType) =
    let mutable position = position
    let mutable velocity = velocity
    let mutable atomType = atomType

    /// Gets or sets the position of this atom.
    member this.Position
        with get () = position
        and set value = position <- value

    /// Gets or sets the velocity of this atom.
    member this.Velocity
        with get () = velocity
        and set value = velocity <- value

    /// Gets the radius of the atom.
    member this.Radius = atomType.Radius

    /// Gets the mass of the atom.
    member this.Mass = atomType.Mass

    /// Gets or sets the type of this atom.
    member this.Type
        with get () = atomType
        and set value = atomType <- value

/// A game world that contains physical content.
type World () =
    let atoms = List<Atom> ()
    let mutable drag = 1.0

    /// Gets the atoms in this world.
    member this.Atoms = atoms :> seq<Atom>

    /// Spawns the given atom in this world.
    member this.Spawn atom = atoms.Add atom

    /// Gets or sets the drag coefficient for this world. This is used in determining the drag force 
    /// felt by atoms, given by the equation F = radius * velocity ^ 2 * drag.
    member this.Drag 
        with get () = drag
        and set value = drag <- value

    /// Updates the state of this world by the given amount of time.
    member this.Update (time : TimeDelta) =

        // Update individual atoms.
        for atom in atoms do
            let speed = atom.Velocity.Length
            let dragCoefficient = drag * atom.Radius / atom.Mass
            let drag = drag * atom.Radius * atom.Velocity.Length * time / atom.Mass
            atom.Position <- atom.Position + atom.Velocity * time
            atom.Velocity <- atom.Velocity * ((sqrt (1.0 + 4.0 * speed * dragCoefficient * time) - 1.0) / (2.0 * speed * dragCoefficient * time))

        // Collision handling
        for a in atoms do
            for b in atoms do
                if a <> b then
                    let difference = b.Position - a.Position
                    let distance = difference.Length
                    let penetration = a.Radius + b.Radius - distance
                    if penetration > 0.0 then
                        let normal = difference * (1.0 / distance)
                        let ima = 1.0 / a.Mass
                        let imb = 1.0 / b.Mass
                        let seperation = normal * (penetration / (ima + imb))
                        a.Position <- a.Position - seperation * ima
                        b.Position <- b.Position + seperation * imb
                        let impact = (a.Velocity - b.Velocity) * normal
                        if impact > 0.0 then
                            let cor = 0.5
                            let j = (1.0 + cor) * impact / (ima + imb)
                            let impulse = normal * j
                            a.Velocity <- a.Velocity - impulse * ima
                            b.Velocity <- b.Velocity + impulse * imb