module BattleSoup.Element

open System.Drawing
open BattleSoup.Util
open BattleSoup.Render
open BattleSoup.Atom

/// The radius of an element atom.
let elementRadius = 0.5

/// A type of element.
type ElementType (name : string, symbol : string, mass : float, color : Color) =
    inherit AtomType (elementRadius, mass)

    /// Gets the full name of this element type.
    member this.Name = name

    /// Gets the symbol for this element type.
    member this.Symbol = symbol

    /// Gets the base color for this element type.
    member this.Color = color

/// The element type for hydrogen.
let hydrogen = ElementType ("Hydrogen", "H", 0.1, Color (0.0, 0.0, 1.0))

/// The element type for carbon.
let carbon = ElementType ("Carbon", "C", 0.3, Color (1.0, 0.0, 0.0))

/// The element type for nitrogen.
let nitrogen = ElementType ("Nitrogen", "N", 0.2, Color (0.0, 1.0, 0.0))

/// The element type for oxygen.
let oxygen = ElementType ("Oxygen", "O", 0.4, Color (1.0, 1.0, 1.0))

/// The radius of an element atom.
let radius = 0.5

/// The mass of an element atom.
let mass = 1.0

/// The border width of an element atom.
let borderWidth = 0.1

/// The font for the symbol in an element.
let elementFont = systemFont "Verdana" |? genericFont

let draw (element : ElementType) (g : Graphics) =
    let baseColor = systemColor element.Color
    let borderColor = systemColor (Color (1.0, 1.0, 1.0))
    let textColor = systemColor (Color (1.0, 1.0, 1.0))
    let radius = float32 elementRadius
    let diameter = radius * 2.0f
    let borderWidth = float32 borderWidth
    use p = new Pen (borderColor, float32 borderWidth)
    use b = new SolidBrush (baseColor)
    use t = new SolidBrush (textColor)
    use f = new Font (elementFont, diameter * 0.3f, FontStyle.Bold)
    let stringSize = g.MeasureString (element.Symbol, f)
    g.FillEllipse (b, -radius, -radius, diameter, diameter)
    g.DrawEllipse (p, -radius + borderWidth * 0.5f, -radius + borderWidth * 0.5f, diameter - borderWidth, diameter - borderWidth)
    g.DrawString (element.Symbol, f, t, -stringSize.Width * 0.5f, -stringSize.Height * 0.5f)