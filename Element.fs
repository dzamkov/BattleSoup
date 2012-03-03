﻿module BattleSoup.Element

open System.Drawing
open BattleSoup.Util
open BattleSoup.Geometry
open BattleSoup.Drawing
open BattleSoup.Sprite
open BattleSoup.Atom

/// The radius of an element atom.
let elementRadius = 0.5

/// The border width of an element atom.
let borderWidth = 0.1

/// The font for the symbol in an element.
let elementFont = systemFont "Verdana" |? genericFont

/// Gets the two contrasting colors used to draw the body of an element given its primary color.
let getDrawingColors (primary : Color) =
    let highlight = primary.Desaturate (Color (0.6, 0.6, 0.6), Color (1.0, 1.0, 1.0))
    let body = primary.Desaturate (Color (0.4, 0.4, 0.4), Color (0.7, 0.7, 0.7))
    (highlight, body)

/// A type of element.
type ElementType (name : string, symbol : string, mass : float, color : Color) =
    inherit AtomType (elementRadius, mass)
    let drawBody (g : Graphics) =
        let highlight, body = getDrawingColors color
        let highlight, body = systemColor highlight, systemColor body
        let radius = float32 elementRadius
        let diameter = radius * 2.0f
        let borderWidth = float32 borderWidth
        use p = new Pen (highlight, float32 borderWidth)
        use b = new SolidBrush (body)
        use t = new SolidBrush (highlight)
        use f = new Font (elementFont, diameter * 0.3f, FontStyle.Bold)
        let stringSize = g.MeasureString (symbol, f)
        g.FillEllipse (b, -radius, -radius, diameter, diameter)
        g.DrawEllipse (p, -radius + borderWidth * 0.5f, -radius + borderWidth * 0.5f, diameter - borderWidth, diameter - borderWidth)
        g.DrawString (symbol, f, t, -stringSize.Width * 0.5f, -stringSize.Height * 0.5f)
    let bodySource = Draw (Rectangle (-elementRadius, -elementRadius, elementRadius, elementRadius), drawBody)
    let body = SpriteReference bodySource

    /// Gets the full name of this element type.
    member this.Name = name

    /// Gets the symbol for this element type.
    member this.Symbol = symbol

    /// Gets the base color for this element type.
    member this.Color = color

    /// Gets the sprite reference for the body of an atom of this type.
    member this.Body = body

/// The element type for hydrogen.
let hydrogen = ElementType ("Hydrogen", "H", 0.1, Color (0.0, 0.0, 1.0))

/// The element type for carbon.
let carbon = ElementType ("Carbon", "C", 0.3, Color (1.0, 0.0, 0.0))

/// The element type for nitrogen.
let nitrogen = ElementType ("Nitrogen", "N", 0.2, Color (0.0, 1.0, 0.0))

/// The element type for oxygen.
let oxygen = ElementType ("Oxygen", "O", 0.4, Color (1.0, 1.0, 1.0))

/// The element type for sulfur.
let sulfur = ElementType ("Sulfur", "S", 0.8, Color (1.0, 1.0, 0.0))

/// The element type for copper.
let copper = ElementType ("Copper", "Cu", 1.0, Color (1.0, 0.5, 0.0))

/// The element type for iron.
let iron = ElementType ("Iron", "Fe", 1.4, Color (0.5, 0.5, 0.5))