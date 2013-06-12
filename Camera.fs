namespace global

/// A persistent movable view.
type Camera (center : Vector2, velocity : Vector2, zoom : Scalar, zoomVelocity : Scalar) =
    let mutable center = center
    let mutable velocity = velocity
    let mutable zoom = zoom
    let mutable zoomVelocity = zoomVelocity
    let damping = 0.01
    let zoomDamping = 0.01
    let lateralMovement = 0.7
    new (center, zoom) = Camera (center, Vector2.zero, zoom, 0.0)

    /// Gets or sets the position of the center of the camera's view.
    member this.Center
        with get () = center
        and set value = center <- value

    /// Gets or sets the velocity of the camera, in viewspace units per second.
    member this.Velocity
        with get () = velocity
        and set value = velocity <- value

    /// Gets or sets the zoom level of the camera.
    member this.Zoom
        with get () = zoom
        and set value = zoom <- value

    /// Gets or sets the change in the zoom level of the camera per second.
    member this.ZoomVelocity
        with get () = zoomVelocity
        and set value = zoomVelocity <- value

    /// Gets the extent of the camera's view. This is the length from the center of the
    /// camera's view to any of the edges.
    member this.Extent = 2.0 ** -zoom

    /// Gets the viewspace to worldspace transform for the current state of the camera.
    member this.Transform =
        let extent = this.Extent
        Transform2.translate center * Transform2.dilate extent

    /// Updates the state of the camera by the given time.
    member this.Update (time : Time) =
        let extent = this.Extent
        center <- center + velocity * (extent * time)
        velocity <- velocity * (damping ** time)
        zoom <- zoom + zoomVelocity * time
        zoomVelocity <- zoomVelocity * (zoomDamping ** time)

    /// Changes the velocity and zoom velocity of the camera to zoom in or out of the given target
    /// point.
    member this.ZoomTo (amount : Scalar, target : Vector2) =
        let extent = this.Extent
        let dif = target - center
        zoomVelocity <- zoomVelocity + amount
        velocity <- velocity + dif * (amount * lateralMovement / extent)