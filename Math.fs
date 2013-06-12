[<AutoOpen>]
module Math

/// A vector space over objects of the given type.
type [<AbstractClass; AllowNullLiteral>] VectorSpace<'t> () =
    static let mutable ``default`` = null : VectorSpace<'t>

    /// Gets or sets the default vector space for objects of the given 
    /// vector type.
    static member Default
        with get () = ``default``
        and set value = ``default`` <- value

    /// Scales a vector in this vector space.
    abstract Scale : Scalar -> 't -> 't

    /// Adds two vectors in this vector space. Note that addition
    /// must be commutative.
    abstract Add : 't -> 't -> 't

    /// Adds a vector with a scaled form of another vector.
    abstract AddScaled : 't -> Scalar -> 't -> 't
    default this.AddScaled a scalar b =
        this.Add a (this.Scale scalar b)

    /// Multiplies two vectors in this vector space. Multiplication is
    /// non-commutative and must distribute over addition.
    abstract Multiply : 't -> 't -> 't

    /// Gets the zero vector. This vector is the additive identity
    /// and the result of scaling any vector by 0.
    abstract Zero : 't


/// A number that can multiply vectors.
and Scalar = float
VectorSpace<Scalar>.Default <- 
    { new VectorSpace<Scalar> () with
        override this.Scale a b = a * b
        override this.Add a b = a + b
        override this.AddScaled a s b = a + s * b
        override this.Multiply a b = a * b
        override this.Zero = 0.0 }


/// A vector with two components.
type [<Struct>] Vector2 =
    val public X : Scalar
    val public Y : Scalar
    new (x, y) = { X = x; Y = y }
    static member (+) (a : Vector2, b : Vector2) =
        Vector2 (a.X + b.X, a.Y + b.Y)
    static member (-) (a : Vector2, b : Vector2) =
        Vector2 (a.X - b.X, a.Y - b.Y)
    static member (~-) (a : Vector2) =
        Vector2 (-a.X, -a.Y)
    static member (*) (a : Vector2, b : Vector2) =
        a.X * b.X + a.Y * b.Y
    static member (*) (a : Vector2, b : Scalar) =
        Vector2 (a.X * b, a.Y * b)
    static member (*) (a : Scalar, b : Vector2) =
        Vector2 (a * b.X, a * b.Y)
    static member (/) (a : Vector2, b : Scalar) =
        Vector2 (a.X / b, a.Y / b)

/// Contains functions related to two-component vectors.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2 =

    /// The zero vector.
    let zero = Vector2 (0.0, 0.0)

    /// The X unit vector.
    let x = Vector2 (1.0, 0.0)

    /// The Y unit vector.
    let y = Vector2 (0.0, 1.0)

    /// The vector space for two-component vectors.
    let space = 
        { new VectorSpace<Vector2> () with
            override this.Scale a b = a * b
            override this.Add a b = a + b
            override this.AddScaled a s b = a + s * b
            override this.Multiply a b = zero
            override this.Zero = zero }

    /// Gets the square of the length of the given vector.
    let sqrLen (vec : Vector2) = vec.X * vec.X + vec.Y * vec.Y

    /// Gets the length of the given vector.
    let len (vec : Vector2) = sqrt (sqrLen vec)

/// Constructs a two-component vector.
let vec2 x y = Vector2 (x, y)
VectorSpace<Vector2>.Default <- Vector2.space


/// A 2x2 matrix.
type [<Struct>] Matrix2 =
    val public X : Vector2
    val public Y : Vector2
    new (x, y) = { X = x; Y = y }
    new (m11, m12, m21, m22) = { X = vec2 m11 m21; Y = vec2 m12 m22 }
    member this.M11 = this.X.X
    member this.M12 = this.Y.X
    member this.M21 = this.X.Y
    member this.M22 = this.Y.Y
    static member (+) (a : Matrix2, b : Matrix2) =
        Matrix2 (a.X + b.X, a.Y + b.Y)
    static member (-) (a : Matrix2, b : Matrix2) =
        Matrix2 (a.X - b.X, a.Y - b.Y)
    static member (~-) (a : Matrix2) =
        Matrix2 (-a.X, -a.Y)
    static member (*) (a : Matrix2, b : Vector2) =
        a.X * b.X + a.Y * b.Y
    static member (*) (a : Matrix2, b : Matrix2) =
        Matrix2 (a * b.X, a * b.Y)
    static member (*) (a : Matrix2, b : Scalar) =
        Matrix2 (a.X * b, a.Y * b)
    static member (*) (a : Scalar, b : Matrix2) =
        Matrix2 (a * b.X, a * b.Y)
    static member (/) (a : Matrix2, b : Scalar) =
        Matrix2 (a.X / b, a.Y / b)

/// Contains functions related to 2x2 matrices.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Matrix2 =

    /// The zero matrix.
    let zero = Matrix2 (Vector2.zero, Vector2.zero)

    /// The identity matrix.
    let identity = Matrix2 (Vector2.x, Vector2.y)

    /// The vector space for 2x2 matrices.
    let space = 
        { new VectorSpace<Matrix2> () with
            override this.Scale a b = a * b
            override this.Add a b = a + b
            override this.AddScaled a s b = a + s * b
            override this.Multiply a b = a * b
            override this.Zero = zero }

    /// Gets the determinant of a matrix.
    let det (mat : Matrix2) = 
        (mat.X.X * mat.Y.Y) - (mat.X.Y * mat.Y.X)

    /// Inverts a matrix.
    let inverse (mat : Matrix2) =
        let idet = 1.0 / det mat
        Matrix2 (mat.Y.Y * idet, mat.Y.X * -idet,
                 mat.X.Y * -idet, mat.X.X * idet)

/// Constructs a 2x2 matrix by its column vectors.
let mat2 x y = Matrix2 (x, y)
VectorSpace<Matrix2>.Default <- Matrix2.space


/// An affine transform for two-component vectors.
type [<Struct>] Transform2 =
    val public Linear : Matrix2
    val public Offset : Vector2
    new (linear, offset) = { Linear = linear; Offset = offset }
    new (x, y, offset) = { Linear = mat2 x y; Offset = offset }
    static member (+) (a : Transform2, b : Transform2) =
        Transform2 (a.Linear + b.Linear, a.Offset + b.Offset)
    static member (-) (a : Transform2, b : Transform2) =
        Transform2 (a.Linear - b.Linear, a.Offset - b.Offset)
    static member (~-) (a : Transform2) =
        Transform2 (-a.Linear, -a.Offset)
    static member (*) (a : Transform2, b : Vector2) =
        a.Linear * b + a.Offset
    static member (*) (a : Transform2, b : Transform2) =
        Transform2 (a.Linear * b.Linear, a * b.Offset)
    static member (*) (a : Transform2, b : Scalar) =
        Transform2 (a.Linear * b, a.Offset * b)
    static member (*) (a : Scalar, b : Transform2) =
        Transform2 (a * b.Linear, a * b.Offset)
    static member (/) (a : Transform2, b : Scalar) =
        Transform2 (a.Linear / b, a.Offset / b)

/// Contains functions related to transform of two-vectors.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Transform2 =

    /// The zero transform.
    let zero = Transform2 (Matrix2.zero, Vector2.zero)

    /// The identity transform.
    let identity = Transform2 (Matrix2.identity, Vector2.zero)

    /// The vector space for transforms of two-vectors.
    let space = 
        { new VectorSpace<Transform2> () with
            override this.Scale a b = a * b
            override this.Add a b = a + b
            override this.AddScaled a s b = a + s * b
            override this.Multiply a b = a * b
            override this.Zero = zero }

    /// Inverts a transform.
    let inverse (trans : Transform2) =
        let iLinear = Matrix2.inverse trans.Linear
        let iOffset = iLinear * -trans.Offset
        Transform2 (iLinear, iOffset)

    /// Constructs a transform that independently scales the axes.
    let scale x y = Transform2 (mat2 (Vector2.x * x) (Vector2.y * y), Vector2.zero)

    /// Constructs a transform that dilates by the given scalar.
    let dilate (amount : Scalar) = Transform2 (Matrix2.identity * amount, Vector2.zero)

    /// Constructs a transform that translates by the given vector.
    let translate offset = Transform2 (Matrix2.identity, offset)

    /// Constructs a transform that normalizes a projection from a viewport
    /// of a given aspect ratio so that there is no stretching or skewing.
    let normalize (aspectRatio : float) =
        if aspectRatio < 1.0 then scale aspectRatio 1.0
        else scale 1.0 (1.0 / aspectRatio)

/// Constructs a two-transform by its linear transform and its
/// offset.
let trans2 linear offset = Transform2 (linear, offset)
VectorSpace<Transform2>.Default <- Transform2.space


/// A variable-degree polynomial of vectors.
type [<Struct>] Poly<'t> =
    val public Coefficients : 't[]
    new (coeffs) = { Coefficients = coeffs }
    member this.Item
        with get param =
            let coeffs = this.Coefficients
            if coeffs.Length > 1 then
                let space = VectorSpace<'t>.Default
                let mutable current = coeffs.[0]
                let mutable scalar = param
                for i = 1 to coeffs.Length - 1 do
                    current <- space.AddScaled current scalar coeffs.[i]
                    scalar <- scalar * param
                current
            elif coeffs.Length = 1 then coeffs.[0]
            else VectorSpace<'t>.Default.Zero
    static member (+) (a : Poly<'t>, b : Poly<'t>) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Add a b
    static member (-) (a : Poly<'t>, b : Poly<'t>) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Add a (space.Scale -1.0 b)
    static member (*) (a : Poly<'t>, b : Poly<'t>) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Multiply a b
    static member (*) (a : Poly<'t>, b : Scalar) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Scale b a
    static member (*) (a : Scalar, b : Poly<'t>) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Scale a b
    static member (/) (a : Poly<'t>, b : Scalar) =
        let space = VectorSpace<Poly<'t>>.Default
        space.Scale (1.0 / b) a

/// A variable-degree polynomial of scalars.
type PolyScalar = Poly<Scalar>

/// A variable-degree polynomial of two-vectors.
type PolyVector2 = Poly<Vector2>

/// A variable-degree polynomial of 2x2 matrices.
type PolyMatrix2 = Poly<Matrix2>

/// A variable-degree polynomial of two-transforms.
type PolyTransform2 = Poly<Transform2>

/// Contains functions related to polynomials.
module Poly =

    /// Gets the coefficients for the given polynomial.
    let inline coeffs (poly : Poly<'t>) = poly.Coefficients

    /// A vector space for a polynomial.
    type Space<'t> (inner : VectorSpace<'t>) =
        inherit VectorSpace<Poly<'t>> ()
        override this.Scale scalar poly =
            let pCoeffs = coeffs poly
            let nCoeffs = Array.zeroCreate pCoeffs.Length
            for i = 0 to pCoeffs.Length - 1 do
                nCoeffs.[i] <- inner.Scale scalar pCoeffs.[i]
            Poly<'t> nCoeffs
        override this.Add a b =
            let add (aCoeffs : 't[]) (bCoeffs : 't[]) =
                let aLen = aCoeffs.Length
                let bLen = bCoeffs.Length
                let nCoeffs = Array.zeroCreate aLen
                Array.blit aCoeffs bLen nCoeffs bLen (aLen - bLen)
                for i = 0 to bLen - 1 do
                    nCoeffs.[i] <- inner.Add aCoeffs.[i] bCoeffs.[i]
                Poly<'t> nCoeffs
            let aCoeffs = coeffs a
            let bCoeffs = coeffs b
            if aCoeffs.Length = 0 then b
            elif bCoeffs.Length = 0 then a
            elif aCoeffs.Length < bCoeffs.Length then add bCoeffs aCoeffs
            else add aCoeffs bCoeffs
        override this.Multiply a b =
            let multiply (aCoeffs : 't[]) (bCoeffs : 't[]) =
                let aLen = aCoeffs.Length
                let bLen = bCoeffs.Length
                let nCoeffs = Array.create (aLen + bLen - 1) inner.Zero
                for i = 0 to aLen - 1 do
                    for j = 0 to bLen - 1 do
                        let k = i + j
                        let product = inner.Multiply aCoeffs.[i] bCoeffs.[j]
                        nCoeffs.[k] <- inner.Add nCoeffs.[k] product
                Poly<'t> nCoeffs
            let aCoeffs = coeffs a
            let bCoeffs = coeffs b
            if aCoeffs.Length = 0 then this.Zero
            elif bCoeffs.Length = 0 then this.Zero
            else multiply aCoeffs bCoeffs
        override this.Zero = Poly<'t> Array.empty

    /// Evaluates a polynomial at a given parameter.
    let inline eval (poly : Poly<'t>) param = poly.[param]

    /// Evaluates the given polynomial at 0.
    let eval0 (poly : Poly<'t>) =
        let coeffs = coeffs poly
        if coeffs.Length > 0 then coeffs.[0]
        else VectorSpace<'t>.Default.Zero

    /// Evaluates the given polynomial at 1.
    let eval1 (poly : Poly<'t>) =
        let coeffs = coeffs poly
        if coeffs.Length > 1 then
            let space = VectorSpace<'t>.Default
            let mutable current = coeffs.[0]
            for i = 1 to coeffs.Length - 1 do
                current <- space.Add current coeffs.[i]
            current
        elif coeffs.Length = 1 then coeffs.[0]
        else VectorSpace<'t>.Default.Zero

    /// Squares a polynomial.
    let inline sqr (poly : Poly<'t>) = poly * poly

    /// Gets the derivative of a polynomial.
    let dx (poly : Poly<'t>) =
        let coeffs = coeffs poly
        if coeffs.Length > 1 then
            let space = VectorSpace<'t>.Default
            let nCoeffs = Array.zeroCreate (coeffs.Length - 1)
            nCoeffs.[0] <- coeffs.[1]
            for i = 2 to coeffs.Length - 1 do
                nCoeffs.[i - 1] <- space.Scale (float i) coeffs.[i]
            Poly<'t> nCoeffs
        else Poly<'t> Array.empty

    /// Gets the integral of a polynomial with the given
    /// constant term.
    let ix (c : 't) (poly : Poly<'t>) =
        let coeffs = coeffs poly
        if coeffs.Length > 0 then
            let space = VectorSpace<'t>.Default
            let nCoeffs = Array.zeroCreate (coeffs.Length + 1)
            nCoeffs.[0] <- c
            for i = 0 to coeffs.Length - 1 do
                nCoeffs.[i + 1] <- space.Scale (1.0 / float (i + 2)) coeffs.[i]
            Poly<'t> nCoeffs
        else Poly<'t> [| c |]

    /// Creates a polynomial which linearly interpolates between the given
    /// values with parameters from 0 to 1.
    let lerp a b =
        let space = VectorSpace<'t>.Default
        Poly<'t> [| a; space.AddScaled b -1.0 a |]

    /// Creates a polynomial with a constant value.
    let ``const`` a = Poly<'t> [| a |]

/// Constructs a polynomial from the given coefficients.
let poly coeffs = Poly<'t> coeffs
VectorSpace<PolyScalar>.Default <- Poly.Space<Scalar> VectorSpace<Scalar>.Default
VectorSpace<PolyVector2>.Default <- Poly.Space<Vector2> Vector2.space
VectorSpace<PolyMatrix2>.Default <- Poly.Space<Matrix2> Matrix2.space
VectorSpace<PolyTransform2>.Default <- Poly.Space<Transform2> Transform2.space