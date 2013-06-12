module Program

[<EntryPoint>]
let main args =
    let _ = Vector2.zero // Force Math to initialize

    printfn "t\t\tv\t\tp\t\te"
    printfn "%s" (String.replicate 57 "-")
    let mutable t = 0.0
    let mutable p = 0.0
    let mutable vel = Poly.``const`` 0.0
    while true do
        let mutable err = Poly.``const`` 0.0
        for i = 0 to 2 do
            let accel = Poly.``const`` 1.0 - vel * vel
            let nVel = Poly.ix (Poly.eval0 vel) accel
            err <- Poly.ix 0.0 (Poly.sqr (vel - nVel))
            let dif = Poly.eval1 vel - Poly.eval1 nVel
            let dif = 50000.0 * dif * dif
            vel <- (vel + nVel * dif) / (1.0 + dif)
        let pos = Poly.ix p vel
        let mutable b = 0.0
        while err.[b] < 0.0001 do
            printfn "%f\t%f\t%f\t%f" (b + t) vel.[b] pos.[b] err.[b]
            b <- b + 0.1
        t <- t + b
        p <- pos.[b]
        vel <- Poly.``const`` vel.[b]
        System.Console.ReadKey true |> ignore

    (new Window ()).Run ()
    0