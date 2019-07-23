open System.Reactive.Linq
open System

[<EntryPoint>]
let main _ =
    let exitCode = 0

    let (numRows, numColumns) = (10, 5)
    let gameGrid = GameGrid.create numRows numColumns
    GameGrid.render gameGrid |> Draw.matrix

    let mainLoop = Observable.Interval(TimeSpan.FromSeconds(2.0)).Publish()
    use subscription = mainLoop.Subscribe()

    mainLoop.Connect() |> ignore
    mainLoop.Wait() |> ignore

    exitCode
