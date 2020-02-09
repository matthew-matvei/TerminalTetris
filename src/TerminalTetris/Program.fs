open System

[<EntryPoint>]
let main _ =
    let exitCode = 0

    let (numRows, numColumns) = (10, 5)
    let mutable gameGrid = GameGrid.create numRows numColumns

    let updateGrid (gameGridUpdater) =
        gameGrid <- GameGrid.update gameGrid gameGridUpdater
        GameGrid.render gameGrid |> Draw.matrix

    GameEngine.run (fun _ -> updateGrid Move.blockDown)
    GameEngine.waitForKey (Keys.applyKeyEvent >> updateGrid)

    exitCode
