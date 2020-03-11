open System

[<EntryPoint>]
let main _ =
    let exitCode = 0

    Console.Title <- "Terminal Tetris"
    Console.Clear()
    Console.CursorVisible <- false

    let mutable gameGrid = Scene.getGameGridDimensions() |> GameGrid.create

    let updateGrid (gameGridUpdater) =
        gameGrid <- GameGrid.update gameGrid gameGridUpdater
        GameGrid.render gameGrid |> Scene.drawGameGrid
        Block.render gameGrid.NextBlock |> Scene.drawNextBlock

    GameEngine.run (fun _ -> updateGrid Move.blockDown)
    GameEngine.waitForKey (Keys.applyKeyEvent >> updateGrid)

    exitCode
