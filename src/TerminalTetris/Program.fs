namespace TerminalTetris

open System

module Program =

    [<EntryPoint>]
    let main _ =
        let exitCode = 0

        Console.Title <- "Terminal Tetris"
        Console.Clear()
        Console.CursorVisible <- false

        let mutable gameGrid = Scene.getGameGridDimensions() |> GameGrid.create

        GameGrid.addGameEventHandler (fun gameEventArgs ->
            match gameEventArgs with
            | GameEventArgs.RowsCleared _ -> GameEngine.incrementGameSpeed ())
        GameGrid.addGameEventHandler (fun gameEventArgs ->
            match gameEventArgs with
            | GameEventArgs.RowsCleared rowsCleared -> uint32 rowsCleared |> Score.incrementByRowsCleared)

        let updateGrid (gameGridUpdater) =
            gameGrid <- GameGrid.update gameGrid gameGridUpdater
            GameGrid.render gameGrid |> Scene.drawGameGrid
            Block.render gameGrid.NextBlock |> Scene.drawNextBlock
            Score.currentScore() |> Score.render |> Scene.drawScore

        GameEngine.run (fun _ -> updateGrid Move.blockDown)
        GameEngine.waitForKey (Keys.applyKeyEvent >> updateGrid)

        exitCode
