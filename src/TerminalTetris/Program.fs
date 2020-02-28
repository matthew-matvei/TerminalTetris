open System

[<EntryPoint>]
let main _ =
    let exitCode = 0
    
    Console.Title <- "Terminal Tetris"
    Console.Clear()
    Console.CursorVisible <- false

    let (numRows, numColumns) = (10, 5)
    let mutable gameGrid = GameGrid.create numRows numColumns
    let initialCursorLocation = { 
        Location.X = Console.CursorLeft
        Location.Y = Console.CursorTop }

    let updateGrid (gameGridUpdater) =
        gameGrid <- GameGrid.update gameGrid gameGridUpdater
        GameGrid.render gameGrid |> Draw.matrixAtLocation initialCursorLocation

    GameEngine.run (fun _ -> updateGrid Move.blockDown)
    GameEngine.waitForKey (Keys.applyKeyEvent >> updateGrid)

    exitCode
