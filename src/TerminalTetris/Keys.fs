module Keys

open System

let applyKeyEvent (key: ConsoleKeyInfo) (gameGrid: GameGrid.Grid) =
    match key.Key with
    | ConsoleKey.RightArrow -> Move.blockRight gameGrid
    | ConsoleKey.LeftArrow -> Move.blockLeft gameGrid
    | ConsoleKey.DownArrow -> Move.blockDown gameGrid
    | ConsoleKey.Spacebar -> Move.rotateBlock gameGrid
    | _ -> gameGrid
