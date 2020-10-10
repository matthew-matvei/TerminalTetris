namespace TerminalTetris

open System

module Keys =

    let applyKeyEvent (key: ConsoleKeyInfo) gameGrid =
        match key.Key with
        | ConsoleKey.RightArrow -> Move.blockRight gameGrid
        | ConsoleKey.LeftArrow -> Move.blockLeft gameGrid
        | ConsoleKey.DownArrow -> Move.blockDown gameGrid
        | ConsoleKey.Spacebar -> Move.rotateBlock gameGrid
        | ConsoleKey.UpArrow -> Move.rotateBlock gameGrid
        | _ -> gameGrid
