module Score

open TerminalTetris

let mutable private _currentScore = 0u

let render (score: uint32) =
    "Score: " + score.ToString()

let currentScore _ =
    _currentScore

let private incrementByRowsCleared (rowsCleared: uint32) =
    _currentScore <- _currentScore + rowsCleared

let handleGameEvent (gameEventArgs: GameEventArgs) =
    match gameEventArgs with
    | GameEventArgs.RowsCleared rowsCleared -> uint32 rowsCleared |> incrementByRowsCleared
    | GameEventArgs.GameOver -> ignore()
