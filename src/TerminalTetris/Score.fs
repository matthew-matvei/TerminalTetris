module Score

open TerminalTetris

let mutable private _currentScore = 0u
let mutable private _scoreIsFinal = false

let render (score: uint32) =
    "Score: " + score.ToString() + (if _scoreIsFinal then " (Game Over)" else "")

let currentScore _ =
    _currentScore

let private incrementByRowsCleared (rowsCleared: uint32) =
    _currentScore <- _currentScore + rowsCleared

let handleGameEvent (gameEventArgs: GameEventArgs) =
    match gameEventArgs with
    | GameEventArgs.RowsCleared rowsCleared -> uint32 rowsCleared |> incrementByRowsCleared
    | GameEventArgs.GameOver -> _scoreIsFinal <- true
