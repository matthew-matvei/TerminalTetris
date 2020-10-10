module Score

open TerminalTetris

type private State =
    { CurrentScore: uint32
      ScoreIsFinal: bool }

let mutable private _state =
    { CurrentScore = 0u
      ScoreIsFinal = false }

let render (score: uint32) =
    "Score: "
    + score.ToString()
    + (if _state.ScoreIsFinal then " (Game Over)" else "")

let currentScore _ = _state.CurrentScore

let private incrementByRowsCleared rowsCleared =
    _state <-
        { _state with
              CurrentScore = _state.CurrentScore + rowsCleared }

let handleGameEvent gameEventArgs =
    match gameEventArgs with
    | RowsCleared rowsCleared -> uint32 rowsCleared |> incrementByRowsCleared
    | GameOver -> _state <- { _state with ScoreIsFinal = true }
