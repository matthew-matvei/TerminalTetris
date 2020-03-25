module Score

let mutable private _currentScore = 0u

let render (score: uint32) =
    "Score: " + score.ToString()

let currentScore _ =
    _currentScore

let incrementByRowsCleared (rowsCleared: uint32) =
    _currentScore <- _currentScore + rowsCleared
