namespace TerminalTetris

[<Struct>]
type GameEventArgs =
    | RowsCleared of int
    | GameOver

type GameEvent = Event<GameEventArgs>
