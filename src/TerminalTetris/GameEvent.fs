namespace TerminalTetris

type GameEventArgs =
    RowsCleared of int

type GameEvent = Event<GameEventArgs>
