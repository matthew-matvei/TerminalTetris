module GameEvent

type Args =
    RowsCleared of int

type GameEvent = Event<Args>
