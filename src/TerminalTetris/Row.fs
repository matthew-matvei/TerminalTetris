namespace TerminalTetris

type Row = bool[]

module Row =

    let copy (row: Row) =
        Array.copy row

    let isFull (row: Row) =
        Array.forall id row
