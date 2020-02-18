module Row

type Row = bool[]

let copy (row: Row) =
    Array.copy row

let isFull (row: Row) =
    Array.forall id row
