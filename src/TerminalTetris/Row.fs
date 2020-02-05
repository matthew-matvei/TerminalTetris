module Row

type Row = bool[]

let copy (row: Row) =
    Array.copy row
    