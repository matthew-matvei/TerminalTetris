module GameGrid

type private Row = bool[]

type Grid =
    { Rows: Row[] }

let create numRows numColumns = { Rows = Array.create numRows (Array.create numColumns false)}

let private renderRow (row: Row) =
    Array.concat [
        [| "|" |]
        Array.map (fun column -> if column then "X" else " ") row
        [| "|" |]
    ]

let render (grid: Grid) =
    Array.concat [
        Array.map renderRow grid.Rows
        [| Array.create (grid.Rows.[0].Length + 2) "=" |]
    ]
