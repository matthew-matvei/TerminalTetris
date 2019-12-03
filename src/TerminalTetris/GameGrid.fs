module GameGrid

type private Row = bool[]
type Grid =
    { Rows: Row[]
      ActiveBlock: Option<Block.Block> }

let create numRows numColumns =
    { Rows = Array.create numRows (Array.create numColumns false)
      ActiveBlock = Option<Block.Block>.None }

let private copy gameGrid = { Rows = Array.copy gameGrid.Rows; ActiveBlock = gameGrid.ActiveBlock }
let update gameGrid (updateFunction: Grid -> Grid) = copy gameGrid |> updateFunction
let addBlock gameGrid (block: Block.Block) = { gameGrid with ActiveBlock = Some(block) }

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
