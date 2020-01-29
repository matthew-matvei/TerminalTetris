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

let activeBlockPresent (activeBlock: Block.Block option) rowIndex columnIndex =
    if activeBlock.IsNone then
        false
    else
        ArrayHelpers.tryGet activeBlock.Value.Rows (rowIndex - activeBlock.Value.Location.Y)
        |> Option.bind (fun r -> ArrayHelpers.tryGet r (columnIndex - activeBlock.Value.Location.X))
        |> Option.defaultValue false

let private renderRow (activeBlock: Block.Block option) rowIndex (row: Row) =
    Array.concat [
        [| "|" |]
        Array.mapi (fun columnIndex column -> if column || activeBlockPresent activeBlock rowIndex columnIndex then "X" else " ") row
        [| "|" |]
    ]

let render (grid: Grid) =
    Array.concat [
        Array.mapi (fun index r -> renderRow grid.ActiveBlock index r) grid.Rows
        [| Array.create (grid.Rows.[0].Length + 2) "=" |]
    ]
