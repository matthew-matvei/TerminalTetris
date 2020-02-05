module GameGrid

type Grid =
    { Rows: Row.Row[]
      ActiveBlock: Option<Block.Block> }

let create numRows numColumns =
    { Rows = Array.create numRows (Array.create numColumns false)
      ActiveBlock = Option<Block.Block>.None }

let private copy gameGrid = { Rows = Array.map Row.copy gameGrid.Rows; ActiveBlock = gameGrid.ActiveBlock }
let update gameGrid (updateFunction: Grid -> Grid) = copy gameGrid |> updateFunction
let addBlock gameGrid block = { gameGrid with ActiveBlock = Some(block) }

let activeBlockPresent (activeBlock: Block.Block option) rowIndex columnIndex =
    if activeBlock.IsNone then
        false
    else
        Array.tryItem (rowIndex - activeBlock.Value.Location.Y) activeBlock.Value.Rows
            |> Option.bind (fun r -> Array.tryItem (columnIndex - activeBlock.Value.Location.X) r)
            |> Option.defaultValue false

let private renderRow (activeBlock: Block.Block option) rowIndex row =
    Array.concat [
        [| "|" |]
        Array.mapi (fun columnIndex column -> if column || activeBlockPresent activeBlock rowIndex columnIndex then "X" else " ") row
        [| "|" |]
    ]

let render grid =
    Array.concat [
        Array.mapi (fun index r -> renderRow grid.ActiveBlock index r) grid.Rows
        [| Array.create (grid.Rows.[0].Length + 2) "=" |]
    ]

let activeBlockCanMove (gameGrid: Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingY = gameGrid.ActiveBlock.Value.Location.Y

        let obstructionBelowRow rowIndex =
            if gameGrid.Rows.Length <= startingY + rowIndex + 1 then
                true
            else
                let row = Array.tryItem rowIndex gameGrid.ActiveBlock.Value.Rows 
                let obstructionBelowCell columnIndex =
                    let blockCell = Option.bind (fun r -> Array.tryItem columnIndex r) row |> Option.defaultValue false
                    let gameGridCellBelow =
                        Array.tryItem (startingY + rowIndex + 1) gameGrid.Rows
                        |> Option.bind (fun r -> Array.tryItem columnIndex r)
                        |> Option.defaultValue false

                    blockCell && gameGridCellBelow

                Option.map (fun (r: Row.Row) -> Seq.exists obstructionBelowCell (seq { 0 .. r.Length - 1 })) row 
                    |> Option.defaultValue false

        let somethingBlocking = Seq.exists obstructionBelowRow (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })

        not somethingBlocking