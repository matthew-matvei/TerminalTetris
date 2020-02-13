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

let private activeBlockPresent (activeBlock: Block.Block option) rowIndex columnIndex =
    if activeBlock.IsNone then
        false
    else
        Array.tryItem (rowIndex - activeBlock.Value.Location.Y) activeBlock.Value.Rows
            |> Option.bind (fun r -> Array.tryItem (columnIndex - activeBlock.Value.Location.X) r)
            |> Option.defaultValue false

let private gameGridBlockPresent (gameGrid: Grid) rowIndex columnIndex =
    if activeBlockPresent gameGrid.ActiveBlock rowIndex columnIndex then
        false
    else
        Array.tryItem rowIndex gameGrid.Rows
            |> Option.bind (Array.tryItem columnIndex)
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

let private blockCanMoveLeft (gameGrid: Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingX = gameGrid.ActiveBlock.Value.Location.X
        let startingY = gameGrid.ActiveBlock.Value.Location.Y
        let firstActiveBlockRow = Seq.tryHead gameGrid.ActiveBlock.Value.Rows |> Option.defaultValue Array.empty

        let obstructionToLeftOfColumn columnIndex =
            if startingX + columnIndex = 0 then
                true
            else
                let obstructionToLeftOfCell rowIndex =
                    activeBlockPresent gameGrid.ActiveBlock (startingY + rowIndex) (startingX + columnIndex)
                        && gameGridBlockPresent gameGrid (startingY + rowIndex) (startingX + columnIndex - 1)

                Seq.exists obstructionToLeftOfCell (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })

        not (Seq.exists obstructionToLeftOfColumn (seq { 0 .. firstActiveBlockRow.Length - 1 }))

let private blockCanMoveRight (gameGrid: Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingX = gameGrid.ActiveBlock.Value.Location.X
        let startingY = gameGrid.ActiveBlock.Value.Location.Y
        let firstRow = Seq.tryHead gameGrid.Rows |> Option.defaultValue Array.empty
        let firstActiveBlockRow = Seq.tryHead gameGrid.ActiveBlock.Value.Rows |> Option.defaultValue Array.empty

        let obstructionToRightOfColumn columnIndex =
            if firstRow.Length <= startingX + columnIndex + 1 then
                true
            else
                let obstructionToRightOfCell rowIndex =
                    activeBlockPresent gameGrid.ActiveBlock (startingY + rowIndex) (startingX + columnIndex)
                        && gameGridBlockPresent gameGrid (startingY + rowIndex) (startingX + columnIndex + 1)

                Seq.exists obstructionToRightOfCell (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })

        not (Seq.exists obstructionToRightOfColumn (seq { 0 .. firstActiveBlockRow.Length - 1 }))

let private blockCanMoveDown (gameGrid: Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingX = gameGrid.ActiveBlock.Value.Location.X
        let startingY = gameGrid.ActiveBlock.Value.Location.Y

        let obstructionBelowRow rowIndex =
            if gameGrid.Rows.Length <= startingY + rowIndex + 1 then
                true
            else
                let row = Array.tryItem rowIndex gameGrid.ActiveBlock.Value.Rows |> Option.defaultValue Array.empty
                let obstructionBelowCell columnIndex =
                    activeBlockPresent gameGrid.ActiveBlock (startingY + rowIndex) (startingX + columnIndex)
                        && gameGridBlockPresent gameGrid (startingY + rowIndex + 1) (startingX + columnIndex)

                Seq.exists obstructionBelowCell (seq { 0 .. row.Length - 1 })

        not (Seq.exists obstructionBelowRow (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 }))

let activeBlockCanMove (gameGrid: Grid) (direction: Direction.Direction) =
    match direction with
    | Direction.Left -> blockCanMoveLeft gameGrid
    | Direction.Right -> blockCanMoveRight gameGrid
    | Direction.Down -> blockCanMoveDown gameGrid
