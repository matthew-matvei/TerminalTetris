module Move

type Coordinate = { X: int; Y: int }

let getCellCoordinates (activeBlock: Block.Block) rowIndex =
    let row = activeBlock.Rows.[rowIndex]
    Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length - 1 })
        |> Seq.map (fun columnIndex -> { Y = rowIndex + activeBlock.Location.Y; X = columnIndex + activeBlock.Location.X })

let private setCellAtCoordinates (gameGrid: GameGrid.Grid) (coords: Coordinate) =
    Array.set gameGrid.Rows.[coords.Y] coords.X true

let blockCanMove (gameGrid: GameGrid.Grid) =
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

                Option.map (fun (r: Block.Row) -> Seq.exists obstructionBelowCell (seq { 0 .. r.Length - 1 })) row 
                    |> Option.defaultValue false

        let somethingBlocking = Seq.exists obstructionBelowRow (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })

        not somethingBlocking

let private fuseBlockWithGrid (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else
        let cellCoordinates = Seq.collect (fun rowIndex -> getCellCoordinates gameGrid.ActiveBlock.Value rowIndex) (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })
        for coords in cellCoordinates do
            setCellAtCoordinates gameGrid coords

        { gameGrid with ActiveBlock = Option<Block.Block>.None }

let private moveBlockDown (gameGrid: GameGrid.Grid) =
    if blockCanMove gameGrid then
        {
            gameGrid with ActiveBlock = Some({
                gameGrid.ActiveBlock.Value with Location = {
                    gameGrid.ActiveBlock.Value.Location with Y = gameGrid.ActiveBlock.Value.Location.Y + 1 }})}
    else
        fuseBlockWithGrid gameGrid

let blockDown (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        GameGrid.addBlock gameGrid (Block.create Block.Square)
    else
        moveBlockDown gameGrid
