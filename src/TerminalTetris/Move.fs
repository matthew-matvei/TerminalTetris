module Move

let private getCellCoordinates (activeBlock: Block.Block) rowIndex =
    let row = activeBlock.Rows.[rowIndex]
    Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length - 1 })
        |> Seq.map (fun columnIndex -> { Location.Y = rowIndex + activeBlock.Location.Y; Location.X = columnIndex + activeBlock.Location.X })

let private setCellAtLocation (gameGrid: GameGrid.Grid) (location: Location.Location) =
    Array.set gameGrid.Rows.[location.Y] location.X true

let private fuseBlockWithGrid (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else
        let cellCoordinates = Seq.collect (fun rowIndex -> getCellCoordinates gameGrid.ActiveBlock.Value rowIndex) (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })
        for coords in cellCoordinates do
            setCellAtLocation gameGrid coords

        { gameGrid with ActiveBlock = Option<Block.Block>.None }

let private moveBlockDown gameGrid =
    if GameGrid.activeBlockCanMove gameGrid then
        { gameGrid with ActiveBlock = Some(Block.move Direction.Down gameGrid.ActiveBlock.Value) }
    else
        fuseBlockWithGrid gameGrid

let blockDown (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        GameGrid.addBlock gameGrid (Block.create Block.Square)
    else
        moveBlockDown gameGrid

let blockRight (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else if GameGrid.blockCanMoveRight gameGrid then
        { gameGrid with ActiveBlock = Some(Block.move Direction.Right gameGrid.ActiveBlock.Value) }
    else
        gameGrid

let blockLeft (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else if GameGrid.blockCanMoveLeft gameGrid then
        { gameGrid with ActiveBlock = Some(Block.move Direction.Left gameGrid.ActiveBlock.Value) }
    else
        gameGrid
        