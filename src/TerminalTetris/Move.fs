namespace TerminalTetris

module Move =

    let private getCellCoordinates (activeBlock: Block) rowIndex =
        let row = activeBlock.Rows.[rowIndex]
        Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length - 1 })
            |> Seq.map (fun columnIndex -> { Location.Y = rowIndex + activeBlock.Location.Y; Location.X = columnIndex + activeBlock.Location.X })

    let private setCellAtLocation (gameGrid: GameGrid) (location: Location) =
        let row = Array.tryItem location.Y gameGrid.Rows |> Option.defaultValue Array.empty

        ArrayHelpers.trySet row (uint32 location.X) true

    let private removeFullRows (gameGrid: GameGrid) =
        let newRows = ResizeArray<Row>()
        let mutable removedRowCount = 0

        for row in gameGrid.Rows do
            if Row.isFull row then
                newRows.Insert(0, Array.zeroCreate row.Length)
                removedRowCount <- removedRowCount + 1
            else
                newRows.Add(row)

        ({ gameGrid with Rows = newRows.ToArray() }, removedRowCount)

    let private fuseBlockWithGrid (gameGrid: GameGrid) =
        if gameGrid.ActiveBlock.IsNone then
            gameGrid
        else
            let cellCoordinates = Seq.collect (fun rowIndex -> getCellCoordinates gameGrid.ActiveBlock.Value rowIndex) (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })
            for coords in cellCoordinates do
                setCellAtLocation gameGrid coords

            { gameGrid with ActiveBlock = Option<Block>.None }

    let private moveBlockDown gameGrid =
        if GameGrid.activeBlockCanMove gameGrid Direction.Down then
            { gameGrid with ActiveBlock = Some(Block.move Direction.Down gameGrid.ActiveBlock.Value) }
        else
            let (grid, removedRowCount) = fuseBlockWithGrid gameGrid |> removeFullRows

            if removedRowCount > 0 then
                grid.GameEvent.Trigger (GameEventArgs.RowsCleared(removedRowCount))

            grid

    let blockDown (gameGrid: GameGrid) =
        if gameGrid.ActiveBlock.IsNone then
            GameGrid.addBlock gameGrid
        else
            moveBlockDown gameGrid

    let blockRight (gameGrid: GameGrid) =
        if GameGrid.activeBlockCanMove gameGrid Direction.Right then
            { gameGrid with ActiveBlock = Some(Block.move Direction.Right gameGrid.ActiveBlock.Value) }
        else
            gameGrid

    let blockLeft (gameGrid: GameGrid) =
        if GameGrid.activeBlockCanMove gameGrid Direction.Left then
            { gameGrid with ActiveBlock = Some(Block.move Direction.Left gameGrid.ActiveBlock.Value) }
        else
            gameGrid

    let rotateBlock (gameGrid: GameGrid) =
        if GameGrid.activeBlockCanMove gameGrid Direction.Rotate then
            { gameGrid with ActiveBlock = Some(Block.move Direction.Rotate gameGrid.ActiveBlock.Value) }
        else
            gameGrid
