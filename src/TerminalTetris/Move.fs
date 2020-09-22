namespace TerminalTetris

module Move =

    let private getCellCoordinates (activeBlock: Block) rowIndex =
        let row = activeBlock.Rows.[rowIndex]
        Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length - 1 })
        |> Seq.map (fun columnIndex ->
            { Location.Y = rowIndex + activeBlock.Location.Y
              Location.X = columnIndex + activeBlock.Location.X })

    let private setCellAtLocation (gameGrid: GameGrid) (location: Location) =
        let row =
            Array.tryItem location.Y gameGrid.Rows
            |> Option.defaultValue Array.empty

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

        ({ gameGrid with
               Rows = newRows.ToArray() },
         removedRowCount)

    let private fuseBlockWithGrid (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> gameGrid
        | Some activeBlock ->
            let cellCoordinates =
                Seq.collect (getCellCoordinates activeBlock) (seq { 0 .. activeBlock.Rows.Length - 1 })

            for coords in cellCoordinates do
                setCellAtLocation gameGrid coords

            { gameGrid with
                  ActiveBlock = Option<Block>.None }

    let private moveBlockDown gameGrid =
        if GameGrid.activeBlockCanMove gameGrid Down then
            { gameGrid with
                  ActiveBlock = Some(Block.move Down gameGrid.ActiveBlock.Value) }
        else

        if (gameGrid.ActiveBlock.Value.Location.Y
            + gameGrid.ActiveBlock.Value.Rows.Length < 4) then
            gameGrid.GameEvent.Trigger(GameOver)
            gameGrid
        else
            let (grid, removedRowCount) =
                fuseBlockWithGrid gameGrid |> removeFullRows

            if removedRowCount > 0
            then grid.GameEvent.Trigger(RowsCleared(removedRowCount))

            grid

    let private moveBlockIf move expression gameGrid =
        if expression then
            { gameGrid with
                ActiveBlock = Some(move gameGrid.ActiveBlock.Value)}
        else
            gameGrid

    let blockDown (gameGrid: GameGrid) =
        if gameGrid.ActiveBlock.IsNone then GameGrid.addBlock gameGrid else moveBlockDown gameGrid

    let blockRight (gameGrid: GameGrid) =
        moveBlockIf (Block.move Right) (GameGrid.activeBlockCanMove gameGrid Right) gameGrid

    let blockLeft (gameGrid: GameGrid) =
        moveBlockIf (Block.move Left) (GameGrid.activeBlockCanMove gameGrid Left) gameGrid

    let rotateBlock (gameGrid: GameGrid) =
        moveBlockIf (Block.move Rotate) (GameGrid.activeBlockCanMove gameGrid Rotate) gameGrid
