module Move

let blockCanMove (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingY = gameGrid.ActiveBlock.Value.Location.Y

        let immovableRow rowIndex =
            if gameGrid.Rows.Length <= startingY + rowIndex + 1 then
                true
            else
                let row = gameGrid.ActiveBlock.Value.Rows.[rowIndex]
                let immovableCell columnIndex =
                    let blockCell = row.[columnIndex]
                    let nextGameGridCell = gameGrid.Rows.[startingY + rowIndex + 1].[columnIndex]
                    blockCell && nextGameGridCell

                Seq.exists immovableCell (seq { 0 .. row.Length })

        let rowRange = Seq.toArray (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length })
        Array.exists immovableRow rowRange

let private fuseBlockWithGrid (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else
        let getCellCoordinates rowIndex =
            let row = gameGrid.ActiveBlock.Value.Rows.[rowIndex]
            Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length })
                |> Seq.map (fun columnIndex -> (rowIndex + gameGrid.ActiveBlock.Value.Location.Y, columnIndex + gameGrid.ActiveBlock.Value.Location.X))

        let setCellAtCoordinates ((x, y): int * int) =
            Array.set gameGrid.Rows.[y] x true

        let cellCoordinates = Seq.collect getCellCoordinates (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length })
        for coords in cellCoordinates do
            setCellAtCoordinates coords

        { gameGrid with ActiveBlock = Option<Block.Block>.None }

let private moveBlockDown (gameGrid: GameGrid.Grid) =
    if blockCanMove gameGrid then
        {
            gameGrid with ActiveBlock = Some({
                gameGrid.ActiveBlock.Value with Location = {
                    gameGrid.ActiveBlock.Value.Location with Y = gameGrid.ActiveBlock.Value.Location.Y + 1 }}) }
    else
        fuseBlockWithGrid gameGrid

let blockDown (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        GameGrid.addBlock gameGrid (Block.create Block.Square)
    else
        moveBlockDown gameGrid
