module Move

let getCellCoordinates (gameGrid: GameGrid.Grid) rowIndex =
    let row = gameGrid.ActiveBlock.Value.Rows.[rowIndex]
    Seq.where (fun columnIndex -> row.[columnIndex]) (seq { 0 .. row.Length - 1 })
        |> Seq.map (fun columnIndex -> (rowIndex + gameGrid.ActiveBlock.Value.Location.Y, columnIndex + gameGrid.ActiveBlock.Value.Location.X))

let private setCellAtCoordinates (gameGrid: GameGrid.Grid) ((x, y): int * int) =
    Array.set gameGrid.Rows.[y] x true

let blockCanMove (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let startingY = gameGrid.ActiveBlock.Value.Location.Y

        let immovableRow rowIndex =
            if gameGrid.Rows.Length <= startingY + rowIndex + 1 then
                true
            else
                let row = gameGrid.Rows.[rowIndex]
                let immovableCell columnIndex =
                    let blockCell = row.[columnIndex]
                    let nextGameGridCell = gameGrid.Rows.[startingY + rowIndex + 1].[columnIndex]
                    blockCell && nextGameGridCell

                Seq.exists immovableCell (seq { 0 .. row.Length - 1 })

        Seq.exists immovableRow (seq { 0 .. gameGrid.Rows.Length - 1 })

let private fuseBlockWithGrid (gameGrid: GameGrid.Grid) =
    if gameGrid.ActiveBlock.IsNone then
        gameGrid
    else
        let cellCoordinates = Seq.collect (fun rowIndex -> getCellCoordinates gameGrid rowIndex) (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 })
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
