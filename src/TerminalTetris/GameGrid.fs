module GameGrid

type Grid =
    { Rows: Row.Row[]
      ActiveBlock: Option<Block.Block>
      NextBlock: Block.Block }

let create numRows numColumns =
    let nextBlock = Block.generateRandom()
    let yOffset = nextBlock.Rows.Length
    
    { Rows = Array.create numRows (Array.create numColumns false)
      ActiveBlock = Option<Block.Block>.None
      NextBlock = { nextBlock with Location = { X = numColumns / 2; Y = -yOffset } } }

let private copy gameGrid = { 
    Rows = Array.map Row.copy gameGrid.Rows
    ActiveBlock = gameGrid.ActiveBlock
    NextBlock = gameGrid.NextBlock }

let update gameGrid (updateFunction: Grid -> Grid) = copy gameGrid |> updateFunction
let addBlock (gameGrid: Grid) = 
    let columnCount = Array.tryHead gameGrid.Rows |> Option.defaultValue Array.empty |> Array.length
    let nextBlock = Block.generateRandom()
    let yOffset = nextBlock.Rows.Length

    { gameGrid with 
        ActiveBlock = Some(gameGrid.NextBlock)
        NextBlock = { nextBlock with Location = { X = columnCount / 2; Y = -yOffset } } }

let private activeBlockPresent (activeBlock: Block.Block option) (gameGridLocation: Location.Location) =
    if activeBlock.IsNone then
        false
    else
        Array.tryItem (gameGridLocation.Y - activeBlock.Value.Location.Y) activeBlock.Value.Rows
            |> Option.bind (fun r -> Array.tryItem (gameGridLocation.X - activeBlock.Value.Location.X) r)
            |> Option.defaultValue false

let private gameGridBlockPresent (gameGrid: Grid) (gameGridLocation: Location.Location) =
    if activeBlockPresent gameGrid.ActiveBlock gameGridLocation then
        false
    else
        Array.tryItem gameGridLocation.Y gameGrid.Rows
            |> Option.bind (Array.tryItem gameGridLocation.X)
            |> Option.defaultValue false

let private renderCell rowIndex columnIndex (grid: Grid) =
    if gameGridBlockPresent grid { Y = rowIndex; X = columnIndex } || 
        activeBlockPresent grid.ActiveBlock { Y = rowIndex; X = columnIndex } then
        "X"
    else
        " "

let private renderRow rowIndex grid =
    let bar () = if rowIndex >= 0 then [| "|" |] else [| " " |]
    
    let rowlength = Array.tryHead grid.Rows |> Option.map Array.length |> Option.defaultValue 0
    Array.concat [
        bar()
        Array.map 
            (fun columnIndex -> renderCell rowIndex columnIndex grid) 
            (Array.ofSeq (seq { 0 .. rowlength - 1 }))
        bar()
    ]

let render (grid: Grid) =
    Array.append
        (Array.map (fun rowIndex -> renderRow rowIndex grid) (Array.ofSeq (seq { -4 .. grid.Rows.Length - 1 })))
        [| Array.create (grid.Rows.[0].Length + 2) "=" |]

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
                    activeBlockPresent gameGrid.ActiveBlock { Y = startingY + rowIndex; X = startingX + columnIndex }
                        && gameGridBlockPresent gameGrid { Y = startingY + rowIndex; X = startingX + columnIndex - 1 }

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
                    activeBlockPresent gameGrid.ActiveBlock { Y = startingY + rowIndex; X = startingX + columnIndex }
                        && gameGridBlockPresent gameGrid { Y = startingY + rowIndex; X = startingX + columnIndex + 1 }

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
                    activeBlockPresent gameGrid.ActiveBlock { Y = startingY + rowIndex; X = startingX + columnIndex }
                        && gameGridBlockPresent gameGrid { Y = startingY + rowIndex + 1; X = startingX + columnIndex }

                Seq.exists obstructionBelowCell (seq { 0 .. row.Length - 1 })

        not (Seq.exists obstructionBelowRow (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 }))

let private blockCanRotate (gameGrid: Grid) =
    if gameGrid.ActiveBlock.IsNone then
        false
    else
        let rotatedActiveBlock = Block.rotate gameGrid.ActiveBlock.Value
        let startingX = rotatedActiveBlock.Location.X
        let startingY = rotatedActiveBlock.Location.Y

        let rowHasObstruction rowIndex =
            let row = Array.tryItem rowIndex rotatedActiveBlock.Rows |> Option.defaultValue Array.empty

            let cellHasObstruction columnIndex =
                activeBlockPresent (Some(rotatedActiveBlock)) { X = startingX + columnIndex; Y = startingY + rowIndex }
                    && gameGridBlockPresent gameGrid { X = startingX + columnIndex; Y = startingY + rowIndex }

            let cellOutsideBoundary columnIndex =
                let gameGridRow = Array.tryHead gameGrid.Rows |> Option.defaultValue Array.empty
                let outsideBoundary = 
                    startingY + rowIndex >= gameGrid.Rows.Length 
                        || startingY + rowIndex < 0
                        || startingX + columnIndex >= gameGridRow.Length
                        || startingX + columnIndex < 0

                activeBlockPresent (Some(rotatedActiveBlock)) { X = startingX + columnIndex; Y = startingY + rowIndex }
                    && outsideBoundary

            Seq.exists (fun columnIndex -> cellHasObstruction columnIndex || cellOutsideBoundary columnIndex) (seq { 0 .. row.Length - 1 })

        not (Seq.exists rowHasObstruction (seq { 0 .. rotatedActiveBlock.Rows.Length - 1 }))

let activeBlockCanMove (gameGrid: Grid) (direction: Direction.Direction) =
    match direction with
    | Direction.Left -> blockCanMoveLeft gameGrid
    | Direction.Right -> blockCanMoveRight gameGrid
    | Direction.Down -> blockCanMoveDown gameGrid
    | Direction.Rotate -> blockCanRotate gameGrid
