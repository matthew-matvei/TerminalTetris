namespace TerminalTetris

type GameGrid =
    { Rows: Row []
      ActiveBlock: Option<Block>
      NextBlock: Block
      [<CLIEvent>]
      GameEvent: GameEvent }

module GameGrid =

    let mutable private gameEventObservable = Option<IEvent<GameEventArgs>>.None

    let create (dimensions: Dimensions) =
        let nextBlock = Block.generateRandom ()
        let yOffset = nextBlock.Rows.Length
        let numRows = int dimensions.Height
        let numColumns = int dimensions.Width

        let gameEvent = new GameEvent()
        gameEventObservable <- Some(gameEvent.Publish)

        { Rows = Array.create numColumns false |> Array.create numRows
          ActiveBlock = Option<Block>.None
          NextBlock =
              { nextBlock with
                    Location = { X = numColumns / 2; Y = -yOffset } }
          GameEvent = gameEvent }

    let private copy gameGrid =
        { Rows = Array.map Row.copy gameGrid.Rows
          ActiveBlock = gameGrid.ActiveBlock
          NextBlock = gameGrid.NextBlock
          GameEvent = gameGrid.GameEvent }

    let update gameGrid (updateFunction: GameGrid -> GameGrid) = copy gameGrid |> updateFunction

    let addBlock (gameGrid: GameGrid) =
        let columnCount =
            Array.tryHead gameGrid.Rows
            |> Option.defaultValue Array.empty
            |> Array.length

        let nextBlock = Block.generateRandom ()
        let yOffset = nextBlock.Rows.Length

        { gameGrid with
              ActiveBlock = Some(gameGrid.NextBlock)
              NextBlock =
                  { nextBlock with
                        Location = { X = columnCount / 2; Y = -yOffset } } }

    let private tryItem row column gameGrid =
        Array.tryItem row gameGrid.Rows
        |> Option.bind (Array.tryItem column)

    let private activeBlockPresent (activeBlock: Block option) (gameGridLocation: Location) =
        match activeBlock with
        | None -> false
        | Some block ->
            let x, y = gameGridLocation.X - block.Location.X,
                        gameGridLocation.Y - block.Location.Y

            Block.tryItem y x block |> Option.defaultValue false

    let private gameGridBlockPresent (gameGrid: GameGrid) (gameGridLocation: Location) =
        if activeBlockPresent gameGrid.ActiveBlock gameGridLocation then
            false
        else
            tryItem gameGridLocation.Y gameGridLocation.X gameGrid
            |> Option.defaultValue false

    let private renderCell rowIndex columnIndex (grid: GameGrid) =
        if gameGridBlockPresent grid { Y = rowIndex; X = columnIndex }
           || activeBlockPresent grid.ActiveBlock { Y = rowIndex; X = columnIndex } then
            "X"
        else
            " "

    let private renderRow rowIndex grid =
        let bar () =
            if rowIndex >= 0 then [| "|" |] else [| " " |]

        let rowlength =
            Array.tryHead grid.Rows
            |> Option.map Array.length
            |> Option.defaultValue 0

        Array.concat [ bar ()
                       Array.map (fun columnIndex -> renderCell rowIndex columnIndex grid)
                           (Array.ofSeq (seq { 0 .. rowlength - 1 }))
                       bar () ]

    let render (grid: GameGrid) =
        let ceilingHeight = 4

        Array.append
            (Array.map (fun rowIndex -> renderRow rowIndex grid)
                 (Array.ofSeq (seq { -ceilingHeight .. grid.Rows.Length - 1 })))
            [| Array.create (grid.Rows.[0].Length + 2) "=" |]

    let private blockCanMoveLeft (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

            let firstActiveBlockRowLength =
                Seq.tryHead activeBlock.Rows
                |> Option.defaultValue Array.empty
                |> Array.length

            let obstructionToLeftOfColumn columnIndex =
                if startingX + columnIndex = 0 then
                    true
                else
                    let obstructionToLeftOfCell rowIndex =
                        activeBlockPresent
                            (Some activeBlock)
                            { Y = startingY + rowIndex
                              X = startingX + columnIndex }
                        && gameGridBlockPresent
                            gameGrid
                               { Y = startingY + rowIndex
                                 X = startingX + columnIndex - 1 }

                    Seq.exists obstructionToLeftOfCell (seq { 0 .. activeBlock.Rows.Length - 1 })

            not (Seq.exists obstructionToLeftOfColumn (seq { 0 .. firstActiveBlockRowLength - 1 }))

    let private blockCanMoveRight (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

            let firstRowLength =
                Seq.tryHead gameGrid.Rows
                |> Option.defaultValue Array.empty
                |> Array.length

            let firstActiveBlockRowLength =
                Seq.tryHead activeBlock.Rows
                |> Option.defaultValue Array.empty
                |> Array.length

            let obstructionToRightOfColumn columnIndex =
                if firstRowLength <= startingX + columnIndex + 1 then
                    true
                else
                    let obstructionToRightOfCell rowIndex =
                        activeBlockPresent
                            (Some activeBlock)
                            { Y = startingY + rowIndex
                              X = startingX + columnIndex }
                        && gameGridBlockPresent
                            gameGrid
                               { Y = startingY + rowIndex
                                 X = startingX + columnIndex + 1 }

                    Seq.exists obstructionToRightOfCell (seq { 0 .. activeBlock.Rows.Length - 1 })

            not (Seq.exists obstructionToRightOfColumn (seq { 0 .. firstActiveBlockRowLength - 1 }))

    let private blockCanMoveDown (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

            let obstructionBelowRow rowIndex =
                if gameGrid.Rows.Length <= startingY + rowIndex + 1 then
                    true
                else
                    let rowLength =
                        Array.tryItem rowIndex activeBlock.Rows
                        |> Option.defaultValue Array.empty
                        |> Array.length

                    let obstructionBelowCell columnIndex =
                        activeBlockPresent
                            (Some activeBlock)
                            { Y = startingY + rowIndex
                              X = startingX + columnIndex }
                        && gameGridBlockPresent
                            gameGrid
                               { Y = startingY + rowIndex + 1
                                 X = startingX + columnIndex }

                    Seq.exists obstructionBelowCell (seq { 0 .. rowLength - 1 })

            not (Seq.exists obstructionBelowRow (seq { 0 .. gameGrid.ActiveBlock.Value.Rows.Length - 1 }))

    let private blockCanRotate (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let rotatedActiveBlock = Block.rotate activeBlock
            let startingX = rotatedActiveBlock.Location.X
            let startingY = rotatedActiveBlock.Location.Y

            let rowHasObstruction rowIndex =
                let row =
                    Array.tryItem rowIndex rotatedActiveBlock.Rows
                    |> Option.defaultValue Array.empty

                let cellHasObstruction columnIndex =
                    activeBlockPresent
                        (Some(rotatedActiveBlock))
                        { X = startingX + columnIndex
                          Y = startingY + rowIndex }
                    && gameGridBlockPresent
                        gameGrid
                           { X = startingX + columnIndex
                             Y = startingY + rowIndex }

                let cellOutsideBoundary columnIndex =
                    let gameGridRowLength =
                        Array.tryHead gameGrid.Rows
                        |> Option.defaultValue Array.empty
                        |> Array.length

                    let outsideBoundary =
                        startingY
                        + rowIndex
                        >= gameGrid.Rows.Length
                        || startingX + columnIndex >= gameGridRowLength
                        || startingX + columnIndex < 0

                    activeBlockPresent
                        (Some(rotatedActiveBlock))
                        { X = startingX + columnIndex
                          Y = startingY + rowIndex }
                    && outsideBoundary

                Seq.exists (fun columnIndex ->
                    cellHasObstruction columnIndex
                    || cellOutsideBoundary columnIndex) (seq { 0 .. row.Length - 1 })

            not (Seq.exists rowHasObstruction (seq { 0 .. rotatedActiveBlock.Rows.Length - 1 }))

    let activeBlockCanMove (gameGrid: GameGrid) (direction: Direction) =
        match direction with
        | Left -> blockCanMoveLeft gameGrid
        | Right -> blockCanMoveRight gameGrid
        | Down -> blockCanMoveDown gameGrid
        | Rotate -> blockCanRotate gameGrid

    let addGameEventHandler (eventHandler: GameEventArgs -> unit) =
        match gameEventObservable with
        | None -> ignore ()
        | Some observable -> observable.Add(eventHandler)
