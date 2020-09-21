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

        { Rows = Array.create numRows (Array.create numColumns false)
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

    let private activeBlockPresent (activeBlock: Block option) (gameGridLocation: Location) =
        match activeBlock with
        | None -> false
        | Some block ->
            block.Rows
            |> Array.tryItem (gameGridLocation.Y - block.Location.Y)
            |> Option.bind (fun r -> Array.tryItem (gameGridLocation.X - block.Location.X) r)
            |> Option.defaultValue false

    let private gameGridBlockPresent (gameGrid: GameGrid) (gameGridLocation: Location) =
        if activeBlockPresent gameGrid.ActiveBlock gameGridLocation then
            false
        else
            Array.tryItem gameGridLocation.Y gameGrid.Rows
            |> Option.bind (Array.tryItem gameGridLocation.X)
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

            let firstActiveBlockRow =
                Seq.tryHead activeBlock.Rows
                |> Option.defaultValue Array.empty

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

            not (Seq.exists obstructionToLeftOfColumn (seq { 0 .. firstActiveBlockRow.Length - 1 }))

    let private blockCanMoveRight (gameGrid: GameGrid) =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

            let firstRow =
                Seq.tryHead gameGrid.Rows
                |> Option.defaultValue Array.empty

            let firstActiveBlockRow =
                Seq.tryHead activeBlock.Rows
                |> Option.defaultValue Array.empty

            let obstructionToRightOfColumn columnIndex =
                if firstRow.Length <= startingX + columnIndex + 1 then
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

            not (Seq.exists obstructionToRightOfColumn (seq { 0 .. firstActiveBlockRow.Length - 1 }))

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
                    let row =
                        Array.tryItem rowIndex activeBlock.Rows
                        |> Option.defaultValue Array.empty

                    let obstructionBelowCell columnIndex =
                        activeBlockPresent
                            (Some activeBlock)
                            { Y = startingY + rowIndex
                              X = startingX + columnIndex }
                        && gameGridBlockPresent
                            gameGrid
                               { Y = startingY + rowIndex + 1
                                 X = startingX + columnIndex }

                    Seq.exists obstructionBelowCell (seq { 0 .. row.Length - 1 })

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
                    let gameGridRow =
                        Array.tryHead gameGrid.Rows
                        |> Option.defaultValue Array.empty

                    let outsideBoundary =
                        startingY
                        + rowIndex
                        >= gameGrid.Rows.Length
                        || startingX + columnIndex >= gameGridRow.Length
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
