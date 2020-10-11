namespace TerminalTetris

[<Struct>]
type GameGrid =
    { Rows: Row []
      ActiveBlock: Option<Block>
      NextBlock: Block
      [<CLIEvent>]
      GameEvent: GameEvent }

module GameGrid =

    let mutable private gameEventObservable = Option<IEvent<GameEventArgs>>.None

    let create dimensions =
        let nextBlock = Block.generateRandom ()
        let yOffset = nextBlock.Rows.Length
        let numRows = int dimensions.Height
        let numColumns = int dimensions.Width

        let gameEvent = new GameEvent()
        gameEventObservable <- Some(gameEvent.Publish)

        { Rows = Row.create numColumns |> Array.create numRows
          ActiveBlock = None
          NextBlock =
              { nextBlock with
                    Location = { X = numColumns / 2; Y = -yOffset } }
          GameEvent = gameEvent }

    let private copy gameGrid =
        { Rows = Array.map Row.copy gameGrid.Rows
          ActiveBlock = gameGrid.ActiveBlock
          NextBlock = gameGrid.NextBlock
          GameEvent = gameGrid.GameEvent }

    type private UpdateFunction = GameGrid -> GameGrid

    let update gameGrid (updateFunction: UpdateFunction) = copy gameGrid |> updateFunction

    let private width gameGrid =
        Array.tryHead gameGrid.Rows
        |> Option.map Array.length
        |> Option.defaultValue 0

    let addBlock gameGrid =
        let nextBlock = Block.generateRandom ()
        let yOffset = nextBlock.Rows.Length

        { gameGrid with
              ActiveBlock = Some(gameGrid.NextBlock)
              NextBlock =
                  { nextBlock with
                        Location = { X = width gameGrid / 2; Y = -yOffset } } }

    let private tryItem row column gameGrid =
        Array.tryItem row gameGrid.Rows
        |> Option.bind (Array.tryItem column)

    let private activeBlockPresent activeBlock gameGridLocation =
        match activeBlock with
        | None -> false
        | Some block ->
            let x, y =
                gameGridLocation.X - block.Location.X, gameGridLocation.Y - block.Location.Y

            Block.tryItem y x block
            |> Option.defaultValue false

    let private gameGridBlockPresent gameGrid gameGridLocation =
        if activeBlockPresent gameGrid.ActiveBlock gameGridLocation then
            false
        else
            tryItem gameGridLocation.Y gameGridLocation.X gameGrid
            |> Option.defaultValue false

    let private renderCell grid rowIndex columnIndex =
        if gameGridBlockPresent grid { Y = rowIndex; X = columnIndex }
           || activeBlockPresent grid.ActiveBlock { Y = rowIndex; X = columnIndex } then
            "X"
        else
            " "

    let private renderRow grid rowIndex =
        let bar () =
            if rowIndex >= 0 then [| "|" |] else [| " " |]

        Array.concat [ bar ()
                       Seq.map (renderCell grid rowIndex) (seq { 0 .. width grid - 1 })
                       |> Seq.toArray
                       bar () ]

    let render grid =
        let ceilingHeight = 4

        Array.append
            (Seq.map (renderRow grid) (seq { -ceilingHeight .. grid.Rows.Length - 1 })
             |> Seq.toArray)
            [| Array.create (width grid + 2) "=" |]

    let private blockCanMoveLeft gameGrid =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

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

            not (Seq.exists obstructionToLeftOfColumn (seq { 0 .. Block.width activeBlock - 1 }))

    let private blockCanMoveRight gameGrid =
        match gameGrid.ActiveBlock with
        | None -> false
        | Some activeBlock ->
            let startingX = activeBlock.Location.X
            let startingY = activeBlock.Location.Y

            let obstructionToRightOfColumn columnIndex =
                if width gameGrid <= startingX + columnIndex + 1 then
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

            not (Seq.exists obstructionToRightOfColumn (seq { 0 .. Block.width activeBlock - 1 }))

    let private blockCanMoveDown gameGrid =
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

            not (Seq.exists obstructionBelowRow (seq { 0 .. activeBlock.Rows.Length - 1 }))

    let private blockCanRotate gameGrid =
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

    let activeBlockCanMove gameGrid direction =
        match direction with
        | Left -> blockCanMoveLeft gameGrid
        | Right -> blockCanMoveRight gameGrid
        | Down -> blockCanMoveDown gameGrid
        | Rotate -> blockCanRotate gameGrid

    let addGameEventHandler eventHandler =
        match gameEventObservable with
        | None -> ignore ()
        | Some observable -> observable.Add(eventHandler)
