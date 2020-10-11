namespace TerminalTetris.UnitTests

open TerminalTetris
open Xunit

module GameGridTests =

    let createTestBlock (width, height) =
        { Block.Rows = Array.create height (Array.create width true)
          Block.Location = { Location.X = 0; Location.Y = 0 } }

    let createTestBlockAtLocation (width, height) (location: Location) =
        { Block.Rows = Array.create height (Array.create width true)
          Block.Location = location }

    let withCellSetAtLocation (location: Location) (gameGrid: GameGrid) =
        let copiedRows = Array.mapi (fun i r -> if i = location.Y then Array.mapi (fun j column -> if j = location.X then true else column) r else r) gameGrid.Rows
        { gameGrid with Rows = copiedRows }

    [<Fact>]
    let ``GameGrid.create creates a grid with the correct number of rows`` () =
        let numRows = 10
        let grid = GameGrid.create { Height = numRows; Width = 20 }
        Assert.Equal(grid.Rows.Length, numRows)

    [<Fact>]
    let ``GameGrid.create creates a grid with the correct number of columns`` () =
        let numColumns = 10
        let grid = GameGrid.create { Height = 20; Width = numColumns }
        Assert.Equal(grid.Rows.[0].Length, numColumns)

    [<Fact>]
    let ``GameGrid.render renders ' ' if a column value is 'false'`` () =
        let renderedGrid =
            { GameGrid.create { Height = 1; Width = 1 } with Rows = [| [| false |] |] }
                |> GameGrid.render

        Array.head renderedGrid |> Array.contains " " |> Assert.True

    [<Fact>]
    let ``GameGrid.render renders an 'X' if a column value is 'true'`` () =
        let gameGridCeilingOffset = 4
        let renderedGrid =
            { GameGrid.create { Height = 1; Width = 1 } with Rows = [| [| true |] |] }
                |> GameGrid.render

        Array.skip gameGridCeilingOffset renderedGrid
            |> Array.head
            |> Array.contains "X"
            |> Assert.True

    [<Fact>]
    let ``GameGrid.render renders a final row of '='`` () =
        let renderedGrid = GameGrid.render (GameGrid.create { Height = 1; Width = 1 })

        Array.last renderedGrid |> Array.contains "=" |> Assert.True

    [<Fact>]
    let givenGridActiveBlockNotExistWhenQueryIfActiveBlockCanMoveDownThenReturnsFalse () =
        let gameGrid =
            { GameGrid.create { Height = 10; Width = 20 } with
                ActiveBlock = Option<Block>.None }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.False

    [<Theory>]
    [<InlineData(0)>]
    [<InlineData(1)>]
    let givenGridActiveBlockBelowOrAtGridBottomWhenQueryingIfActiveBlockCanMoveDownReturnsFalse yDifference =
        let numRows = 10
        let activeBlock = Some({createTestBlock (1, 1) with Location = { X = 0; Y = numRows + yDifference }})
        let gameGrid =
            { GameGrid.create { Height = numRows; Width = 20 } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.False

    [<Fact>]
    let givenGridActiveBlockHasSpaceUnderneathWhenQueryingIfActiveBlockCanMoveDownThenReturnsTrue () =
        let activeBlock = Some(createTestBlock (1, 1))
        let gameGrid =
            { GameGrid.create { Height = 2; Width = 1 } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.True

    [<Fact>]
    let givenGridActiveBlockHasBottomOfGridUnderneathWhenQueryingIfActiveBlockCanMoveDownThenReturnsTrue () =
        let activeBlock = Some(createTestBlock (2, 1))
        let gameGrid = {
            GameGrid.create { Height = 1; Width = 2 } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.False

    [<Fact>]
    let givenFusedBlockLowerRightThanActiveBlockWhenQueryingIfActiveBlockCanMoveDownThenReturnsTrue () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 0; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 4; Width = 4 } with ActiveBlock = activeBlock }
                |> withCellSetAtLocation { X = 3; Y = 2 }
                |> withCellSetAtLocation { X = 3; Y = 3 }
                |> withCellSetAtLocation { X = 4; Y = 2 }
                |> withCellSetAtLocation { X = 4; Y = 3 }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.True

    [<Fact>]
    let givenFusedBlockLowerLeftThanActiveBlockWhenQueryingIfActiveBlockCanMoveDownThenReturnsTrue () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 2; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 4; Width = 4 } with ActiveBlock = activeBlock }
                |> withCellSetAtLocation { X = 0; Y = 2 }
                |> withCellSetAtLocation { X = 0; Y = 3 }
                |> withCellSetAtLocation { X = 1; Y = 2 }
                |> withCellSetAtLocation { X = 1; Y = 3 }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.True

    [<Fact>]
    let givenFusedBlockUnderneathActiveBlockWhenQueryingIfActiveBlockCanMoveDownThenReturnsTrue () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 2; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 4; Width = 4 } with ActiveBlock = activeBlock }
                |> withCellSetAtLocation { X = 2; Y = 2 }
                |> withCellSetAtLocation { X = 2; Y = 3 }
                |> withCellSetAtLocation { X = 3; Y = 2 }
                |> withCellSetAtLocation { X = 3; Y = 3 }

        GameGrid.activeBlockCanMove gameGrid Direction.Down |> Assert.False

    [<Fact>]
    let givenNoActiveBlockWhenQueryingIfActiveBlockCanMoveRightThenItReturnsFalse () =
        let gameGrid =
            { GameGrid.create { Height = 2; Width = 2 } with
                ActiveBlock = Option<Block>.None }

        GameGrid.activeBlockCanMove gameGrid Direction.Right |> Assert.False

    [<Fact>]
    let givenRightSideOfActiveBlockAtRightEdgeOfGameGridWhenQueryingIfActiveBlockCanMoveRightThenItReturnsFalse () =
        let blockWidth = 2
        let gridWidth = 10
        let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 10; Width = gridWidth } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Right |> Assert.False

    [<Fact>]
    let givenRightSideOfActiveBlockPastRightEdgeOfGameGridWhenQueryingIfActiveBlockCanMoveRightThenItReturnsFalse () =
        let blockWidth = 2
        let gridWidth = 10
        let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth + 1; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 10; Width = gridWidth } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Right |> Assert.False

    [<Fact>]
    let givenRightSideOfActiveBlockNotObstructedThenReturnsTrue () =
        let blockWidth = 2
        let gridWidth = 10
        let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth - 1; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 10; Width = gridWidth } with
                ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Right |> Assert.True

    [<Fact>]
    let givenFusedBlockToRightOfActiveBlockWhenQueryingIfActiveBlockCanMoveRightThenItReturnsFalse () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 0; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 4; Width = 4 } with ActiveBlock = activeBlock }
                |> withCellSetAtLocation { X = 2; Y = 0 }
                |> withCellSetAtLocation { X = 2; Y = 1 }
                |> withCellSetAtLocation { X = 3; Y = 0 }
                |> withCellSetAtLocation { X = 3; Y = 1 }

        GameGrid.activeBlockCanMove gameGrid Direction.Right |> Assert.False

    [<Fact>]
    let givenNoActiveBlockWhenQueryingIfActiveBlockCanMoveLeftThenItReturnsFalse () =
        let gameGrid =
            { GameGrid.create { Height = 2; Width = 2 } with
                ActiveBlock = Option<Block>.None }

        GameGrid.activeBlockCanMove gameGrid Direction.Left |> Assert.False

    [<Fact>]
    let givenLeftSideOfActiveBlockAtLeftEdgeOfGameGridWhenQueryingIfActiveBlockCanMoveLeftThenItReturnsFalse () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 0; Y = 0 })
        let gameGrid = { GameGrid.create { Height = 10; Width = 10 } with ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Left |> Assert.False

    [<Fact>]
    let givenLeftSideOfActiveBlockIsPastLeftEdgeOfGameGridWhenQueryingIfActiveBlockCanMoveLeftThenItReturnsFalse () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = -1; Y = 0 })
        let gameGrid = { GameGrid.create { Height = 10; Width = 10 } with ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Left |> Assert.False

    [<Fact>]
    let givenLeftSideOfActiveBlockIsNotObstructedWhenQueryingIfActiveBlockCanMoveLeftThenItReturnsTrue () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 1; Y = 0 })
        let gameGrid = { GameGrid.create { Height = 10; Width = 10 } with ActiveBlock = activeBlock }

        GameGrid.activeBlockCanMove gameGrid Direction.Left |> Assert.True

    [<Fact>]
    let givenFusedBlockToLeftOfActiveBlockWhenQueryingIfActiveBlockCanMoveLeftThenItReturnsFalse () =
        let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 2; Y = 0 })
        let gameGrid =
            { GameGrid.create { Height = 4; Width = 4 } with ActiveBlock = activeBlock }
                |> withCellSetAtLocation { X = 0; Y = 0 }
                |> withCellSetAtLocation { X = 0; Y = 1 }
                |> withCellSetAtLocation { X = 1; Y = 0 }
                |> withCellSetAtLocation { X = 1; Y = 1 }

        GameGrid.activeBlockCanMove gameGrid Direction.Left |> Assert.False

    [<Fact>]
    let WhenAddingBlockThenNextBlockPlacedInHorizontalCentreOfGameGrid () =
        let columnCount = 10
        let expectedMidPoint = 5

        let gameGrid = GameGrid.create { Height = 10; Width = columnCount } |> GameGrid.addBlock

        Assert.Equal(expectedMidPoint, gameGrid.NextBlock.Location.X)

    [<Fact>]
    let WhenAddingBlockThenNextBlockSetToActiveBlock () =
        let nextBlock = { Block.Rows = [||]; Block.Location = { X = 0; Y = 0 }}
        let gameGrid = { GameGrid.create { Height = 4; Width = 4 } with NextBlock = nextBlock }

        Assert.Equal(nextBlock, (GameGrid.addBlock gameGrid).ActiveBlock.Value)
