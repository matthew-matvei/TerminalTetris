module GameGridTests

open Xunit

let createTestBlock (width, height) =
    { Block.Rows = Array.create height (Array.create width true)
      Block.Location = { Location.X = 0; Location.Y = 0 } }

let createTestBlockAtLocation (width, height) (location: Location.Location) =
    { Block.Rows = Array.create height (Array.create width true)
      Block.Location = location }

[<Fact>]
let ``GameGrid.create creates a grid with the correct number of rows`` () =
    let numRows = 10
    let grid = GameGrid.create numRows 20
    Assert.Equal(grid.Rows.Length, numRows)

[<Fact>]
let ``GameGrid.create creates a grid with the correct number of columns`` () =
    let numColumns = 10
    let grid = GameGrid.create 20 numColumns
    Assert.Equal(grid.Rows.[0].Length, numColumns)

[<Fact>]
let ``GameGrid.render renders ' ' if a column value is 'false'`` () =
    let grid = GameGrid.create 1 1
    grid.Rows.[0].[0] <- false
    let renderedGrid = GameGrid.render grid
    Assert.True(Array.contains " " renderedGrid.[0])

[<Fact>]
let ``GameGrid.render renders an 'X' if a column value is 'true'`` () =
    let grid = GameGrid.create 1 1
    grid.Rows.[0].[0] <- true
    let renderedGrid = GameGrid.render grid
    Assert.True(Array.contains "X" renderedGrid.[0])

[<Fact>]
let ``GameGrid.render renders a final row of '='`` () =
    let renderedGrid = GameGrid.render (GameGrid.create 1 1)
    let lastRow = Array.last renderedGrid
    Assert.True(Array.contains "=" lastRow)

[<Fact>]
let ``GameGrid.activeBlockCanMove GIVEN grid's active block doesn't exist THEN it returns false`` () =
    let gameGrid = { GameGrid.create 10 20 with ActiveBlock = Option<Block.Block>.None }
    GameGrid.activeBlockCanMove gameGrid |> Assert.False

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
let ``GameGrid.activeBlockCanMove GIVEN the grid's active block's current Y-axis location is greater than or equal to the height of the game grid THEN it returns false`` (yDifference) =
    let numRows = 10
    let activeBlock = Some({createTestBlock (1, 1) with Location = { X = 0; Y = numRows + yDifference }})    
    let gameGrid = { GameGrid.create numRows 20 with ActiveBlock = activeBlock }

    GameGrid.activeBlockCanMove gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.activeBlockCanMove GIVEN the grid's active block has space underneath it THEN it returns true`` () =
    let activeBlock = Some(createTestBlock (1, 1))
    let gameGrid = { GameGrid.create 2 1 with ActiveBlock = activeBlock }

    GameGrid.activeBlockCanMove gameGrid |> Assert.True

[<Fact>]
let ``GameGrid.activeBlockCanMove GIVEN the grid's active block has the bottom of the grid underneath it THEN it returns false`` () =
    let activeBlock = Some(createTestBlock (2, 1))
    let gameGrid = { GameGrid.create 1 2 with ActiveBlock = activeBlock }

    GameGrid.activeBlockCanMove gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveRight GIVEN no active block THEN it returns false`` () =
    let gameGrid = { GameGrid.create 2 2 with ActiveBlock = Option<Block.Block>.None }

    GameGrid.blockCanMoveRight gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveRight GIVEN right side of active block is at right edge of game grid THEN it returns false`` () =
    let blockWidth = 2
    let gridWidth = 10
    let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth; Y = 0 })
    let gameGrid = { GameGrid.create gridWidth 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveRight gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveRight GIVEN right side of active block is past right edge of game grid THEN it returns false`` () =
    let blockWidth = 2
    let gridWidth = 10
    let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth + 1; Y = 0 })
    let gameGrid = { GameGrid.create gridWidth 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveRight gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveRight GIVEN the right side of active block is not obstructed by the edge of game grid THEN it returns true`` () =
    let blockWidth = 2
    let gridWidth = 10
    let activeBlock = Some(createTestBlockAtLocation (blockWidth, 2) { X = gridWidth - blockWidth - 1; Y = 0 })
    let gameGrid = { GameGrid.create gridWidth 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveRight gameGrid |> Assert.True

[<Fact>]
let ``GameGrid.blockCanMoveLeft GIVEN no active block THEN it returns false`` () =
    let gameGrid = { GameGrid.create 2 2 with ActiveBlock = Option<Block.Block>.None }

    GameGrid.blockCanMoveLeft gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveLeft GIVEN left side of active block is at left edge of game grid THEN it returns false`` () =
    let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 0; Y = 0 })
    let gameGrid = { GameGrid.create 10 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveLeft gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveLeft GIVEN left side of active block is past left edge of game grid THEN it returns false`` () =
    let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = -1; Y = 0 })
    let gameGrid = { GameGrid.create 10 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveLeft gameGrid |> Assert.False

[<Fact>]
let ``GameGrid.blockCanMoveLeft GIVEN the left side of active block is not obstructed by the edge of game grid THEN it returns true`` () =
    let activeBlock = Some(createTestBlockAtLocation (2, 2) { X = 1; Y = 0 })
    let gameGrid = { GameGrid.create 10 10 with ActiveBlock = activeBlock }

    GameGrid.blockCanMoveLeft gameGrid |> Assert.True
