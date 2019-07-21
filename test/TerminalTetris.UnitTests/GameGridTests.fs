module GameGridTests

open Xunit

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
