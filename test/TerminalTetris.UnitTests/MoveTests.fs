module MoveTests

open Xunit

let createTestBlock (width, height) =
    { Block.Rows = Array.create height (Array.create width true)
      Block.Location = { X = 0; Y = 0 } }

[<Fact>]
let ``Move.blockCanMove GIVEN grid's active block doesn't exist THEN it returns false`` () =
    let gameGrid = GameGrid.create 10 20
    Move.blockCanMove { gameGrid with ActiveBlock = Option<Block.Block>.None }
        |> Assert.False

[<Theory>]
[<InlineData(0)>]
[<InlineData(1)>]
let ``Move.blockCanMove GIVEN the grid's active block's current Y-axis location is greater than or equal to the height of the game grid THEN it returns false`` (yDifference) =
    let numRows = 10
    let activeBlock = Some({createTestBlock (1, 1) with Location = { X = 0; Y = numRows + yDifference }})    
    let gameGrid = { GameGrid.create numRows 20 with ActiveBlock = activeBlock }

    Move.blockCanMove gameGrid
        |> Assert.False

[<Fact>]
let ``Move.blockCanMove GIVEN the grid's active block has space underneath it THEN it returns true`` () =
    let activeBlock = Some(createTestBlock (1, 1))
    let gameGrid = { GameGrid.create 2 1 with ActiveBlock = activeBlock }

    Move.blockCanMove gameGrid
        |> Assert.True

[<Fact>]
let ``Move.blockCanMove GIVEN the grid's active block has the bottom of the grid underneath it THEN it returns false`` () =
    let activeBlock = Some(createTestBlock (2, 1))
    let gameGrid = { GameGrid.create 1 2 with ActiveBlock = activeBlock }

    Move.blockCanMove gameGrid
        |> Assert.False
