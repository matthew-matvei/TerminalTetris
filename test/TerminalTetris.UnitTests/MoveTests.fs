module MoveTests

open Xunit

[<Fact>]
let ``Move.blockCanMove returns false if grid's active block doesn't exist`` () =
    let gameGrid = GameGrid.create 10 20
    Move.blockCanMove { gameGrid with ActiveBlock = Option<Block.Block>.None }
        |> Assert.False
