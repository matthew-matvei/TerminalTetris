namespace TerminalTetris.UnitTests

open TerminalTetris
open Xunit

module BlockTests =

    let createTestBlock (location: Location) =
        { Block.Rows = Array.create 2 (Array.create 2 true)
          Block.Location = location }

    [<Fact>]
    let ``Block.move can move the given block left`` () =
        let startingX = 1
        let expectedX = 0
        let block = createTestBlock { X = startingX; Y = 0 }
        let movedBlock = Block.move Direction.Left block
        Assert.Equal(expectedX, movedBlock.Location.X)

    [<Fact>]
    let ``Block.move can move the given block right`` () =
        let startingX = 0
        let expectedX = 1
        let block = createTestBlock { X = startingX; Y = 0 }
        let movedBlock = Block.move Direction.Right block
        Assert.Equal(expectedX, movedBlock.Location.X)

    [<Fact>]
    let ``Block.move can move the given block down`` () =
        let startingY = 0
        let expectedY = 1
        let block = createTestBlock { X = 0; Y = startingY }
        let movedBlock = Block.move Direction.Down block
        Assert.Equal(expectedY, movedBlock.Location.Y)

    [<Fact>]
    let ``Block.rotate returns the given block rotated 90 degrees clockwise`` () =
        let rows = [|
            [| true; false |]
            [| true; true |]
            [| true; false |]
            |]
        let expectedRows = [|
            [| true; true; true |]
            [| false; true; false |]
             |]
        let block = { Block.Rows = rows; Block.Location = { X = 0; Y = 0 }}

        let rotatedBlock = Block.rotate block

        Assert.Equal<Row>(expectedRows, rotatedBlock.Rows)

    [<Fact>]
    let ``Block.rotate rotates a block 4 times to return it to its initial orientation`` () =
        let rows = [|
            [| true; false |]
            [| true; true |]
            [| true; false |]
            |]
        let expectedRows = [|
            [| true; false |]
            [| true; true |]
            [| true; false |]
            |]
        let block = { Block.Rows = rows; Block.Location = { X = 0; Y = 0 }}

        let rotatedBlock =
            Block.rotate block
            |> Block.rotate
            |> Block.rotate
            |> Block.rotate

        Assert.Equal<Row>(expectedRows, rotatedBlock.Rows)

    [<Fact>]
    let ``Block.generateRandom generates some random block at location X = 0 and Y = 0`` () =
        let randomBlock = Block.generateRandom()
        let expectedLocation = { Location.X = 0; Location.Y = 0 }

        Assert.Equal<Location>(expectedLocation, randomBlock.Location)
