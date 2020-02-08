module BlockTests

open Xunit

let createTestBlock (location: Location.Location) =
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
