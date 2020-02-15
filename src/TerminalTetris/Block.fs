module Block

open System
open Microsoft.FSharp.Reflection

type Block = { Rows: Row.Row[]; Location: Location.Location }
type BlockType =
    | Square
    | Line

let private numberOfBlockTypes = FSharpType.GetUnionCases(typeof<BlockType>).Length

let private createSquare _ =
    { Rows = [| [| true; true |]; [| true; true; |] |]
      Location = { X = 0; Y = 0 }}

let private createLine _ =
    { Rows = [| [| true |]; [| true |]; [| true |]; [| true |] |]
      Location = { X = 0; Y = 0 }}

let create blockType =
    match blockType with
    | Square -> createSquare()
    | Line -> createLine()

let generateRandom _ =
    let random = Random().Next(numberOfBlockTypes)
    match random with
    | 0 -> create Square
    | 1 -> create Line
    | _ -> create Square

let move (direction: Direction.Direction) (block: Block) =
    match direction with
    | Direction.Left -> { block with Location = { Y = block.Location.Y; X = block.Location.X - 1 }}
    | Direction.Right -> { block with Location = { Y = block.Location.Y; X = block.Location.X + 1}}
    | Direction.Down -> { block with Location = { X = block.Location.X; Y = block.Location.Y + 1 }}
