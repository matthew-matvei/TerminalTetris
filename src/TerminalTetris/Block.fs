module Block

open System
open Microsoft.FSharp.Reflection

type Block = { Rows: Row.Row[]; Location: Location.Location }
type BlockType =
    | Square
    | Line
    | L
    | Triangle
    | ZigZag

let private numberOfBlockTypes = FSharpType.GetUnionCases(typeof<BlockType>).Length

let private createSquareAt (location: Location.Location) =
    { Rows = [| [| true; true |]; [| true; true; |] |]
      Location = location }

let private createLineAt (location: Location.Location) =
    { Rows = [| [| true |]; [| true |]; [| true |]; [| true |] |]
      Location = location }

let private createLAt (location: Location.Location) =
    { Rows = [|
        [| false; true |]
        [| false; true |]
        [| false; true |]
        [| true; true |]
        |]
      Location = location }

let private createTriangleAt (location: Location.Location) =
    { Rows = [|
        [| true; false |]
        [| true; true |]
        [| true; false |]
        |]
      Location = location }

let private createZigZagAt (location: Location.Location) =
    { Rows = [|
        [| true; false |]
        [| true; true |]
        [| false; true |]
        |]
      Location = location }

let createAt (location: Location.Location) blockType =
    match blockType with
    | Square -> createSquareAt location
    | Line -> createLineAt location
    | L -> createLAt location
    | Triangle -> createTriangleAt location
    | ZigZag -> createZigZagAt location

let generateRandomAt (location: Location.Location) =
    let random = Random().Next(numberOfBlockTypes)
    match random with
    | 0 -> createAt location Square
    | 1 -> createAt location Line
    | 2 -> createAt location L
    | 3 -> createAt location Triangle
    | 4 -> createAt location ZigZag
    | _ -> createAt location Square

let private newLocation (oldLocation: Location.Location) rowCount columnCount =
    let middleX = columnCount / 2
    let middleY = rowCount / 2
    { Location.X = oldLocation.X + middleY - middleX
      Location.Y = oldLocation.Y + middleX - middleY }

let rotate (block: Block) =
    let columnCount = Array.head block.Rows |> Array.length

    let createRowFromColumn columnIndex =
        Array.map (fun r -> Array.item columnIndex r) block.Rows
    
    let newRows = 
        Seq.map (createRowFromColumn >> Array.rev) (seq {0 .. columnCount - 1})
        |> Seq.toArray

    let newRowCount = newRows.Length
    let newColumnCount = Array.head newRows |> Array.length

    { block with 
        Rows = newRows
        Location = newLocation block.Location newRowCount newColumnCount }

let move (direction: Direction.Direction) (block: Block) =
    match direction with
    | Direction.Left -> { block with Location = { Y = block.Location.Y; X = block.Location.X - 1 }}
    | Direction.Right -> { block with Location = { Y = block.Location.Y; X = block.Location.X + 1}}
    | Direction.Down -> { block with Location = { X = block.Location.X; Y = block.Location.Y + 1 }}
    | Direction.Rotate -> rotate block
