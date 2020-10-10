namespace TerminalTetris

open System
open Microsoft.FSharp.Reflection

type Block = { Rows: Row []; Location: Location }

type BlockType =
    | Square
    | Line
    | L
    | Triangle
    | ZigZag

module Block =

    let private numberOfBlockTypes =
        FSharpType.GetUnionCases(typeof<BlockType>).Length

    let private createSquareAt location =
        { Rows = [| [| true; true |]; [| true; true |] |]
          Location = location }

    let private createLineAt location =
        { Rows =
              [| [| true |]
                 [| true |]
                 [| true |]
                 [| true |] |]
          Location = location }

    let private createLAt location =
        { Rows =
              [| [| false; true |]
                 [| false; true |]
                 [| true; true |] |]
          Location = location }

    let private createTriangleAt location =
        { Rows =
              [| [| true; false |]
                 [| true; true |]
                 [| true; false |] |]
          Location = location }

    let private createZigZagAt location =
        { Rows =
              [| [| true; false |]
                 [| true; true |]
                 [| false; true |] |]
          Location = location }

    let createAt location blockType =
        match blockType with
        | Square -> createSquareAt location
        | Line -> createLineAt location
        | L -> createLAt location
        | Triangle -> createTriangleAt location
        | ZigZag -> createZigZagAt location

    let generateRandomAt location =
        let random = Random().Next(numberOfBlockTypes)
        match random with
        | 0 -> createAt location Square
        | 1 -> createAt location Line
        | 2 -> createAt location L
        | 3 -> createAt location Triangle
        | 4 -> createAt location ZigZag
        | _ -> createAt location Square

    let generateRandom () = generateRandomAt { X = 0; Y = 0 }

    let private newLocation oldLocation rowCount columnCount =
        let middleX = columnCount / 2
        let middleY = rowCount / 2
        { Location.X = oldLocation.X + middleY - middleX
          Location.Y = oldLocation.Y + middleX - middleY }

    let rotate block =
        let columnCount = Array.head block.Rows |> Array.length

        let createRowFromColumn columnIndex =
            Array.map (Array.item columnIndex) block.Rows

        let newRows =
            Seq.map (createRowFromColumn >> Array.rev) (seq { 0 .. columnCount - 1 })
            |> Seq.toArray

        let newRowCount = newRows.Length
        let newColumnCount = Array.head newRows |> Array.length

        { block with
              Rows = newRows
              Location = newLocation block.Location newRowCount newColumnCount }

    let move direction block =
        match direction with
        | Left ->
            { block with
                  Location =
                      { Y = block.Location.Y
                        X = block.Location.X - 1 } }
        | Right ->
            { block with
                  Location =
                      { Y = block.Location.Y
                        X = block.Location.X + 1 } }
        | Down ->
            { block with
                  Location =
                      { X = block.Location.X
                        Y = block.Location.Y + 1 } }
        | Rotate -> rotate block

    let tryItem row column block =
        Array.tryItem row block.Rows
        |> Option.bind (Array.tryItem column)

    let render block =
        let largestHeight = 4
        let largestWidth = 4

        let renderRow rowIndex =
            let renderCell columnIndex =
                let cell =
                    tryItem rowIndex columnIndex block
                    |> Option.defaultValue false

                if cell then "X" else " "

            Seq.map renderCell (seq { 0 .. largestWidth - 1 })
            |> Seq.toArray

        Seq.map renderRow (seq { 0 .. largestHeight - 1 })
        |> Seq.toArray

    let width block =
        Array.tryHead block.Rows
        |> Option.map Array.length
        |> Option.defaultValue 0
