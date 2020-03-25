module Scene

open System

let getGameGridDimensions _ =
    { Dimensions.Height = 10;
      Dimensions.Width = 5 }

let private matrixAtLocation (location: Location.Location) (matrix: string[][]) (previousMatrix: Option<string[][]>) =
    let rowLength =
        Array.tryHead matrix
        |> Option.map Array.length
        |> Option.defaultValue 0

    for rowIndex in seq { 0 .. matrix.Length - 1 } do
        for columnIndex in seq { 0 .. rowLength - 1 } do
            let previousValue =
                previousMatrix
                |> Option.bind (Array.tryItem rowIndex)
                |> Option.bind (Array.tryItem columnIndex)

            let currentValue = Array.item rowIndex matrix |> Array.item columnIndex

            if previousValue.IsNone || previousValue.Value <> currentValue then
                Draw.printAt { X = location.X + columnIndex; Y = location.Y + rowIndex } currentValue

let private valueAtLocation (location: Location.Location) (value: string) (previousValue: Option<string>) =
    let maxValueLength = Math.Max(value.Length, Option.map String.length previousValue |> Option.defaultValue 0)

    for characterIndex in seq { 0 .. maxValueLength - 1 } do
        let previousValue = previousValue |> Option.bind (StringHelpers.tryItem (uint32 characterIndex))
        let currentValue = value |> StringHelpers.tryItem (uint32 characterIndex)

        match (previousValue, currentValue) with
        | (previous, current) when previous.IsSome && current.IsSome && previous.Value = current.Value ->
            ignore()
        | (_, current) when current.IsSome ->
            string current.Value |> Draw.printAt { X = location.X + characterIndex; Y = location.Y }
        | (_, current) when current.IsNone ->
            " " |> Draw.printAt { X = location.X + characterIndex; Y = location.Y }
        | (_, __) ->
            ignore()

let mutable private previousGameGrid = Option<string[][]>.None

let drawGameGrid (grid: string[][]) =
    matrixAtLocation { X = 0; Y = 0 } grid previousGameGrid
    previousGameGrid <- Some(grid)

let mutable private previousNextBlock = Option<string[][]>.None

let drawNextBlock (nextBlock: string[][]) =
    let location =
        { Location.X = getGameGridDimensions().Width + 3
          Location.Y = 4 }

    matrixAtLocation location nextBlock previousNextBlock
    previousNextBlock <- Some(nextBlock)

let mutable private previousScore = Option<string>.None

let drawScore (score: string) =
    let location =
        { Location.X = getGameGridDimensions().Width + 6
          Location.Y = 4 }

    valueAtLocation location score previousScore