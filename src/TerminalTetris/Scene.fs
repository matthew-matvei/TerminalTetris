namespace TerminalTetris

open System

module Scene =

    let getGameGridDimensions _ =
        { Dimensions.Height = 10;
          Dimensions.Width = 5 }

    let private matrixAtLocation (location: Location) (matrix: string[][]) (previousMatrix: Option<string[][]>) =
        let rowLength =
            Array.tryHead matrix
            |> Option.map Array.length
            |> Option.defaultValue 0

        for rowIndex in seq { 0 .. matrix.Length - 1 } do
            for columnIndex in seq { 0 .. rowLength - 1 } do
                let previousValue =
                    previousMatrix
                    |> Option.bind (ArrayHelpers2D.tryItem rowIndex columnIndex)

                let currentValue = Array.item rowIndex matrix |> Array.item columnIndex

                let print value =
                    Draw.printAt { X = location.X + columnIndex; Y = location.Y + rowIndex } value

                match previousValue with
                | None -> print currentValue
                | Some v when v <> currentValue -> print currentValue
                | _ -> ignore()

    let private valueAtLocation (location: Location) (value: string) (previousValue: Option<string>) =
        let maxValueLength = Math.Max(value.Length, Option.map String.length previousValue |> Option.defaultValue 0)

        for characterIndex in seq { 0 .. maxValueLength - 1 } do
            let previousValue = previousValue |> (Option.bind << StringHelpers.tryItem << uint32) characterIndex
            let currentValue = value |> (StringHelpers.tryItem << uint32) characterIndex

            match (previousValue, currentValue) with
            | (Some previous, Some current) when previous = current ->
                ignore()
            | (_, Some current) ->
                string current |> Draw.printAt { X = location.X + characterIndex; Y = location.Y }
            | (_, None) ->
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
