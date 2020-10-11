namespace TerminalTetris

open System

module Scene =

    type private State =
        { PreviousGameGrid: string array array option
          PreviousNextBlock: string array array option
          PreviousScore: string option }

    let mutable private _state =
        { PreviousGameGrid = None
          PreviousNextBlock = None
          PreviousScore = None }

    let getGameGridDimensions () =
        { Dimensions.Height = 10
          Dimensions.Width = 5 }

    let private matrixAtLocation location matrix previousMatrix =
        ArrayHelpers2D.iteri (fun (rowIndex, columnIndex) currentValue ->
            let previousValue =
                previousMatrix
                |> Option.bind (ArrayHelpers2D.tryItem rowIndex columnIndex)

            let print value =
                Draw.printAt
                    { X = location.X + columnIndex
                      Y = location.Y + rowIndex }
                    value

            match previousValue with
            | None -> print currentValue
            | Some v when v <> currentValue -> print currentValue
            | _ -> ignore ()) matrix

    let private valueAtLocation location value previousValue =
        let maxValueLength =
            Math.Max
                (String.length value,
                 Option.map String.length previousValue
                 |> Option.defaultValue 0)

        for characterIndex in seq { 0 .. maxValueLength - 1 } do
            let previousValue =
                previousValue
                |> Option.bind (StringHelpers.tryItem (uint32 characterIndex))

            let currentValue =
                value
                |> (StringHelpers.tryItem << uint32) characterIndex

            match (previousValue, currentValue) with
            | (Some previous, Some current) when previous = current -> ignore ()
            | (_, Some current) ->
                string current
                |> Draw.printAt
                    { X = location.X + characterIndex
                      Y = location.Y }
            | (_, None) ->
                " "
                |> Draw.printAt
                    { X = location.X + characterIndex
                      Y = location.Y }
            | (_, __) -> ignore ()

    let drawGameGrid grid =
        matrixAtLocation { X = 0; Y = 0 } grid _state.PreviousGameGrid
        _state <-
            { _state with
                  PreviousGameGrid = Some(grid) }

    let drawNextBlock nextBlock =
        let location =
            { Location.X = getGameGridDimensions().Width + 3
              Location.Y = 4 }

        matrixAtLocation location nextBlock _state.PreviousNextBlock
        _state <-
            { _state with
                  PreviousNextBlock = Some(nextBlock) }

    let drawScore score =
        let location =
            { Location.X = getGameGridDimensions().Width + 6
              Location.Y = 4 }

        valueAtLocation location score _state.PreviousScore
