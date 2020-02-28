module Draw

open System
open System.Collections.Concurrent

let mutable private previousMatrix = Option<string[][]>.None

let private drawQueue = new ConcurrentQueue<(Location.Location * string[][])>()

let printAt (location: Location.Location) (value: string) =
    Console.SetCursorPosition(location.X, location.Y)
    printf "%s" value

let private doWork (location: Location.Location) (matrix: string[][]) =
    let rowLength = 
        Array.tryHead matrix 
        |> Option.map Array.length 
        |> Option.defaultValue 0

    for rowIndex in seq { 0 .. matrix.Length - 1 } do
        for columnIndex in seq { 0 .. rowLength - 1 } do
            let previousValue = 
                previousMatrix
                |> Option.bind (Array2DHelpers.tryItem rowIndex columnIndex)

            let currentValue = Array2DHelpers.item rowIndex columnIndex matrix

            if previousValue.IsNone || previousValue.Value <> currentValue then
                printAt { X = location.X + columnIndex; Y = location.Y + rowIndex } currentValue
            
    previousMatrix <- Some(matrix)

let matrixAtLocation (location: Location.Location) (matrix: string[][]) =
    drawQueue.Enqueue((location, matrix))

    while not (drawQueue.IsEmpty) do
        let success, (l, m) = drawQueue.TryDequeue()
        if success then
            doWork l m
