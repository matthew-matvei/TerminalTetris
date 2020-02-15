module Draw

open System

let matrixAtLocation (location: Location.Location) (matrix: string[][]) =
    Console.SetCursorPosition(location.X, location.Y)
    for row in matrix do
        for column in row do
            printf "%s" column
        printfn ""
