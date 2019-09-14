module Draw

open System

let matrix (matrix: string[][]) =
    Console.Clear()
    for row in matrix do
        for column in row do
            printf "%s" column
        printfn ""
