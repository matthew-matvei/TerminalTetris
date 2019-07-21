module Draw

let matrix (matrix: string[][]) =
    for row in matrix do
        for column in row do
            printf "%s" column
        printfn ""
