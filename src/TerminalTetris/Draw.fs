module Draw

open System

let private sync = new Object()

let printAt (location: Location.Location) (value: string) =
    lock sync (fun _ ->
        Console.SetCursorPosition(location.X, location.Y)
        printf "%s" value
    )
