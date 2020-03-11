module Draw

open System

let printAt (location: Location.Location) (value: string) =
    Console.SetCursorPosition(location.X, location.Y)
    printf "%s" value
