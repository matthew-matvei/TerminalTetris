namespace TerminalTetris

open System

module Draw =

    let private sync = Object()

    let printAt (location: Location) (value: string) =
        lock sync (fun _ ->
            Console.SetCursorPosition(location.X, location.Y)
            printf "%s" value)
