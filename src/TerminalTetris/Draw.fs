namespace TerminalTetris

open System

module Draw =

    let private sync = Object()

    let printAt location value =
        lock sync (fun _ ->
            Console.SetCursorPosition(location.X, location.Y)
            printf "%s" value)
