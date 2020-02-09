module GameEngine

open System

let run (tick: unit -> unit) =
    let timer = new Timers.Timer(2000.0)
    timer.Elapsed.Add(fun _ -> tick())
    timer.AutoReset <- true
    timer.Enabled <- true

let waitForKey (keyPressHandler: ConsoleKeyInfo -> unit) =
    while true do
        Console.ReadKey() |> keyPressHandler
