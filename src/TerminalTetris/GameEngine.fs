module GameEngine

open System

let mutable private timer = Option<Timers.Timer>.None

let run (tick: unit -> unit) =
    timer <- Some(new Timers.Timer(2000.0))
    timer.Value.Elapsed.Add(fun _ -> tick())
    timer.Value.AutoReset <- true
    timer.Value.Enabled <- true

let incrementGameSpeed (_: unit) =
    if timer.IsNone then
        ignore()

    let decrement = 0.1 * timer.Value.Interval
    let currentInterval = timer.Value.Interval
    timer.Value.Interval <- currentInterval - decrement

let waitForKey (keyPressHandler: ConsoleKeyInfo -> unit) =
    while true do
        Console.ReadKey() |> keyPressHandler
