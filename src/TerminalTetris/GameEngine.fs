module GameEngine

open System
open TerminalTetris

let mutable private timer = Option<Timers.Timer>.None

let run (tick: unit -> unit) =
    timer <- Some(new Timers.Timer(2000.0))
    timer.Value.Elapsed.Add(fun _ -> tick())
    timer.Value.AutoReset <- true
    timer.Value.Enabled <- true

let waitForKey (keyPressHandler: ConsoleKeyInfo -> unit) =
    while true do
        Console.ReadKey() |> keyPressHandler

let private incrementGameSpeed (_: unit) =
    match timer with
        | None -> ignore()
        | Some t ->
            let decrement = 0.1 * t.Interval
            t.Interval <- t.Interval - decrement

let private stop (_: unit) =
    match timer with
        | None -> ignore()
        | Some t -> t.Stop()

let handleGameEvent (gameEventArgs: GameEventArgs) =
    match gameEventArgs with
    | GameEventArgs.RowsCleared _ -> incrementGameSpeed ()
    | GameEventArgs.GameOver -> stop ()
