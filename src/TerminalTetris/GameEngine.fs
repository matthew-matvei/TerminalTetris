module GameEngine

open System
open TerminalTetris

let mutable private timer = Option<Timers.Timer>.None

let run (tick: unit -> unit) =
    timer <- Some(new Timers.Timer(2000.0))
    timer.Value.Elapsed.Add(fun _ -> tick ())
    timer.Value.AutoReset <- true
    timer.Value.Enabled <- true

let waitForKey keyPressHandler =
    while true do
        Console.ReadKey() |> keyPressHandler

let private incrementGameSpeed () =
    match timer with
    | None -> ignore ()
    | Some t ->
        let decrement = 0.1 * t.Interval
        t.Interval <- t.Interval - decrement

let private stop () =
    match timer with
    | None -> ignore ()
    | Some t -> t.Stop()

let handleGameEvent gameEventArgs =
    match gameEventArgs with
    | RowsCleared _ -> incrementGameSpeed ()
    | GameOver -> stop ()
