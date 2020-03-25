module Draw

open System
open System.Collections.Concurrent

let private printQueue = new ConcurrentQueue<Location.Location * string>()

let private doWork _ =
    while not printQueue.IsEmpty do
        let success, (l, v) = printQueue.TryDequeue()
        if success then
            lock printQueue (fun _ ->
                Console.SetCursorPosition(l.X, l.Y)
                printf "%s" v
            )

let private timer = new Timers.Timer(40.0)
timer.Elapsed.Add(fun _ -> doWork())
timer.AutoReset <- true
timer.Enabled <- true

let printAt (location: Location.Location) (value: string) =
    printQueue.Enqueue((location, value))
