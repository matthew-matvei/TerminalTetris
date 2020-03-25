module Draw

open System
open System.Collections.Concurrent

let private printQueue = new ConcurrentQueue<Location.Location * string>()

let printAt (location: Location.Location) (value: string) =
    printQueue.Enqueue((location, value))

    while not printQueue.IsEmpty do
        let success, (l, v) = printQueue.TryDequeue()
        if success then
            Console.SetCursorPosition(l.X, l.Y)
            printf "%s" v
