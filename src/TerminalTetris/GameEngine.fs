module GameEngine

open System
open System.Reactive.Linq

let run (updateGrid: int64 -> unit) =
    let timer = Observable.Interval(TimeSpan.FromSeconds(2.0))
    use subscription = timer.Subscribe(updateGrid)

    let publishedLoop = timer.Publish()

    publishedLoop.Connect() |> ignore
    publishedLoop.Wait() |> ignore
