module ArrayHelpers

open System

let tryGet (arr: 'a array) (index: int) =
    try
        Some(Array.get arr index)
    with
    | :? IndexOutOfRangeException -> None
