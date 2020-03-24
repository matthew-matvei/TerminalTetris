module StringHelpers

let tryItem (index: uint32) (string: string) =
    try
        Some(string.[int index])
    with
        | _ -> Option<char>.None
