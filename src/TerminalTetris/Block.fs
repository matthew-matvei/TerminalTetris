module Block

type Block = { Rows: Row.Row[]; Location: Location.Location }
type BlockType =
    | Square

let private createSquare _ =
    { Rows = [| [| true; true |]; [| true; true; |] |]
      Location = { X = 0; Y = 0 }}

let create blockType =
    match blockType with
    | Square -> createSquare()
