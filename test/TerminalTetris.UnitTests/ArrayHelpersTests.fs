module ArrayHelpersTests

open Xunit

[<Fact>]
let givenIndexNotInRangeOfArrayWhenTrySettingValueThenArrayNotModified () =
    let array = [| 1; 2; 3 |]
    let valueToSet = 10

    ArrayHelpers.trySet array (uint32 (array.Length + 1)) valueToSet

    Array.contains valueToSet array |> Assert.False

[<Fact>]
let givenIndexWithinRangeOfArrayWhenTrySettingValueThenArrayModified () =
    let array = [| 1; 2; 3 |]
    let valueToSet = 10

    ArrayHelpers.trySet array 0u valueToSet

    Array.contains valueToSet array |> Assert.True
