module Puzzle02Tests

open Xunit
open FsUnit.Xunit
open Puzzle02

[<Fact>]
let ``hasDupes works`` () =
    "abcdef" |> BoxId.parse |> BoxId.hasDupes |> should equal false
    "bababc" |> BoxId.parse |> BoxId.hasDupes |> should equal true

[<Fact>]
let ``diffCount two`` () =
    let a = BoxId "abcde"
    let b = BoxId "axcye"
    let result = BoxId.diffCount a b
    result |> should equal 2

[<Fact>]
let ``findBoxes works`` () =
    let boxIds = [
        BoxId "abcde";
        BoxId "fghij";
        BoxId "klmno";
        BoxId "pqrst";
        BoxId "fguij";
        BoxId "axcye";
        BoxId "wvxyz" ]

    let result = findBoxes boxIds

    result |> should equal (Some (BoxId "fghij", BoxId "fguij"))
