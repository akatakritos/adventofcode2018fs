module Puzzle01Tests

open Xunit
open FsUnit.Xunit
open Puzzle01

[<Fact>]
let ``sumInts 1 1 1 is 3`` () =
    [1;1;1] |> sumInts |> should equal 3