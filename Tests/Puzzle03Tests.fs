module Puzzle03Tests

open Xunit
open FsUnit.Xunit
open Puzzle03


[<Fact>]
let ``parse works`` () =
    let claim = Claim.parse "#1 @ 1,3: 4x4"
    claim |> should equal (Some { ClaimId = "1"; Left = 1; Top = 3; Width = 4; Height = 4})

