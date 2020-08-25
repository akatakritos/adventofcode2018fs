module Puzzle03
open Utils

type Claim = {
    ClaimId: string;
    Left: int;
    Top: int;
    Width: int;
    Height: int;
}

type Dimension = {
    Width: int;
    Height: int;
}

module Claim =
    let parse = function
        | Regex "#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" [id; left; top; width; height;] ->
            Some { ClaimId = id; Left = int left; Top = int top; Width = int width; Height = int height }
        | _ -> None


    let dimensions claims =
        claims
        |> List.fold (fun dim claim -> { dim with Dimension.Width = max dim.Width (claim.Left + claim.Width); Height = max dim.Height (claim.Top + claim.Height); }) { Height = 0; Width = 0; }


type FabricCell =
    | Empty
    | Single of string
    | Multiple of int

module FabricCell =
    let isMultiple = function
        | Multiple _ -> true
        | _ -> false

    let isSingle = function
        | Single _ -> true
        | _ -> false

type FabricSpace = {
    Spaces: FabricCell[,]
    Dimensions: Dimension
}

module FabricSpace =
    let create dimensions =
        { Spaces = Array2D.create dimensions.Height dimensions.Width Empty; Dimensions = dimensions }

    let applyClaim claim space =
        for y in claim.Top..(claim.Top + claim.Height - 1) do
            for x in claim.Left..(claim.Left + claim.Width - 1) do
                match space.Spaces.[y, x] with
                    | Empty ->
                        space.Spaces.[y, x] <- Single claim.ClaimId
                    | Single _ ->
                        space.Spaces.[y, x] <- Multiple 2
                    | Multiple n ->
                        space.Spaces.[y, x] <- Multiple (n+1)

        space

    let cells space =
        seq {
            for y in 0..(space.Dimensions.Height-1) do
                for x in 0..(space.Dimensions.Width-1) do
                    yield space.Spaces.[y, x];
        }

    let claimCells claim space =
        seq {
            for y in claim.Top..(claim.Top + claim.Height - 1) do
               for x in claim.Left..(claim.Left + claim.Width - 1) do
                    yield space.Spaces.[y, x]
        }

    let countMultiples space =
        cells space
        |> Seq.filter FabricCell.isMultiple
        |> Seq.length

    let isFullyRepresented claim space =
        claimCells claim space
        |> Seq.forall FabricCell.isSingle


let execute s =
    let claims =
        splitLines s
        |> Seq.map (Claim.parse >> (fun claim -> claim.Value))
        |> Seq.toList

    let space =
        claims
        |> Claim.dimensions
        |> FabricSpace.create

    claims |> List.iter (fun claim -> FabricSpace.applyClaim claim space |> ignore)

    let multipleCount = FabricSpace.countMultiples space

    printfn "Spaces with multiple: %d" multipleCount

    let singleCell =
        claims
        |> Seq.tryFind (fun claim -> FabricSpace.isFullyRepresented claim space)

    printfn "Claim with single space %A" singleCell


