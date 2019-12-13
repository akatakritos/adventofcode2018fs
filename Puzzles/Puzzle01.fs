module Puzzle01

let sumInts (ints: seq<int>) =
    ints |> Seq.sum

let execute1 (input: string) =
    input
    |> Utils.splitLines
    |> Seq.map int
    |> sumInts
    |> printfn "The sum is %d"

    printfn "It should be 411"

let execute2 (input: string) =
    let mutable set = new Set<int>([])

    let isInSet i =
        match set.Contains i with
            | true -> true
            | false ->
                set <- set.Add i
                false

    let source =
        input
        |> Utils.splitLines
        |> Seq.map int
        |> Seq.toArray

    let loop = seq {
        while true do
            yield! source
    }

    loop
    |> Seq.scan (+) 0
    |> Seq.find isInSet
    |> printfn "the first repeated value is %d. Last year it was 56360"


let execute input =
    execute1 input
    execute2 input
