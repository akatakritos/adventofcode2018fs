module Puzzle02

type BoxId =
    BoxId of string

module BoxId =
    let parse s =
        BoxId s

    let unwrap boxid =
        match boxid with
            BoxId s -> s


    let hasDupes boxid =
        boxid
        |> unwrap
        |> Seq.groupBy (id)
        |> Seq.map (snd >> Seq.length)
        |> Seq.contains 2

    let hasTrips boxid =
        boxid
        |> unwrap
        |> Seq.groupBy (id)
        |> Seq.map (snd >> Seq.length)
        |> Seq.contains 3

    let private tupleMatch (a, b) =
        a <> b

    let diffCount box1 box2 =
        (unwrap box1)
        |> Seq.zip (unwrap box2)
        |> Seq.filter tupleMatch
        |> Seq.length

    let commonLetters (box1, box2) =
        (unwrap box1)
        |> Seq.zip (unwrap box2)
        |> Seq.filter (fun (a, b) -> a = b)
        |> Seq.map fst
        |> Array.ofSeq
        |> System.String


let combineAll (items: 'a list) =
    let comb (h: 'a list) =
        match h with
            | head::tail -> tail |> Seq.map(fun item -> (head, item))
            | _ -> Seq.empty


    let rec inner (l: 'a list) =
        seq {
            match l with
                | head::tail ->
                    yield! comb l
                    yield! inner tail
                | _ -> ()
        }

    inner items

let findBoxes (boxIds:BoxId list) =
    combineAll boxIds
    |> Seq.tryFind (fun (a, b) -> BoxId.diffCount a b = 1)


let execute input =
    let boxIds =
        input
        |> Utils.splitLines
        |> Seq.map BoxId.parse
        |> Seq.toList

    let countDupes = boxIds |> Seq.filter BoxId.hasDupes |> Seq.length
    let countTrips = boxIds |> Seq.filter BoxId.hasTrips |> Seq.length
    let checksum = countDupes * countTrips
    printfn "Checksum: %d" checksum

    findBoxes boxIds
    |> Option.map BoxId.commonLetters
    |> printfn "Common Letters: %A"

