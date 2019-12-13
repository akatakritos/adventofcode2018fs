// Learn more about F# at http://fsharp.org

open System

let input s =
    System.IO.File.ReadAllText("inputs\\" + s)

let splitLines (s: string) =
    s.Split('\n')

[<EntryPoint>]
let main argv =
    let day = argv.[0]

    match day with
        | "1" ->
            input "puzzle01.txt"
            |> Puzzle01.execute
        | _ -> printfn "havent done that day yet."

    0



