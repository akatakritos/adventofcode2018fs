// Learn more about F# at http://fsharp.org

open System

let input s =
    System.IO.File.ReadAllText("inputs\\" + s)

let splitLines (s: string) =
    s.Split('\n')

let puzzleDay (argv: string array) =
    if argv.Length >= 1 then
        Some argv.[0]
    else
        None

[<EntryPoint>]
let main argv =
    let day = puzzleDay argv

    match day with
        | Some "1" ->
            input "puzzle01.txt" |> Puzzle01.execute
        | Some "2" ->
            input "puzzle02.txt" |> Puzzle02.execute
        | Some "3" -> input "puzzle03.txt" |> Puzzle03.execute
        | Some _ -> printfn "havent done that day yet."
        | None -> printfn "Pass day number 'dotnet run -- 2'"

    0



