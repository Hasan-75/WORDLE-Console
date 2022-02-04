open System.IO
open Utils
open Game

[<EntryPoint>]
let main argv =
    printfn "Game starting..."
    try
        startGame()
    with
    | :? FileNotFoundException ->
        printError "File is not found"
    | e ->
        printError e.Message
    0 // return an integer exit code
