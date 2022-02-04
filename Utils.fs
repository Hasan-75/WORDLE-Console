module Utils

open System
open System.IO

type Config = {
    WordFilePath : string
    RowNumber    : int
    ColumnNumber : int
    WordLength   : int
}

let printColored (color: ConsoleColor) (text: string) =
    Console.ForegroundColor <- color
    printf "%s" text
    Console.ResetColor()

let printError =
    printColored ConsoleColor.Red

let loadFile (path: string) : seq<string> =
    path
    |> File.ReadAllLines
    |> seq

let capitalize (text: string) : string =
    text.ToUpper()