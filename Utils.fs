module Utils

open System
open System.IO

type Config = {
    WordFilePath : string
    RowNumber    : int
    ColumnNumber : int
    WordLength   : int
}

type ValidationError =
    | InvalidWordLength
    | InvalidCharacter
    | NotExistInDictionary

type ValidationResult =
    | Ok
    | Error of ValidationError

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

let input() : string =
    "Input your guess:\n"
    |> printColored ConsoleColor.Cyan
    System.Console.ReadLine()

let containsAlphabetsOnly (text: string) : bool =
    text
    |> Seq.forall (fun c -> System.Char.IsLetter(c))

let doesContainUniqueCharacters (text: string) : bool =
    text
    |> seq
    |> Set.ofSeq
    |> Set.count
    |> (=) text.Length

let stringToCharIndexMap (str: string) : Map<char, int> =
    str
    |> Seq.mapi (fun i c -> c, i)
    |> Map.ofSeq