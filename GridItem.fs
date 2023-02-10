module GridItem

open System
open Utils

type GridItem =
    | Character of char: char * color: Option<ConsoleColor>
    | LineBreak

let replicate (count: int) (item: GridItem) : list<GridItem> =
    fun _ -> item
    |> List.init count

let replicateList (count: int) (li: list<GridItem>) : list<GridItem> =
    [1 .. count]
    |> List.fold (fun grid _ -> grid @ li) []

let print (item: GridItem) =
    match item with
    | LineBreak -> printfn ""
    | Character (char, maybeColor) ->
        char
        |> sprintf "%c "
        |> printColored (Option.defaultValue Console.ForegroundColor maybeColor)

let charToCharacter (color: option<ConsoleColor>) (c: char) : GridItem =
    Character (c, color)

let charToGreenCharacter = charToCharacter (Some ConsoleColor.Green)

let charToYellowCharacter = charToCharacter (Some ConsoleColor.Yellow)

let charToGrayCharacter = charToCharacter (Some ConsoleColor.DarkGray)