module Grid

open System
open Utils

type GridItem =
    | Character of char: char * color: Option<ConsoleColor>
    | LineBreak

type Grid = {
    Row        : int
    Column     : int
    Items : GridItem list
}

module GridItem =
    let replicate (count: int) (item: GridItem) : list<GridItem> =
        fun _ -> item
        |> List.init count

    let replicateList (count: int) (li: list<GridItem>) : list<GridItem> =
        [1 .. count]
        |> List.fold (fun grid _ -> grid @ li) []

    let print (item: GridItem) =
        match item with
        | LineBreak -> printfn ""
        |Character (char, maybeColor) ->
            char
            |> sprintf "%c "
            |> printColored (Option.defaultValue Console.ForegroundColor maybeColor)


let charToCharacter (c: char) : GridItem =
    Character (c, None)

let generateInitialGrid (row: int) (column: int): Grid =
    {   
        Row = row
        Column = column
        Items =
            '*'
            |> charToCharacter
            |> GridItem.replicate column
            |> List.append [LineBreak]
            |> GridItem.replicateList row
    }

    


let printGrid (grid: Grid) =
    grid.Items
    |> List.iter GridItem.print