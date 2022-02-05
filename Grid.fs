module Grid

open System
open Utils

type GridItem =
    | Character of char: char * color: Option<ConsoleColor>
    | LineBreak

type Grid = {
    Row    : int
    Column : int
    Items  : GridItem list
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

    let charToCharacter (color: option<ConsoleColor>) (c: char) : GridItem =
        Character (c, color)


let stringToGridRow (s: string) : list<GridItem> =
    s
    |> Seq.map (fun c -> GridItem.charToCharacter None c)
    |> List.ofSeq
    |> fun li -> li @ [LineBreak]

let stringAllGreen (s: string) : list<GridItem> =
    s
    |> Seq.map (fun c -> GridItem.charToCharacter (Some ConsoleColor.Green) c)
    |> List.ofSeq
    |> fun li -> li @ [LineBreak]


let generateInitialGrid (row: int) (column: int): Grid =
    {   
        Row = row
        Column = column
        Items =
            '*'
            |> GridItem.charToCharacter None
            |> GridItem.replicate column
            |> fun li -> li @ [LineBreak]
            |> GridItem.replicateList row
    }

let updateGrid (currentRound: int) (current: Grid) (row: list<GridItem>) : Grid =
    {
        current with
            Items =
                current.Items
                |> fun li ->
                    (List.take (currentRound * row.Length) li)
                    @ row
                    @ (List.skip ((currentRound + 1) * row.Length) li)
    }

let printGrid (grid: Grid) : Grid =
    printfn ""
    grid.Items
    |> List.iter GridItem.print

    grid