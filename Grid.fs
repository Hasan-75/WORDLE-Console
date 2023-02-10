module Grid

open GridItem

type Grid = {
    Row    : int
    Column : int
    Items  : GridItem list
}

let stringToGridRow (s: string) : list<GridItem> =
    s
    |> Seq.map (fun c -> GridItem.charToCharacter None c)
    |> List.ofSeq
    |> fun li -> li @ [LineBreak]

let generateInitialGrid (row: int) (column: int): Grid =
    {   
        Row    = row
        Column = column
        Items  =
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
    grid.Items
    |> List.iter GridItem.print

    printfn ""
    grid

let charToCharacterMap (grid: Grid) : Map<char, GridItem> =
    grid.Items
    |> List.fold
        (fun acc gridItem ->
            match gridItem with
            | LineBreak -> acc
            | Character (c, _) ->
                acc
                |> Map.add c gridItem
        )
        Map.empty

let printKeyboardForGrid (grid: Grid) : Grid =
    let gridCharToCharacterMap = charToCharacterMap grid

    ['A' .. 'Z']
    |> List.iter
        (fun ch ->
            let maybeCharacterOfGrid = Map.tryFind ch gridCharToCharacterMap

            maybeCharacterOfGrid
            |> Option.defaultValue (GridItem.charToCharacter None ch)
            |> GridItem.print
        )
    printfn ""
    grid
