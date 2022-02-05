module Game

open Utils
open Grid
open System

let config : Config =
    {   WordFilePath = "./Resources/words_alpha.txt"
        RowNumber = 6
        ColumnNumber = 5
        WordLength = 5
    }

type Result =
    | Solved of round: int
    | StillOn
    | Lost

let loadWords : seq<string> =
    config.WordFilePath
    |> loadFile
    |> Seq.filter
        (fun w -> w.Length = config.WordLength && (allUniqueChar w))
    |> Seq.map capitalize

let validate (wordList: list<string>) (word: string) : ValidationResult =
    if word.Length <> 5 then
        Error InvalidWordLength

    elif word |> (containsAlphabetsOnly >> not) then
        Error InvalidCharacter

    elif wordList |> List.contains word |> not then
        Error NotExistInDictionary

    else
        Ok


let chooseRandomWord (words: list<string>) : string=
    let randomIndex =
        System.Random().Next(0, words.Length)

    words
    |> List.item randomIndex
    |> capitalize

let calculateColor (answer: string) (guess: string) : list<GridItem> =
    let ansChars = answer |> seq |> List.ofSeq

    guess
    |> stringToGridRow
    |> List.mapi (
        fun i x ->
            match x with
            | LineBreak -> LineBreak
            | Character (c, _) ->
                ansChars
                |> Seq.contains c
                |> function
                    | true ->
                        let ansCharAtCurrentPos = List.item i ansChars
                        match c = ansCharAtCurrentPos with
                        | true -> GridItem.charToCharacter (Some ConsoleColor.Green) c
                        | false -> GridItem.charToCharacter (Some ConsoleColor.Yellow) c

                    | false -> GridItem.charToCharacter (Some ConsoleColor.DarkGray) c
        )

let calculateResult (guess: string) (answer: string) (playedRound: int) (totalRound: int) (grid: Grid) =
    match guess = answer with
    | true -> grid, Solved playedRound
    | false ->
        match (playedRound + 1) >= totalRound with
        | true -> grid, Lost
        | false -> grid, StillOn

let rec nextRound (answer: string) (currentRound: int) (totalRound: int) (wordList: list<string>) (currentGrid: Grid) : Result =
    printfn ""

    let guess =
        input()
        |> capitalize

    match validate wordList guess with
    | Ok ->
        guess
        |> calculateColor answer
        |> updateGrid currentRound currentGrid
        |> printGrid
        |> calculateResult guess answer currentRound totalRound
        |> function
            | _, Solved r -> Solved r
            | grid, StillOn -> grid |> nextRound answer (currentRound + 1) totalRound wordList
            | _, Lost -> Lost

    | Error e->
        match e with
        | InvalidWordLength ->
            printError (sprintf "Your guess should have exact %i letters\n" config.WordLength)
        | InvalidCharacter ->
            printError "Your guess should have alphabets only\n"
        | NotExistInDictionary ->
            printError "This word doesn't exist in game's dictionary\n"
        nextRound answer currentRound totalRound wordList currentGrid


let startGame() : unit =
    printfn "Game started... \n"

    let wordList =
        loadWords
        |> List.ofSeq

    let randomWord =
        wordList
        |> chooseRandomWord

    let grid =
        generateInitialGrid config.RowNumber config.ColumnNumber
        |> printGrid

    let result =
        grid
        |> nextRound randomWord 0 config.RowNumber wordList

    match result with
    | Solved r ->
        sprintf "\nCongrats! you've solved the puzzle at %i guess! \n" (r + 1)
        |> printColored ConsoleColor.Green
    | Lost ->
        sprintf "\nGame over! The word was %s. Better luck next time... \n" randomWord
        |> printColored ConsoleColor.Red
    | _ -> ()