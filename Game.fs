module Game

open Utils
open Grid
open GridItem
open System

type Result =
    | Solved  of round: int
    | StillOn of round: int
    | Lost

let loadWords (filePath: string) (wordLength: int) : seq<string> =
    filePath
    |> loadFile
    |> Seq.filter
        (fun w ->
            w.Length = wordLength
            && (doesContainUniqueCharacters w)
        )
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

let calculateColor (answerCharIndexMap: Map<char, int>) (guess: string) : list<GridItem> =

    let guessCharIndexMap = stringToCharIndexMap guess

    guess
    |> stringToGridRow
    |> List.map (
        fun gridItem ->
            match gridItem with
            | LineBreak -> LineBreak
            | Character (c, _) ->
                let maybeIndexInAnswer = Map.tryFind c answerCharIndexMap
                let maybeIndexInGuess  = Map.tryFind c guessCharIndexMap

                (maybeIndexInAnswer, maybeIndexInGuess)
                ||> Option.map2 (
                    fun indexInAnswer indexInGuess ->
                        match indexInAnswer = indexInGuess with
                        | true  -> GridItem.charToGreenCharacter c
                        | false -> GridItem.charToYellowCharacter c
                )
                |> Option.defaultValue (GridItem.charToGrayCharacter c)
        )

let calculateResult (guess: string) (answer: string) (playedRound: int) (totalRound: int) =
    match guess = answer with
    | true  -> Solved playedRound
    | false ->
        match (playedRound + 1) >= totalRound with
        | true  -> Lost
        | false -> StillOn playedRound

let startRound (wordLength: int) (answer: string) (totalRound: int) (wordList: list<string>) (currentGrid: Grid) : Result =

    let answerCharIndexMap =
        answer
        |> stringToCharIndexMap

    let rec nextRound (currentRound: int) (currentGrid: Grid) =
        printfn ""

        let guess =
            input()
            |> capitalize

        match validate wordList guess with
        | Ok ->
            let updatedGrid =
                guess
                |> calculateColor answerCharIndexMap
                |> updateGrid currentRound currentGrid
                |> printGrid
                |> printKeyboardForGrid

            calculateResult guess answer currentRound totalRound
            |> function
                | Solved round  -> Solved round
                | StillOn round -> nextRound (round + 1) updatedGrid
                | Lost          -> Lost

        | Error e->
            match e with
            | InvalidWordLength ->
                printError (sprintf "Your guess should have exact %i letters\n" wordLength)
            | InvalidCharacter ->
                printError "Your guess should have alphabets only\n"
            | NotExistInDictionary ->
                printError "This word doesn't exist in game's dictionary\n"

            nextRound currentRound currentGrid

    nextRound 0 currentGrid

let startGame() : unit =
    printfn "Game started... \n"

    let config : Config =
        {
            WordFilePath = "./Resources/words_alpha.txt"
            RowNumber    = 6
            ColumnNumber = 5
            WordLength   = 5
        }

    let wordList =
        loadWords config.WordFilePath config.WordLength
        |> List.ofSeq

    let randomWord =
        wordList
        |> chooseRandomWord

    let grid =
        generateInitialGrid config.RowNumber config.ColumnNumber
        |> printGrid

    let result =
        grid
        |> startRound config.WordLength randomWord config.RowNumber wordList

    match result with
    | Solved r ->
        sprintf "\nCongrats! you've solved the puzzle at %i guess! \n" (r + 1)
        |> printColored ConsoleColor.Green
    | Lost ->
        sprintf "\nGame over! The word was %s. Better luck next time... \n" randomWord
        |> printColored ConsoleColor.Red
    | _ -> ()