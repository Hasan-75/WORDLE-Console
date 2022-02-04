module Game

open Utils
open Grid

let config : Config =
    {   WordFilePath = "./Resources/words_alpha.txt"
        RowNumber = 6
        ColumnNumber = 5
        WordLength = 5
    }

let loadWords : seq<string> =
    config.WordFilePath
    |> loadFile
    |> Seq.filter
        (fun w -> w.Length = 5)

let chooseRandomWord (words: list<string>) : string=
    let randomIndex =
        System.Random().Next(0, words.Length)

    words
    |> List.item randomIndex
    |> capitalize

let startGame() : unit =
    printfn "Game started... \n"

    let randomWord = 
        loadWords
        |> List.ofSeq
        |> chooseRandomWord

    generateInitialGrid config.RowNumber config.ColumnNumber
    |> printGrid