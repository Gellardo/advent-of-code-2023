namespace Library

open System.IO

module AoC =
    let input day =
        try
            use streamreader = new StreamReader(sprintf "input/%d/input.txt" day)
            streamreader.ReadToEnd()
        with
        | :? FileNotFoundException as ex -> failwith $"File not found: {ex.Message}"
        | :? IOException as ex -> failwith $"An IO error occurred: {ex.Message}"

module Say =
    let hello name = sprintf "Hello %s" name
