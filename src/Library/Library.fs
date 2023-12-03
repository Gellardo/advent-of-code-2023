namespace Library

open System.IO

module AoC =
    let input day =
        try
            use streamreader = new StreamReader(sprintf "input/%d/input.txt" day)
            streamreader.ReadToEnd()
        with
        | :? FileNotFoundException as ex -> failwith $"File for day {day} not found: {ex.Message}"
        | :? IOException as ex -> failwith $"An IO error occurred: {ex.Message}"

module Util =
    let chars (s: string) =
        [| 0 .. s.Length - 1 |]
        |> Array.map (fun i -> s.Chars(i).ToString())

    let parseInt (s: string) =
        match System.Int32.TryParse(s) with
        | (true, n) -> Some n
        | _ -> None


module Say =
    let hello name = sprintf "Hello %s" name
