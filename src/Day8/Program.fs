open Library
open System.Text.RegularExpressions

let real_input = AoC.input 8

let test_input =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

let toSeq (pattern: string) =
    seq {
        let instructions = Util.chars pattern

        while true do
            yield! instructions
    }

let parseRules (rules: string) =
    rules.Split(
        "\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )
    |> Array.map (fun line ->
        let matches = Regex.Matches(line, "([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)")
        let from = matches[0].Groups[0]
        let left = matches[0].Groups[1]
        let right = matches[0].Groups[2]
        (from.Value, (left.Value, right.Value)))
    |> Map.ofArray

let splitLines (input: string) =
    match
        input.Split
            (
                "\n\n",
                System.StringSplitOptions.RemoveEmptyEntries
                ||| System.StringSplitOptions.TrimEntries
            )
        with
    | [| pattern: string; rules: string |] -> ((toSeq pattern), (parseRules rules))
    | a -> failwith (sprintf "could not parse lines: %A" a)


let real_lines = splitLines real_input
printfn "%A" real_lines
let test_lines = splitLines test_input

let parseLine s = 1

let part1 (pattern, rules: Map<string, (string * string)>) =
    seq {
        let mutable state: string = "AAA"
        let mutable steps: int = 0

        for instruction in pattern do
            state <-
                match instruction with
                | "L" -> fst rules[state]
                | "R" -> snd rules[state]
                | _ -> failwith "unknown pattern"

            steps <- steps + 1
            if state = "ZZZ" then yield steps
    }
    |> Seq.head


// pattern |> Seq.fold (fun acc, instruction -> acc) "AAA"
// "AAA" |> Array.map parseLine |> Array.sum

printfn "part1 test: 6 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 (input) =
    // input |> Array.map parseLine |> Array.sum
    1

printfn "part2 test: YYY = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
