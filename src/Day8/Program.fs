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
        "\n\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )
    |> Array.map (fun line -> line)

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
    | _ -> failwith ("could not parse lines")


let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine s = 1

let part1 (pattern, rules) =
    let mut state = "AAA"
    let mut steps = 0
    seq{
    for instruction in pattern do
        state <- rules[state][instruction]
        steps <- steps + 1
        if state = "ZZZ" then
            yield steps
    } |> Seq.take 1

    pattern |> Seq.fold (fun acc, instruction -> acc) "AAA"
    // "AAA" |> Array.map parseLine |> Array.sum
    1

printfn "part1 test: 6 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 (input: array<string>) =
    input |> Array.map parseLine |> Array.sum

printfn "part2 test: YYY = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
