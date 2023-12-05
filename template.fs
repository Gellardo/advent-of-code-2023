open Library
open System.Text.RegularExpressions

let real_input = AoC.input DAY

let test_input =
    """TESTINPUT"""

let splitLines (input: string) =
    input.Split(
        "\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine s = 1

let part1 (input: array<string>) =
    input |> Array.map parseLine |> Array.sum

printfn "part1 test: XXX == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 (input: array<string>) =
    input |> Array.map parseLine |> Array.sum

printfn "part2 test: YYY = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
