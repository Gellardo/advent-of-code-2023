open Library
open System.Text.RegularExpressions

let real_input = AoC.input 4

let test_input =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

let splitLines (input: string) =
    input.Split(
        "\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine (line: string) =
    let numbers =
        line.Split("|")
        |> Array.map (fun s -> s.Split(" "))
        |> Array.map (fun a -> a |> Array.choose Util.parseInt |> Set.ofArray)

    numbers[0], numbers[1]

let overlapSize =
    parseLine
    >> fun (winning, chosen) -> Set.intersect winning chosen
    >> fun (overlap) -> overlap.Count

let part1 (input: array<string>) =
    input
    |> Seq.map overlapSize
    |> Seq.map (fun overlap -> (pown 2 overlap) / 2)
    |> Seq.sum

printfn "part1 test: 13 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)


let part2 (input: array<string>) =
    // let counts = Array.create input.Length 1

    input
    |> Seq.mapi (fun i line -> i, overlapSize line)
    |> Seq.fold
        (fun (counts: array<int>) (i, overlap) ->
            for won in 1..overlap do
                counts[i + won] <- counts[i + won] + counts[i]

            counts)
        (Array.create input.Length 1)
    |> Seq.sum

printfn "part2 test: 30 = %d" (part2 test_lines)


printfn "part2: %d" (part2 real_lines)
