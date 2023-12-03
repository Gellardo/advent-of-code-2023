open Library
open System.Text.RegularExpressions

let real_input = AoC.input 3

let test_input =
    """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

let splitLines (input: string) =
    input.Split(
        "\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let real_lines = splitLines real_input
let test_lines = splitLines test_input

type Point = { line: int; col: int }

type Number =
    { start: Point
      length: int
      value: int }

let findSymbols (input: array<string>) : Set<Point> =
    let mutable points = []

    for l in 0 .. input.Length - 1 do
        let line = input.[l]

        for c in 0 .. line.Length - 1 do
            let char = line.Chars(c)

            if (not (List.contains char ('.' :: [ '0' .. '9' ]))) then
                points <- { line = l; col = c } :: points

    Set.ofList points

printfn "symbols: %A" (findSymbols test_lines)

let findNumbers (input: array<string>) =

    seq {
        for l in 0 .. input.Length - 1 do
            let line = input.[l]

            for rmatch in Regex.Matches(line, "[0-9]+") do
                yield
                    { start = { line = l; col = rmatch.Index }
                      length = rmatch.Length
                      value = Util.parseInt rmatch.Value |> Option.get }
    }

printfn "%A" (findNumbers test_lines)

let neighbors (start: Point) length =
    Set.ofSeq (
        seq {
            let { line = line; col = col } = start

            for i in col - 1 .. col + length + 1 do
                for j in line - 1 .. line + 1 do
                    yield { line = j; col = i }
        }
    )

let partNumbers (numbers: seq<Number>) symbols =
    numbers
    |> Seq.filter (fun number ->
        Set.intersect (neighbors (number.start) (number.length)) symbols
        |> Set.isEmpty
        |> (not))

let part1 (input: array<string>) =
    partNumbers (findNumbers input) (findSymbols input)
    |> Seq.map (fun number -> number.value)
    |> Seq.sum

printfn "part1 test: 4361 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 (input: string) = 0

printfn "part2 test: YY = %d" (part2 test_input)


printfn "part2: %d" (part2 real_input)
