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

type Number = { start: Point; length: int; num: int }

type Symbol =
    { start: Point
      length: int
      value: string }

let find (regex: string) (input: array<string>) : seq<Symbol> =
    seq {
        for l in 0 .. input.Length - 1 do
            let line = input.[l]

            for rmatch in Regex.Matches(line, regex) do
                yield
                    { start = { line = l; col = rmatch.Index }
                      length = rmatch.Length
                      value = rmatch.Value }
    }

let findSymbols (input: array<string>) : Set<Point> =
    find "[^0-9.]" input
    |> Seq.map (fun symbol -> symbol.start)
    |> Set.ofSeq

printfn "symbols: %A" (findSymbols test_lines)

let findNumbers (input: array<string>) =
    find "[0-9]+" input
    |> Seq.map (fun symbol ->
        { start = symbol.start
          length = symbol.length
          num = Util.parseInt symbol.value |> Option.get })

printfn "%A" (findNumbers test_lines)

let neighbors (start: Point) length =
    Set.ofSeq (
        seq {
            let { line = line; col = col } = start

            for i in col - 1 .. col + length do
                for j in line - 1 .. line + 1 do
                    yield { line = j; col = i }
        }
    )

let partNumbers (numbers: seq<Number>) symbols =
    numbers
    |> Seq.filter (fun number ->
        Set.intersect (neighbors number.start number.length) symbols
        // |> Set.map (fun set ->
        // printfn "%A %A" number set
        // set)
        |> Set.isEmpty
        |> (not))

let part1 (input: array<string>) =
    partNumbers (findNumbers input) (findSymbols input)
    |> Seq.map (fun number -> number.num)
    |> Seq.sum

printfn "part1 test: 4361 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let findGearRatios (parts: seq<Number>) (input: array<string>) =
    find "\*" input
    |> Seq.map (fun gear ->
        parts
        |> Seq.filter (fun number ->
            Set.intersect (neighbors number.start number.length) (Set.ofList [ gear.start ])
            |> Set.isEmpty
            |> (not)))
    |> Seq.filter (fun adjacent -> Seq.length adjacent = 2)
    |> Seq.map (fun adjacent ->
        adjacent
        |> Seq.map (fun number -> number.num)
        |> Seq.reduce (*))

let part2 (input: array<string>) =
    let parts = partNumbers (findNumbers input) (findSymbols input)
    findGearRatios parts input |> Seq.sum

printfn "part2 test: 467835 = %d" (part2 test_lines)


printfn "part2: %d" (part2 real_lines)
