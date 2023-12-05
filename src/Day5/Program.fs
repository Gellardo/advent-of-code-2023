open Library
open System.Text.RegularExpressions

let real_input = AoC.input 5

let test_input =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

let splitLines (input: string) =
    input.Split(
        "\n\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine s = 1

type Mapping =
    { source: int64
      destination: int64
      length: int64 }

let parseMap (s: string) =
    s.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.removeAt 0
    |> Array.map (fun l ->
        l.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose Util.parseInt64)
    |> Array.map (fun l ->
        { source = l[1]
          destination = l[0]
          length = l[2] })

let mapSeeds (mappings: array<Mapping>) (seed: int64) =
    let mappedSeed =
        mappings
        |> Array.choose (fun mapping ->
            match mapping with
            | { source = source
                destination = destination
                length = length } when 0L <= seed - source && seed - source < length ->
                Some(destination + seed - source)
            | _ -> None)

    match mappedSeed with
    | [| result |] -> result
    | [||] -> seed
    | _ -> failwith ("multiple mapping options")

let doMapping (seedString: string) (mapStrings: list<string>) =
    let seeds =
        seedString.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose Util.parseInt64

    let maps = mapStrings |> Seq.map parseMap

    maps
    |> Seq.fold (fun seeds map -> seeds |> Array.map (mapSeeds map)) seeds

let part1 (input: array<string>) =
    match (List.ofArray input) with
    | seeds :: maps -> doMapping seeds maps
    | [] -> failwith "seeds not found"
    |> Seq.min

printfn "part1 test: 35 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 (input: array<string>) =
    input |> Array.map parseLine |> Array.sum

printfn "part2 test: YYY = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
