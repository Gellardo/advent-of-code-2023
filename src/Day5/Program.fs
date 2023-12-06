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
    |> Seq.fold (fun seeds map -> seeds |> Seq.map (mapSeeds map)) seeds

let part1 (input: array<string>) =
    match (List.ofArray input) with
    | seeds :: maps -> doMapping seeds maps
    | [] -> failwith "seeds not found"
    |> Seq.min

printfn "part1 test: 35 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let mapSeedRanges (mappings: array<Mapping>) (seedRanges: seq<int64 * int64>) : seq<int64 * int64> =
    // assumes non-overlapping mapping sources, which i did not validate
    let rec m ((seedStart, seedLen): int64 * int64) (mappings: list<Mapping>) () : seq<int64 * int64> =
        seq {
            match mappings with
            | _ when seedLen <= 0 ->
                // printfn "-"
                ()
            | [] ->
                // printfn "- %A" (seedStart, seedLen)
                yield (seedStart, seedLen)
            | mapping :: rest ->
                let seedEndExclusive = seedStart + seedLen
                let mappingEndExclusive = mapping.source + mapping.length
                let startIntersection = min seedEndExclusive (max seedStart mapping.source)

                let endIntersectionExclusive =
                    min seedEndExclusive (max seedStart mappingEndExclusive)

                yield! (m (seedStart, startIntersection - seedStart) rest) ()

                if startIntersection >= mapping.source
                   && startIntersection < mapping.source + mapping.length
                   && startIntersection < endIntersectionExclusive then
                    let mappedRange =
                        (mapping.destination
                         + (startIntersection - mapping.source),
                         endIntersectionExclusive - startIntersection)

                    // printfn "  %A" mappedRange
                    yield mappedRange

                yield! (m (endIntersectionExclusive, seedEndExclusive - endIntersectionExclusive) rest) ()
            | _ -> failwith ("forgot something it seems")
        }

    seq {
        for range in seedRanges do
            yield! (m range (List.ofArray mappings)) ()
    }

for r in [| 1L, 9L; 1, 2; 1, 3; 6, 3; 10, 5 |] do
    printfn
        "mapping %A to %A"
        r
        (mapSeedRanges
            ([| { source = 3
                  destination = 13
                  length = 5 } |])
            (Seq.ofArray [| (fst r), snd r |]))

let alternativeDoMapping (seedString: string) (mapStrings: list<string>) =
    let seedsSpec =
        seedString.Split(" ", System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose Util.parseInt64

    let seeds =
        seq {
            for i in 0..2 .. seedsSpec.Length - 1 do
                let start = seedsSpec[i]
                let len = seedsSpec[i + 1]

                yield start, len
        }

    let maps = mapStrings |> Seq.map parseMap

    maps
    |> Seq.fold (fun (ranges: seq<int64 * int64>) mapping -> mapSeedRanges mapping ranges) seeds

let part2 (input: array<string>) =
    match (List.ofArray input) with
    | seeds :: maps -> alternativeDoMapping seeds maps
    | [] -> failwith "seeds not found"
    |> Seq.map fst
    |> Seq.min

printfn "part2 test: 46 = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
