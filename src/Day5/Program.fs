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
            | _ when seedLen <= 0 -> ()
            | [] -> yield (seedStart, seedLen)
            //  AAA     |     AAA
            //       BB | BBB
            | mapping :: rest when
                seedStart + seedLen <= mapping.source
                || mapping.source + mapping.length <= seedStart
                ->
                printfn "no overlap"
                yield! (m (seedStart, seedLen) rest) ()
            //  AAA
            //   B
            | mapping :: rest when
                seedStart >= mapping.source
                && seedStart + seedLen
                   <= mapping.source + mapping.length
                ->
                printfn "full overlap"
                let offset = seedStart - mapping.source
                yield mapping.destination + offset, seedLen
            //  AAA
            //   BBB
            | mapping :: rest when
                seedStart >= mapping.source
                && seedStart < mapping.source + mapping.length
                ->
                printfn "right partial overlap"
                let offset = seedStart - mapping.source
                let overlap = mapping.length - offset
                yield mapping.destination + offset, overlap
                yield! (m (mapping.source + mapping.length, seedLen - overlap) rest) ()
            //  AAA
            // BBB
            | mapping :: rest when
                seedStart < mapping.source
                && seedStart + seedLen >= mapping.source
                ->
                printfn "left partial overlap"
                let overlap = seedLen - (mapping.source - seedStart)
                yield! (m (seedStart, seedLen - overlap) rest) ()
                yield mapping.destination, overlap
            //  A
            // BBB
            | mapping :: rest when
                seedStart < mapping.source
                && seedStart + seedLen
                   >= mapping.source + mapping.length
                ->
                printfn "middle overlap"
                let offset = mapping.source - seedStart
                yield! (m (seedStart, offset) rest) ()
                yield mapping.destination, mapping.length
                yield! (m (mapping.source + mapping.length, seedLen - offset - mapping.length) rest) ()
            | _ -> failwith ("forgot something it seems")
        }

    seq {
        for range in seedRanges do
            printfn "Mapping %A" range
            yield! (m range (List.ofArray mappings)) ()
    }

// match remaining with
// | current :: rest ->
//     for map in mappings do
//         for range in remaining do
//             mapped = range :: mapped

//     List.append remaining

//     seedRanges
// mappings
// |> Array.choose (fun mapping ->
//     match mapping with
//     | { source = source
//         destination = destination
//         length = length } when 0L <= seed - source && seed - source < length -> Some(destination + seed - source)
//     | _ -> None)

// match mappedSeed with
// | [| result |] -> seedRanges
// | [||] -> seedRanges
// | _ -> failwith ("multiple mapping options")

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
    |> Seq.fold
        (fun (ranges: seq<int64 * int64>) mapping ->
            printfn "Start mapping"
            mapSeedRanges mapping ranges)
        seeds

let part2 (input: array<string>) =
    match (List.ofArray input) with
    | seeds :: maps -> alternativeDoMapping seeds maps
    | [] -> failwith "seeds not found"
    |> Seq.map fst
    |> Seq.min

printfn "part2 test: 46 = %d" (part2 test_lines)
// printfn "part2: %d" (part2 real_lines)
