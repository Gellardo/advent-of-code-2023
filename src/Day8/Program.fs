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
        let matches =
            Regex.Matches(line, "([A-Z0-9]{3}) = \(([A-Z0-9]{3}), ([A-Z0-9]{3})\)")

        let from = matches[0].Groups[1]
        let left = matches[0].Groups[2]
        let right = matches[0].Groups[3]
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
let test_lines = splitLines test_input

let parseLine s = 1

let part1 (pattern, rules: Map<string, (string * string)>) =
    seq {
        let mutable state: string = "AAA"
        let mutable steps: int = 0

        for instruction in pattern do
            // printfn "%s %s %A" state instruction rules
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

let part2 (pattern, rules: Map<string, (string * string)>) =
    // input |> Array.map parseLine |> Array.sum
    seq {
        let mutable states: list<string> =
            rules.Keys
            |> Seq.filter (fun state -> state.EndsWith "A")
            |> Seq.toList

        printfn "%d starting states: %A" states.Length states

        let mutable steps: int = 0

        for instruction in pattern do
            // printfn "%s %s %A" state instruction rules
            states <-
                states
                |> List.map (fun state ->
                    match instruction with
                    | "L" -> fst rules[state]
                    | "R" -> snd rules[state]
                    | _ -> failwith "unknown pattern")

            steps <- steps + 1

            if states
               |> List.forall (fun state -> state.EndsWith "Z") then
                yield steps

            let currentZs =
                states
                |> List.map (fun state -> if state.EndsWith "Z" then 1 else 0)
                |> Seq.sum

            // if currentZs > 0 then
            // printfn "%d - current Zs: %d" steps currentZs

            if steps % 100000 = 0 then
                printfn "took %d steps" steps
    }
    |> Seq.head

let test_lines_new =
    splitLines
        """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

printfn "part2 test: 6 = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
