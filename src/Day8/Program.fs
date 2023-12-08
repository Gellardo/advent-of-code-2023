open Library
open System.Text.RegularExpressions

let real_input = AoC.input 8

let test_input =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

let toIndexedSeq (pattern: string) =
    seq {
        let instructions = Util.chars pattern

        while true do
            yield! (Seq.zip { 1 .. instructions.Length } instructions)
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
    | [| pattern: string; rules: string |] -> (pattern, (parseRules rules))
    | a -> failwith (sprintf "could not parse lines: %A" a)


let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine s = 1

let part1 (pattern, rules: Map<string, (string * string)>) =
    seq {
        let mutable state: string = "AAA"
        let mutable steps: int = 0

        for instruction in toIndexedSeq pattern do
            // printfn "%s %s %A" state instruction rules
            state <-
                match instruction with
                | _, "L" -> fst rules[state]
                | _, "R" -> snd rules[state]
                | _ -> failwith "unknown pattern"

            steps <- steps + 1
            if state = "ZZZ" then yield steps
    }
    |> Seq.head


// pattern |> Seq.fold (fun acc, instruction -> acc) "AAA"
// "AAA" |> Array.map parseLine |> Array.sum

printfn "part1 test: 6 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let part2 ((pattern: string), rules: Map<string, (string * string)>) =
    let startStates: list<string> =
        rules.Keys
        |> Seq.filter (fun state -> state.EndsWith "A")
        |> Seq.toList

    printfn "%d starting states: %A" startStates.Length startStates
    printfn "pattern length: %d" pattern.Length

    // Since it takes longer than reasonable it means I have to find some math property
    // This implies that there cycles that the startstates run through until they all align far in the future
    // (left it to run for 2.5m -> 132900000 steps)
    let findLoop (startState) =
        let mutable state = startState
        let mutable seen = Map.empty
        let mutable steps = 0

        seq {
            for (pos, instruction) in toIndexedSeq pattern do
                // printfn "%s %s %A" state instruction rules
                state <-
                    match instruction with
                    | "L" -> fst rules[state]
                    | "R" -> snd rules[state]
                    | _ -> failwith "unknown pattern"

                steps <- steps + 1

                if state.EndsWith "Z" then
                    if not (seen.ContainsKey(pos, state)) then
                        seen <- seen.Add((pos, state), steps)
                    else
                        yield steps, state, seen
        }
        |> Seq.head

    // from looking at the result, each start state only reaches one endstate and then loops
    // therefore: metaknowlege: seen only contains 1 element, all of them reached on the same step in the pattern
    startStates
    |> Seq.map (fun state ->
        let (endCycleSteps, endState, seen) = (findLoop state)
        let startCycleSteps = Seq.head seen.Values
        let cycleSteps = (endCycleSteps - startCycleSteps)

        printfn
            "%s->%s: first occurence %d steps = %d cycles, second occurence %d steps = %d cycles"
            state
            endState
            startCycleSteps
            (startCycleSteps / pattern.Length)
            cycleSteps
            (cycleSteps / pattern.Length)

        // more meta knowledge: it seems to be full cycles, so endstate is reached
        // - after pattern is fully consumed
        // - starts over because the second cycle is exactly as long as the first
        // Also cycle-length/patternlength are prime numbers, so the first time they intersect is the product of that
        // yeahy, I don't need to implement gcd/gcm for all the numbers as well
        int64 (cycleSteps / pattern.Length))
    |> Seq.reduce (*)
    |> fun patterncycles -> patterncycles * (int64 pattern.Length)

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
// based on the number of steps I waited, I might have gotten the correct answer after 140 days of waiting 🤯
printfn "part2: %d" (part2 real_lines)
