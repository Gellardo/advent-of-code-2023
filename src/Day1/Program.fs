open Library

let real_input = AoC.input 1

let extractNumber (s: string) =
    let numbers = Util.chars s |> Array.choose Util.parseInt

    numbers[0] * 10 + numbers[numbers.Length - 1]




let part1 (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map extractNumber
    |> Array.sum

printfn "part1: %d" (part1 real_input)

let massReplace replacements s =
    replacements
    |> Array.fold (fun (acc: string) (oldS: string, newS: string) -> acc.Replace(oldS, oldS + newS + oldS)) s


let part2 (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (
        massReplace [| ("one", "1")
                       ("two", "2")
                       ("three", "3")
                       ("four", "4")
                       ("five", "5")
                       ("six", "6")
                       ("seven", "7")
                       ("eight", "8")
                       ("nine", "9") |]
    )
    |> Array.map extractNumber
    // |> Array.map (fun s ->
    // printfn "%d" s
    // s)
    |> Array.sum

printfn
    "part2 test: 281 == %d"
    (part2
        """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
""")


printfn "part2: %d" (part2 real_input)
