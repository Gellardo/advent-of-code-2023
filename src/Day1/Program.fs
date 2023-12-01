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

let part2 (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Replace("one", "one1one"))
    |> Array.map (fun s -> s.Replace("two", "two2two"))
    |> Array.map (fun s -> s.Replace("three", "three3three"))
    |> Array.map (fun s -> s.Replace("four", "four4four"))
    |> Array.map (fun s -> s.Replace("five", "five5five"))
    |> Array.map (fun s -> s.Replace("six", "six6six"))
    |> Array.map (fun s -> s.Replace("seven", "seven7seven"))
    |> Array.map (fun s -> s.Replace("eight", "eight8eight"))
    |> Array.map (fun s -> s.Replace("nine", "nine9nine"))
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
