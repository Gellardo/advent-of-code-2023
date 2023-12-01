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

let part2 (input: string) = 0
printfn "part2: %d" (part2 real_input)
