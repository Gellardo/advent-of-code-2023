open Library
let real_input = AoC.input 2

let test_input = """here"""

let splitLines (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)

let real_lines = splitLines real_input
let test_lines = splitLines test_input


let part1 (input: string) = 0

printfn "part1 test: XX == %d" (part1 test_input)
printfn "part1: %d" (part1 real_input)

let part2 (input: string) = 0

printfn "part2 test: YY = %d" (part2 test_input)


printfn "part2: %d" (part2 real_input)
