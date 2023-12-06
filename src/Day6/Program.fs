open Library
open System.Text.RegularExpressions

let real_input = AoC.input 6

let test_input =
    """Time:      7  15   30
Distance:  9  40  200
"""

let splitLines (input: string) =
    input.Split(
        "\n",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )

let real_lines = splitLines real_input
let test_lines = splitLines test_input

let parseLine (s: string) =
    s.Split(
        " ",
        System.StringSplitOptions.RemoveEmptyEntries
        ||| System.StringSplitOptions.TrimEntries
    )
    |> Array.choose Util.parseInt64

let solveEquation (time: int64, recordDistance: int64) : int64 =
    // (time - x) * x > record -> -x^2 + time*x - record > 0
    // for the question to make sense, the values for time and record have to be chosen so that
    // at least one int64eger solution exists, meaning the described parabola is upside down and
    // int64ersects the x-axis in 2 point64s. (Otherwise the solution would be ... * 0, which is boring)
    let betterThanRecord pick = (time - pick) * pick - recordDistance

    let left =
        ((double -time)
         + sqrt (double (time * time - 4L * recordDistance)))
        / (double -2)

    let right =
        ((double -time)
         - sqrt (double (time * time - 4L * recordDistance)))
        / (double -2)

    let leftFixed =
        if (betterThanRecord (int64 left)) <= 0 then
            (int64 left) + 1L
        else
            (int64 left)

    let rightFixed =
        if (betterThanRecord (int64 right)) <= 0 then
            (int64 right) - 1L
        else
            (int64 right)



    printfn
        "%d %d -> %f %f -> %d %d -> %d"
        time
        recordDistance
        left
        right
        leftFixed
        rightFixed
        (rightFixed - leftFixed + 1L)

    rightFixed - leftFixed + 1L


let part1 (input: array<string>) : int64 =
    input
    |> Array.map parseLine
    |> (fun lines -> Array.zip lines[0] lines[1])
    |> Array.map solveEquation
    |> Array.reduce (*)

printfn "part1 test: 288 == %d" (part1 test_lines)
printfn "part1: %d" (part1 real_lines)

let parseLine2 (s: string) =
    s
        .Replace(" ", "")
        .Split(
            ":",
            System.StringSplitOptions.RemoveEmptyEntries
            ||| System.StringSplitOptions.TrimEntries
        )
    |> Array.choose Util.parseInt64

let part2 (input: array<string>) =
    input
    |> Array.map parseLine2
    |> (fun lines -> Array.zip lines[0] lines[1])
    |> Array.map solveEquation
    |> Array.reduce (*)

printfn "part2 test: 71503 = %d" (part2 test_lines)
printfn "part2: %d" (part2 real_lines)
