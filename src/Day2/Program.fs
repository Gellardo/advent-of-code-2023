open Library
let real_input = AoC.input 2

let test_input =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

let limits =
    Map [ "red", 12
          "green", 13
          "blue", 14 ]


let processLines (s: string) =
    let parts = s.Split(":")

    let id =
        parts[ 0 ].Split(" ")[1]
        |> Util.parseInt
        |> Option.get

    let turns =
        parts[ 1 ].Split(";")
        |> Array.map (fun turn ->
            turn.Split(",", System.StringSplitOptions.TrimEntries)
            |> Array.fold
                (fun acc (cubes: string) ->
                    let (num, color) = cubes.Split(" ") |> (fun a -> (a[0], a[1]))
                    Map.add color (Util.parseInt num |> Option.get) acc)
                (Map []))

    id, turns

printfn "test lines: %A" (processLines "game 1: 1 red, 1 green;2 blue, 3 white")

type Game = int * array<Map<string, int>>

let checkLimits (game: Game) =
    snd game
    |> Array.forall (fun turn ->
        turn
        |> Map.filter (fun color value -> limits[color] < value)
        |> Map.isEmpty)

printfn
    "test lines: %A"
    (processLines "game 1: 1 red, 1 green;2 blue, 3 green"
     |> checkLimits)

let part1 (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map processLines
    |> Array.filter checkLimits
    |> Array.map fst
    |> Array.sum

printfn "part1 test: 5+3 = 8 == %d" (part1 test_input)
printfn "part1: %d" (part1 real_input)

let power (game: Game) = 0


let part2 (input: string) =
    input.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map processLines
    |> Array.map power
    |> Array.sum

printfn "part2 test: 2286 = %d" (part2 test_input)


printfn "part2: %d" (part2 real_input)
