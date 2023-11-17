open Library

let input = AoC.input 1

let parseNumbers (s: string) =
    s.Split("\n")
    |> Array.choose (fun x ->
        match System.Int32.TryParse x with
        | (true, n) -> Some n
        | _ -> None)

let elves = input.Split("\n\n") |> Array.map parseNumbers
printfn "%A" elves[..2]

printfn "top elf: %d" (elves |> Array.map Array.sum |> Array.max)

printfn
    "top 3 elves: %d"
    (elves
     |> Array.map Array.sum
     |> Array.sort
     |> Array.rev
     |> Array.take 3
     |> Array.sum)
