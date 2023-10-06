type Judgment =
    | Naughty
    | Nice 

let parse = id

let part1 (input : seq<string>) =
    let badPatterns = ["ab";"cd";"pq";"xy"]

    input 
    |> Seq.filter ( fun s ->
        let has3vowels =
            (
                s
                |> Seq.filter (function
                    | 'a' | 'e' | 'i' | 'o' | 'u' -> true
                    | _ -> false
                )
                |> Seq.length
            ) >= 3
        let hasDoubleChar = 
            s 
            |> Seq.pairwise
            |> Seq.exists (fun (first , second) ->
                first = second
            )
        
        let hasBadPattern = 
            badPatterns
            |> Seq.exists (fun badS -> s.Contains(badS))
        has3vowels && hasDoubleChar && not hasBadPattern
    )
    |> Seq.length

let part2 (input : seq<string>) =
    0

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllLines(file)
    |> parse
    |> part1
    |> printfn "%i"
else
    [
        ["ugknbfddgicrmopn"], 1
        ["aaa"], 1
        ["jchzalrnumimnmhp"], 0
        ["haegwjzuvuyypxyu"], 0
        ["dvszwmarrgswjxmb"], 0
    ]
    |> Seq.map (fun (input, output) -> parse input, output)
    |> Seq.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )