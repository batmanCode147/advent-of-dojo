let parse = id

let part1 (input : seq<string>) =
    let vowels = [|'a'; 'e'; 'i'; 'o'; 'u'|]

    let notBad (bad:string) (input:string) = input.Contains(bad) |> not

    input
    |> Seq.filter (fun s ->
        let vowelCount =
            s
            |> Seq.filter (fun c -> vowels |> Array.contains c)
            |> Seq.length
        vowelCount >= 3
    )
    |> Seq.filter (
        Seq.pairwise
        >> Seq.exists (fun (first, second) -> first = second)
    )
    |> Seq.filter (notBad "ab")
    |> Seq.filter (notBad "cd")
    |> Seq.filter (notBad "pq")
    |> Seq.filter (notBad "xy")
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