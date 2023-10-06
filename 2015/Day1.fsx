
let part1 (input:string) : int =
    input
    |> Seq.map (function
        | '(' -> 1
        | ')' -> -1
        | _ -> failwith "wrong input"
    )
    |> Seq.sum

if fsi.CommandLineArgs.Length = 2 then
    let file = fsi.CommandLineArgs[1]
    System.IO.File.ReadAllText(file)
    |> part1
    |> printfn "%i"
else
    [
        "(())", 0
        "()()", 0
        "(((", 3
        "))(((((",3
        ")())())",-3
    ]
    |> List.iter (fun (input, output) ->
        if part1 input = output then
            printfn "yay"
        else
            printfn "nay"
    )
