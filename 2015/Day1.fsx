
let part1 (input:string) : int =
    (0, input)
    ||> Seq.fold (fun (counter:int) (c:char) ->
        match c with
        | '(' -> counter + 1
        | ')' -> counter - 1
        | _ -> failwith "wrong input"
    )

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
